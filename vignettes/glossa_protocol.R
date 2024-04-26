## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  error = FALSE
)

## -----------------------------------------------------------------------------
# Load libraries
library(glossa)
library(dplyr)
library(ggplot2)

# Load world map
sf::sf_use_s2(FALSE)
study_area_poly <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  sf::st_as_sfc() %>%
  sf::st_union() %>%
  sf::st_make_valid() %>%
  sf::st_wrap_dateline() %>% 
  glossa::invert_polygon(bbox = c(xmin = -180, ymin =-90, xmax = 180, ymax = 90))

non_study_area_poly <- glossa::invert_polygon(study_area_poly)

## -----------------------------------------------------------------------------
# Load presence(/absence) data
pa_file <- "../inst/extdata/sp2.csv"
raw_pa <- glossa::read_glossa_pa(pa_file)

# Load historical layers
historical_files <- "../inst/extdata/historical_layers.zip"
past_layers <- glossa::read_glossa_layers(historical_files)

# Load future layers
future_files <- "../inst/extdata/future1_layers.zip"
future_layers <- glossa::read_glossa_layers(future_files)

## ----echo = FALSE-------------------------------------------------------------
ggplot() +
  geom_sf(data = non_study_area_poly, color = "#353839", fill = "antiquewhite") +
  geom_point(data = raw_pa, aes(x = decimalLongitude, y = decimalLatitude, color = "#F6733A")) +
  theme(
    panel.grid.major = element_line(
      color = gray(.5),
      linetype = "dashed",
      linewidth = 0.5
    ),
    panel.background = element_rect(fill = "white"),
    axis.title = element_blank(),
    legend.position = "bottom"
  )

## -----------------------------------------------------------------------------
coords <- c("decimalLongitude", "decimalLatitude")

# Remove NA coordinates
clean_pa <- raw_pa[complete.cases(raw_pa[, coords]), ]

# Round coordinates to 4 decimal digits (this is up to your choice and the precision you need)
decimal_digits <- 4
clean_pa[, coords[1]] <- round(clean_pa[, coords[1]], decimal_digits)
clean_pa[, coords[2]] <- round(clean_pa[, coords[2]], decimal_digits)

# Remove duplicated points
clean_pa <- glossa::remove_duplicate_points(clean_pa, coords = coords)

# Remove points outside the ocean boundaries (overlapping land)
clean_pa <- glossa::remove_points_poly(clean_pa,
                                       sf_poly = study_area_poly,
                                       overlapping = FALSE,
                                       coords = coords)

## -----------------------------------------------------------------------------
clean_pa <- glossa::clean_coordinates(
  data = raw_pa,
  sf_poly = study_area_poly,
  overlapping = FALSE,
  decimal_digits = decimal_digits,
  coords = coords
)

## -----------------------------------------------------------------------------
past_layers <- lapply(past_layers, function(x){
  glossa::layer_mask(layers = x, sf_poly = study_area_poly)
})

future_layers <- lapply(future_layers, function(x){
  glossa::layer_mask(layers = x, sf_poly = study_area_poly)
})

## -----------------------------------------------------------------------------
# Compute mean and sd of the historical time series for each environmental variable
past_mean <- lapply(past_layers, function(x){
  mean(as.vector(x), na.rm = TRUE)
})

past_sd <- lapply(past_layers, function(x){
  sd(as.vector(x), na.rm = TRUE)
})

# Scale layers with past mean and sd
past_layers <- lapply(seq_along(past_layers), function(x, n, i){
  terra::scale(x[[n[i]]], center = past_mean[[n[i]]], scale = past_sd[[n[i]]])},
  x = past_layers,
  n = names(past_layers))
names(past_layers) <- names(past_mean)

future_layers <- lapply(seq_along(future_layers), function(x, n, i){
  terra::scale(x[[n[i]]], center = past_mean[[n[i]]], scale = past_sd[[n[i]]])},
  x = future_layers,
  n = names(future_layers))
names(future_layers) <- names(past_mean)

## -----------------------------------------------------------------------------
# Get mean of scaled past layers for model fitting
historical_layers <- lapply(past_layers, function(x){
  terra::mean(x, na.rm = TRUE)
})
historical_layers <- terra::rast(historical_layers)

## -----------------------------------------------------------------------------
y_resp <- cbind(clean_pa,
                  terra::extract(historical_layers, clean_pa[, coords])) %>%
  tidyr::drop_na()  %>%
  dplyr::select(colnames(clean_pa))

## -----------------------------------------------------------------------------
# Generate balanced random pseudoabsences
if (all(y_resp[, "pa"] == 1)){
  set.seed(1234)
  y_resp <- glossa::generate_pseudo_absences(y_resp, study_area_poly, historical_layers)
}

## -----------------------------------------------------------------------------
coords_layer <- glossa::create_coords_layer(historical_layers, study_area_poly, scale_layers = TRUE)

## -----------------------------------------------------------------------------
model_native_range <- fit_bart_model(
  y_resp,
  c(historical_layers, coords_layer),
  seed = 1234
)

model_suitable_habitat <- fit_bart_model(
  y_resp,
  historical_layers,
  seed = 1234
)

## -----------------------------------------------------------------------------
historical_native_range <- glossa::predict_bart(model_suitable_habitat, c(historical_layers, coords_layer))
historical_suitable_habitat <- glossa::predict_bart(model_suitable_habitat, historical_layers)

## -----------------------------------------------------------------------------
# Past prediction
past_suitable_habitat <- lapply(1:terra::nlyr(past_layers[[1]]), function(i){
  # Stack covariates by year
  pred_layers <- lapply(past_layers, function(y){
    return(y[[i]])
  })
  pred_layers <- terra::rast(pred_layers)
  
  glossa::predict_bart(model_suitable_habitat, pred_layers)
})

future_suitable_habitat <- lapply(1:terra::nlyr(future_layers[[1]]), function(i){
  # Stack covariates by year
  pred_layers <- lapply(future_layers, function(y){
    return(y[[i]])
  })
  pred_layers <- terra::rast(pred_layers)
  
  glossa::predict_bart(model_suitable_habitat, pred_layers)
})

