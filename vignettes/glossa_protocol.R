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
global_land_mask <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  sf::st_as_sfc() %>%
  sf::st_union() %>%
  sf::st_make_valid() %>%
  sf::st_wrap_dateline()

## -----------------------------------------------------------------------------
# Load presence(/absence) data
pa_files <- c("../inst/extdata/sp1.csv", "../inst/extdata/sp2.csv")
presence_absence_list <- list()
presence_absence_list$raw_pa <- lapply(pa_files, glossa:::load_presence_absence_data)


historical_files <- "../inst/extdata/historical_layers.zip"
future_files <- c("../inst/extdata/future1_layers.zip", "../inst/extdata/future2_layers.zip")

## ----echo = FALSE-------------------------------------------------------------
rbind(
  read.table(pa_files[1], sep = "\t", dec = ".", header = TRUE),
  read.table(pa_files[2], sep = "\t", dec = ".", header = TRUE)
) %>% 
  ggplot(data = .) +
  geom_sf(data = global_land_mask, color = "#353839", fill = "antiquewhite") +
  geom_point(aes(x = decimalLongitude, y = decimalLatitude, color = species)) +
  scale_color_manual(values = c("#F6733A", "#4FA3AB"), name = NULL) +
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

