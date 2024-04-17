#=========================================================#
# Load packages ----
#=========================================================#
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(waiter)

library(ggplot2)
library(sparkline)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)
library(tidyterra)
library(dplyr)
library(mcp)
library(DT)
library(svglite)
library(dbarts)

library(glossa)
#for (i in list.files("../../R/", full.names = TRUE)){
#  source(i)
#}

#=========================================================#
# Data preparation ----
#=========================================================#
sf::sf_use_s2(FALSE)
global_land_mask <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  sf::st_as_sfc() %>%
  sf::st_union() %>%
  sf::st_make_valid() %>%
  sf::st_wrap_dateline()
global_ocean_mask <- invert_polygon(global_land_mask)
