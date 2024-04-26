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

#library(glossa)
#for (i in list.files("../../R/", full.names = TRUE)){
#  source(i)
#}

#=========================================================#
# Data preparation ----
#=========================================================#
# Load world map
sf::sf_use_s2(FALSE)
