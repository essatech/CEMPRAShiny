# ------------------------------------------------------------------------
# Shiny Global Imports
# This is the Shiny App global script
# This f1ile is sourced and run once when the app first loads
# See tutorial here: https://shiny.rstudio.com/articles/scoping.html
# ------------------------------------------------------------------------

# Clear all existing objects from memory
rm(list = ls())

# Load necessary libraries
# devtools::install_github("essatech/CEMPRA", force = TRUE)
library(CEMPRA) # Download instructions: https://github.com/essatech/CEMPRA/

# Load other libraries from CRAN
library(dplyr)
library(readxl)
library(writexl)
library(shiny)
library(DT)
library(shinyjs)
library(shinyFeedback)
library(shinydashboard)
library(shinydashboardPlus)
library(shinybusy)
library(waiter)
library(shinyWidgets)
library(htmlwidgets)
library(dygraphs)
library(sf)
library(leaflet)
library(tidyr)
library(reshape2)
library(popbio)
library(testthat)
library(ggplot2)
library(shinyvalidate)
library(ggthemes)
library(plotly)
library(rjson)

source("./R/utility_population_dat_clean.R")

# Set options
options(
  spinner.color = "#ffffff",
  spinner.color.background = "#0073b7",
  spinner.size = 3,
  shiny.maxRequestSize = 32 * 1024 ^ 2 # Increase file upload size limit to 32MB
)

# TODO: Remove for deployment - pause on error
# options(shiny.error = browser)

# Load stressor-response relationships
# file_name_stressor_response <- "./data/stressor_response_demo.xlsx"
 file_name_stressor_response <- "./data/stressor_response_smd.xlsx"
# file_name_stressor_response <- "./data/ns_ss_sr.xlsx"

sr_wb_dat <- CEMPRA::StressorResponseWorkbook(filename = file_name_stressor_response)

# Record start time
start_time <- Sys.time()

# Load stressor magnitude values associated with each HUC
# file_name_stressor_magnitude <- "./data/stressor_magnitude_demo.xlsx"
 file_name_stressor_magnitude <- "./data/stressor_magnitude_smd.xlsx"
#file_name_stressor_magnitude <- "./data/nd_ss_sm.xlsx"

sm_wb_dat <- CEMPRA::StressorMagnitudeWorkbook(filename = file_name_stressor_magnitude, scenario_worksheet = 1)

# Load life stages for the population model from CSV file
life_stages <- read.csv("./data/chinook_life_cycle_profile.csv")
# life_stages <- read.csv("./data/TEST.csv")
#life_stages <- read.csv("./data/life cycles.csv")

life_stages <- utility_population_dat_clean(dat = life_stages, nstage_fill = 10)

# Load and process map geometry and map object reactive values
# hmdl <- sf::st_read("./data/watersheds.gpkg")
hmdl <- sf::st_read("./data/smd_locations.gpkg")
# hmdl <- sf::st_read("./data/nd_ss_loc2.gpkg")

hmdl$HUC_ID <- as.numeric(hmdl$HUC_ID)
hmdl$uid <- paste0(hmdl$HUC_ID, "|", hmdl$NAME)

# Determine initial variable for display
first_var <- sort(sr_wb_dat$stressor_names)[1]

# Define layer bounds for initial map load
bbox <- st_bbox(hmdl)
bbox_global <- bbox

# Configure color function for System Capacity Choropleth Map
color_func <- colorQuantile(c("#f22300", "#e0af00", "#ebcc2a", "#79b7c5", "#3b9ab2"), domain = c(0, 100), na.color = "lightgrey", n = 8)

# Generate legend
leg_col <- lapply(c(0, 20, 40, 60, 80, 100), color_func) %>% unlist()
leg_lab <- c(0, 20, 40, 60, 80, 100)

# Joe Model initial default settings and result holder
MC.sims <- 50
read.dose <- TRUE

# Deployment reminders (remove before deploying)
# - Turn off reactlog::reactlog_enable()
# - Turn on preloader in ui.R
# - Load package functions into app
# - Run shinytest2::record_test() and shinytest2::test_app()

# Launch the Shiny app
# shiny::runApp(launch.browser = TRUE)