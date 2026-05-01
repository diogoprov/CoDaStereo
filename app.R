# ==============================================================================
# CoDa Stereo — Entry point for Posit Connect Cloud
#
# Connect Cloud looks for an app.R at the repository root. This file sources
# the package modules directly so that the app runs without needing to install
# the CoDaStereo package first.
#
# For local development as a package, use:
#   pkgload::load_all(); run_app()
# ==============================================================================

# Load dependencies
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(scales)           # percent_format() for descriptives
library(readxl)
library(naniar)
library(compositions)
library(zCompositions)
library(RRPP)
library(factoextra)
library(FactoMineR)
library(cicerone)         # guided tour

# Source package code (order matters: helpers first, then modules, then app)
source("R/fct_helpers.R",     local = TRUE)
source("R/mod_tutorial.R",    local = TRUE)
source("R/mod_data.R",        local = TRUE)
source("R/mod_imputation.R",  local = TRUE)
source("R/mod_analysis.R",    local = TRUE)
source("R/mod_stats.R",       local = TRUE)
source("R/app_ui.R",          local = TRUE)
source("R/app_server.R",      local = TRUE)

# Launch
shinyApp(ui = app_ui, server = app_server)
