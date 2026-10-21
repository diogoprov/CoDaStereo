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

# Only the UI packages are attached; everything else is called with pkg::
# (as in the package), which keeps start-up short.
library(shiny)
library(shinydashboard)
.data  <- rlang::.data   # tidy-eval pronoun used in ggplot2 aesthetics
`%||%` <- rlang::`%||%`

# Source package code: every file in R/ (helpers, modules, app)
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f, local = TRUE)

# Launch (start_workers() runs model fits in background mirai workers)
shinyApp(ui = app_ui, server = app_server, onStart = start_workers)
