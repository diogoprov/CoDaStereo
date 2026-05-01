# ==============================================================================
# Main Server — wires module reactives together
# ==============================================================================

#' @noRd
app_server <- function(input, output, session) {

  # -- Guided tour -----------------------------------------------------------
  # init() sends the tour config via WebSocket. It must be called inside
  # observeEvent (after the browser is connected), NOT at server startup.
  mod_tutorial_server("tutorial")
  tour <- build_tour()

  observeEvent(input$tour_btn_header, {
    tour$init()$start()
  }, ignoreInit = TRUE)

  observeEvent(input$tour_btn_intro, {
    tour$init()$start()
  }, ignoreInit = TRUE)

  # -- Module: Data Upload + Inspection --------------------------------------
  data_r <- mod_data_server("data")

  # -- Module: Zero/NA Imputation --------------------------------------------
  impute_r <- mod_imputation_server("imputation",
                                     df    = data_r$df,
                                     parts = data_r$parts)

  # -- Module: Compositional Analysis (ternary, biplot, descriptives) --------
  mod_analysis_server("analysis",
                      df        = data_r$df,
                      parts     = data_r$parts,
                      comp      = impute_r$comp,
                      keep_rows = impute_r$keep_rows)

  # -- Module: Statistical Modeling (MANOVA, pairwise, diagnostics) ----------
  mod_stats_server("stats",
                   df        = data_r$df,
                   parts     = data_r$parts,
                   comp      = impute_r$comp,
                   keep_rows = impute_r$keep_rows)
}
