# ==============================================================================
# Main Server — wires module reactives together
# ==============================================================================

#' @noRd
app_server <- function(input, output, session) {

  # -- Guided tour (rintrojs) ------------------------------------------------
  mod_tutorial_server("tutorial")

  observeEvent(input$tour_btn_header, {
    rintrojs::introjs(session,
                      options = list(steps        = build_tour_steps(),
                                     showBullets  = FALSE,
                                     showProgress = TRUE,
                                     nextLabel    = "Next &rarr;",
                                     prevLabel    = "&larr; Back",
                                     doneLabel    = "Done"))
  }, ignoreInit = TRUE)

  observeEvent(input$tour_btn_intro, {
    rintrojs::introjs(session,
                      options = list(steps        = build_tour_steps(),
                                     showBullets  = FALSE,
                                     showProgress = TRUE,
                                     nextLabel    = "Next &rarr;",
                                     prevLabel    = "&larr; Back",
                                     doneLabel    = "Done"))
  }, ignoreInit = TRUE)

  # -- Module: Data Upload + Inspection --------------------------------------
  data_r <- mod_data_server("data")

  # -- Module: Zero/NA Imputation --------------------------------------------
  impute_r <- mod_imputation_server("imputation",
                                     df    = data_r$df,
                                     parts = data_r$parts)

  # -- Module: Compositional Analysis ----------------------------------------
  mod_analysis_server("analysis",
                      df        = data_r$df,
                      parts     = data_r$parts,
                      comp      = impute_r$comp,
                      keep_rows = impute_r$keep_rows)

  # -- Module: Statistical Modeling ------------------------------------------
  mod_stats_server("stats",
                   df        = data_r$df,
                   parts     = data_r$parts,
                   comp      = impute_r$comp,
                   keep_rows = impute_r$keep_rows)
}
