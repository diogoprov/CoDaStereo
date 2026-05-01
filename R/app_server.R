# ==============================================================================
# Main Server — wires module reactives together
# ==============================================================================

#' @noRd
app_server <- function(input, output, session) {

  # -- Guided tour (wired here to avoid namespace mismatch: both buttons are
  #    defined in the main UI, not inside a module's UI function) -------------
  mod_tutorial_server("tutorial")  # kept for golem compatibility
  tour <- build_tour()
  tour$init()

  observeEvent(input$tour_btn_header, { tour$start() }, ignoreInit = TRUE)
  observeEvent(input$tour_btn_intro,  { tour$start() }, ignoreInit = TRUE)

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
