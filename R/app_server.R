# ==============================================================================
# Main Server — wires module reactives together
# ==============================================================================

#' @noRd
app_server <- function(input, output, session) {
  session$userData$lang <- lang_from_query(isolate(session$clientData$url_search))


  # -- Guided tour (rintrojs) ------------------------------------------------
  mod_tutorial_server("tutorial")

  start_tour <- function() {
    t <- session_tr(session)
    rintrojs::introjs(session,
                      options = list(steps        = build_tour_steps(ui_lang(session)),
                                     showBullets  = FALSE,
                                     showProgress = TRUE,
                                     nextLabel    = paste(t("Next"), "&rarr;"),
                                     prevLabel    = paste("&larr;", t("Back")),
                                     doneLabel    = t("Done")))
  }
  observeEvent(input$tour_btn_header, start_tour(), ignoreInit = TRUE)
  observeEvent(input$tour_btn_intro, start_tour(), ignoreInit = TRUE)

  # -- Session restore (set by the Reproducibility tab) -----------------------
  restore <- reactiveVal(NULL)

  # -- Module: Data Upload + Inspection --------------------------------------
  data_r <- mod_data_server("data", restore = restore)

  # -- Module: Zero/NA Imputation --------------------------------------------
  impute_r <- mod_imputation_server("imputation",
                                     df      = data_r$df,
                                     parts   = data_r$parts,
                                     restore = restore)

  # -- Module: Compositional Analysis ----------------------------------------
  analysis_r <- mod_analysis_server("analysis",
                                   df        = data_r$df,
                                   parts     = data_r$parts,
                                   comp      = impute_r$comp,
                                   keep_rows = impute_r$keep_rows,
                                   raw       = impute_r$raw,
                                   restore   = restore)

  # -- Module: Balances (SBP + CoDa dendrogram) ------------------------------
  balances_r <- mod_balances_server("balances",
                                   df        = data_r$df,
                                   parts     = data_r$parts,
                                   comp      = impute_r$comp,
                                   keep_rows = impute_r$keep_rows,
                                   restore   = restore)

  # -- Module: Statistical Modeling ------------------------------------------
  stats_r <- mod_stats_server("stats",
                              df        = data_r$df,
                              parts     = data_r$parts,
                              comp      = impute_r$comp,
                              keep_rows = impute_r$keep_rows,
                              raw       = impute_r$raw,
                              restore   = restore)

  # -- Module: Reproducibility -----------------------------------------------
  # Steps not run yet (no data, no imputation...) are recorded as NULL
  safely <- function(r) tryCatch(r(), error = function(e) NULL)
  session_state <- reactive({
    req(safely(data_r$state))
    list(
      kind       = "CoDaStereo-session",
      version    = app_version(),
      data       = data_r$state(),
      imputation = safely(impute_r$state),
      analysis   = safely(analysis_r$inputs),
      balances   = list(sbp = safely(balances_r$sbp), group_var = safely(balances_r$group_var),
                        palette = safely(balances_r$palette)),
      stats      = safely(stats_r$state)
    )
  })
  mod_export_server("export", state = session_state, restore = restore)
}
