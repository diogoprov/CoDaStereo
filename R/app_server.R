# ==============================================================================
# Main Server — wires module reactives together
# ==============================================================================

#' @noRd
app_server <- function(input, output, session) {

  # Module: Tutorial (guided tour)
  mod_tutorial_server("tutorial")

  # Module: Data Upload + Inspection
  data_r <- mod_data_server("data")

  # Module: Zero/NA Imputation
  impute_r <- mod_imputation_server("imputation",
                                     df    = data_r$df,
                                     parts = data_r$parts)

  # Module: Compositional Analysis (ternary, biplot, descriptives, downloads)
  mod_analysis_server("analysis",
                      df        = data_r$df,
                      parts     = data_r$parts,
                      comp      = impute_r$comp,
                      keep_rows = impute_r$keep_rows)

  # Module: Statistical Modeling (MANOVA, pairwise, diagnostics)
  mod_stats_server("stats",
                   df        = data_r$df,
                   parts     = data_r$parts,
                   comp      = impute_r$comp,
                   keep_rows = impute_r$keep_rows)
}
