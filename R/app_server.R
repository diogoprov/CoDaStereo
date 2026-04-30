# ==============================================================================
# Main Server — wires module reactives together
# ==============================================================================

#' @noRd
app_server <- function(input, output, session) {

  # Module 1: Data Upload + Inspection
  # Returns: $df (reactive data.frame), $parts (reactive character vector)
  data_r <- mod_data_server("data")

  # Module 2: Zero/NA Imputation
  # Receives: df, parts from data module
  # Returns: $comp, $imputed, $keep_rows, $method_used
  impute_r <- mod_imputation_server("imputation",
                                     df    = data_r$df,
                                     parts = data_r$parts)

  # Module 3: Compositional Analysis (ternary, biplot, downloads)
  # Receives: df, parts, comp, keep_rows
  mod_analysis_server("analysis",
                      df        = data_r$df,
                      parts     = data_r$parts,
                      comp      = impute_r$comp,
                      keep_rows = impute_r$keep_rows)

  # Module 4: Statistical Modeling (MANOVA, pairwise, diagnostics)
  # Receives: df, parts, comp, keep_rows
  mod_stats_server("stats",
                   df        = data_r$df,
                   parts     = data_r$parts,
                   comp      = impute_r$comp,
                   keep_rows = impute_r$keep_rows)
}
