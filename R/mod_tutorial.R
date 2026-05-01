# ==============================================================================
# Module: Interactive Tutorial (cicerone guided tour)
# ==============================================================================

#' Build the PPDAC guided tour using cicerone
#' @noRd
build_tour <- function() {
  cicerone::Cicerone$new(id = "ppdac_tour")$
    step(
      el        = "sidebarMenu",
      title     = "Welcome to CoDa Stereo!",
      description = paste(
        "This application implements the Aitchison geometry for",
        "stereological and histomorphometric data.<br><br>",
        "Follow the sidebar tabs <b>top to bottom</b> to complete the",
        "<b>PPDAC cycle</b> (Problem, Plan, Data, Analysis, Conclusion).<br><br>",
        "Click <b>Next</b> to learn about each step."
      ),
      position = "right"
    )$
    step(
      el        = "data-upload_ui",
      title     = "1. Data Upload",
      description = paste(
        "Upload your CSV or Excel file here.<br><br>",
        "<b>Important:</b> After uploading, select which numeric columns",
        "are the <b>compositional parts</b> (volume fractions, area",
        "percentages, or cell counts that sum to a constant).<br><br>",
        "Columns that look like IDs (e.g., Sample_ID) are deselected",
        "automatically, but check that non-compositional covariates",
        "(body weight, biochemical assays) are also unchecked."
      ),
      position = "right"
    )$
    step(
      el        = "data-inspect_ui",
      title     = "2. Data Inspection",
      description = paste(
        "Before any analysis, inspect your data for:<br>",
        "<ul>",
        "<li><b>Structural zeros</b> (rare tissue compartments)</li>",
        "<li><b>Missing values (NA)</b></li>",
        "</ul>",
        "The heatmap distinguishes observed values, zeros, and NAs.",
        "The naniar plot shows the overall missingness pattern."
      ),
      position = "right"
    )$
    step(
      el        = "imputation-run",
      title     = "3. Zero & NA Handling",
      description = paste(
        "Choose the appropriate imputation method:<br><br>",
        "<b>CZM</b> for count zeros (point counts).<br>",
        "<b>multRepl</b> for rounded zeros (percentages).<br>",
        "<b>lrEM</b> for rounded zeros with larger samples.<br><br>",
        "If your data has NAs, decide whether to drop those rows",
        "or impute them with the column geometric mean.<br><br>",
        "Click <b>Run Imputation</b> when ready."
      ),
      position = "right"
    )$
    step(
      el        = "analysis-ternary_plot",
      title     = "4a. Ternary Plot",
      description = paste(
        "Visualise 3 parts inside the Aitchison simplex.<br><br>",
        "If your composition has more than 3 parts, use",
        "<b>Amalgamate into 3 groups</b> to define sub-compositions",
        "(e.g., Parenchyma, Stroma, Inflammatory).<br><br>",
        "This plot uses the <b>raw data</b> (pre-imputation) for",
        "exploratory purposes."
      ),
      position = "left"
    )$
    step(
      el        = "analysis-biplot",
      title     = "4b. clr-PCA Biplot",
      description = paste(
        "The Compositional Ray Biplot projects the <b>clr-transformed</b>",
        "data onto the first two principal components.<br><br>",
        "Arrow length indicates the importance of each part.<br>",
        "Arrow direction shows correlations between parts.<br>",
        "Convex hulls delimit the morphological space per group.<br><br>",
        "This plot uses the <b>imputed data</b>."
      ),
      position = "left"
    )$
    step(
      el        = "stats-run_manova",
      title     = "5. Statistical Modeling",
      description = paste(
        "Fit a permutational MANOVA on ilr coordinates via RRPP.<br><br>",
        "Select one or more factors. With 2+ factors, choose between",
        "additive or factorial designs.<br><br>",
        "After fitting, run <b>Pairwise comparisons</b> and inspect",
        "the <b>Residual diagnostics</b>.<br><br>",
        "All results are downloadable as CSV or PDF."
      ),
      position = "left"
    )$
    step(
      el        = "sidebarMenu",
      title     = "You're ready!",
      description = paste(
        "That's the full PPDAC cycle.<br><br>",
        "Start by uploading your data or loading the built-in example.",
        "You can restart this tour anytime by clicking the",
        "<b>\U0001f393 Tour</b> button in the header."
      ),
      position = "right"
    )
}

#' @noRd
mod_tutorial_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    tour <- build_tour()
    
    observeEvent(input$start_tour, {
      tour$init()$start()
    })
  })
}
