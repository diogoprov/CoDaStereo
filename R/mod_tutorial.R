# ==============================================================================
# Module: Interactive Tutorial (cicerone guided tour)
# ==============================================================================

# Element IDs used in steps must match the actual HTML element IDs rendered by
# Shiny / shinydashboard. Verified IDs:
#   "sidebarMenu"       -> sidebarMenu(id = "sidebarMenu") in app_ui.R
#   "imputation-run"    -> actionButton(ns("run")) in mod_imputation.R
#   "data-parts"        -> selectInput(ns("parts")) in mod_data.R
#   "analysis-ternary_plot" -> plotOutput(ns("ternary_plot")) in mod_analysis.R
#   "analysis-biplot"   -> plotOutput(ns("biplot")) in mod_analysis.R
#   "stats-run_manova"  -> actionButton(ns("run_manova")) in mod_stats.R
#
# shinydashboard tab panels render as <div id="shiny-tab-TABNAME">, but those
# are only in the DOM when the tab is active, so we avoid pointing at them.

#' Build the PPDAC guided tour using cicerone
#' @noRd
build_tour <- function() {
  cicerone::Cicerone$new()$
    step(
      el          = "sidebarMenu",
      title       = "Welcome to CoDa Stereo!",
      description = paste(
        "This application implements the Aitchison geometry for",
        "stereological and histomorphometric data.<br><br>",
        "Follow the sidebar tabs <b>top to bottom</b> to complete",
        "the <b>PPDAC cycle</b>.",
        "<br><br>Click <b>Next</b> to learn about each step."
      )
    )$
    step(
      el          = "sidebarMenu",
      title       = "1. Data Upload",
      description = paste(
        "Upload a <b>CSV or Excel file</b>.<br><br>",
        "After uploading, use the <b>Compositional parts</b> selector",
        "to declare which numeric columns are the parts of the composition.",
        "ID columns are excluded automatically.<br><br>",
        "The <b>Pre-flight check</b> will alert you if any selected",
        "column has an incompatible scale."
      )
    )$
    step(
      el          = "data-parts",
      title       = "Parts selector",
      description = paste(
        "Select <em>only</em> the columns that form the closed composition",
        "(e.g., volume fractions or cell counts summing to a constant).",
        "<br><br>Non-compositional covariates (body weight, biochemical",
        "assays on a different scale) must be unchecked here."
      ),
      position    = "right"
    )$
    step(
      el          = "sidebarMenu",
      title       = "2. Data Inspection",
      description = paste(
        "Before any transformation, inspect your data for:<br>",
        "<ul>",
        "<li><b>Structural zeros</b> — rare tissue compartments</li>",
        "<li><b>Missing values (NA)</b></li>",
        "</ul>",
        "The three-state heatmap distinguishes observed values,",
        "zeros, and NAs — something <code>vis_miss</code> alone cannot."
      )
    )$
    step(
      el          = "imputation-run",
      title       = "3. Zero and NA handling",
      description = paste(
        "Choose the imputation method:<br><br>",
        "<b>CZM</b> — for count zeros (point counts).<br>",
        "<b>multRepl</b> — for rounded zeros (percentages).<br>",
        "<b>lrEM</b> — for rounded zeros, larger samples.<br><br>",
        "If the data has NAs, choose <em>drop rows</em> or",
        "<em>geometric-mean imputation</em> before running."
      ),
      position    = "top"
    )$
    step(
      el          = "analysis-ternary_plot",
      title       = "4a. Ternary plot",
      description = paste(
        "Visualise 3 parts inside the Aitchison simplex.",
        "<br><br>",
        "For <b>D > 3</b>, use <em>Amalgamate into 3 groups</em>",
        "to combine related parts before plotting.",
        "<br><br>",
        "Convex hulls delimit each group's compositional territory."
      ),
      position    = "right"
    )$
    step(
      el          = "analysis-biplot",
      title       = "4b. clr-PCA Biplot",
      description = paste(
        "The Compositional Ray Biplot projects <b>clr-transformed</b>",
        "data onto the first two principal components.<br><br>",
        "Arrow <b>length</b> = importance of each part.<br>",
        "Arrow <b>direction</b> = log-ratio covariance between parts."
      ),
      position    = "left"
    )$
    step(
      el          = "stats-run_manova",
      title       = "5. Statistical Modeling",
      description = paste(
        "Fit a permutational MANOVA on <b>ilr coordinates</b> via RRPP.",
        "<br><br>",
        "Select one or more factors; toggle additive or factorial design.",
        "After fitting, run pairwise post-hoc tests and inspect",
        "residual diagnostics.",
        "<br><br>",
        "All results are downloadable as CSV or PDF."
      ),
      position    = "top"
    )$
    step(
      el          = "sidebarMenu",
      title       = "You're ready!",
      description = paste(
        "That's the full PPDAC cycle.<br><br>",
        "Start by uploading your data or loading the built-in example.",
        "You can restart this tour anytime by clicking the",
        "<b>\U0001f393 Tour</b> button in the header."
      )
    )
}

#' @noRd
mod_tutorial_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # intentionally empty — tour is wired in app_server to avoid
    # namespace issues with buttons defined outside module UI
  })
}
