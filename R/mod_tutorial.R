# ==============================================================================
# Guided tour using rintrojs
# rintrojs wraps intro.js: each step targets a CSS selector, shows a tooltip,
# and highlights the element with an overlay. No WebSocket timing issues.
# ==============================================================================

#' Build the PPDAC tour steps as a data frame (rintrojs format)
#' @noRd
build_tour_steps <- function() {
  data.frame(
    element = c(
      "#sidebarMenu",
      "#data-parts",
      "#imputation-run",
      "#analysis-ternary_plot",
      "#analysis-biplot",
      "#stats-run_manova",
      "#sidebarMenu"
    ),
    intro = c(
      paste(
        "<b>Welcome to CoDa Stereo!</b><br><br>",
        "This app implements the Aitchison geometry for stereological and",
        "histomorphometric data. Follow the sidebar <b>top to bottom</b>",
        "to complete the PPDAC cycle. Click <b>Next</b> to continue."
      ),
      paste(
        "<b>Compositional parts selector</b><br><br>",
        "Select <em>only</em> the columns that form a closed composition",
        "(volume fractions or cell counts summing to a constant).<br><br>",
        "ID columns are excluded automatically. Uncheck any non-compositional",
        "covariates (body weight, biochemical assays on a different scale).<br><br>",
        "The <b>Pre-flight check</b> on the right alerts you if a selected",
        "column looks incompatible."
      ),
      paste(
        "<b>Zero and NA handling</b><br><br>",
        "Choose the imputation method before running:<br>",
        "<ul>",
        "<li><b>CZM</b> — count zeros (point-counting stereology)</li>",
        "<li><b>multRepl</b> — rounded zeros (percentages, proportions)</li>",
        "<li><b>lrEM</b> — rounded zeros, larger samples</li>",
        "</ul>",
        "If the data has NAs, choose <em>drop rows</em> or",
        "<em>geometric-mean imputation</em> first."
      ),
      paste(
        "<b>Ternary plot</b><br><br>",
        "Visualise 3 parts inside the Aitchison simplex.",
        "Convex hulls delimit each group's compositional territory.<br><br>",
        "For <b>D &gt; 3</b>, use <em>Amalgamate</em> to combine related parts",
        "into 3 super-groups before plotting."
      ),
      paste(
        "<b>Compositional Ray Biplot (clr-PCA)</b><br><br>",
        "Projects <b>clr-transformed</b> data onto the first two PCs.<br>",
        "Arrow <b>length</b> = variance explained by that part.<br>",
        "Arrow <b>direction</b> = log-ratio covariance between parts.<br><br>",
        "Uses the imputed composition — run Zero Handling first."
      ),
      paste(
        "<b>Permutational MANOVA (RRPP)</b><br><br>",
        "Fits a linear model on <b>ilr coordinates</b>.",
        "Select one or more factors; toggle additive or factorial design.",
        "The formula is shown before fitting for transparency.<br><br>",
        "After fitting: run pairwise post-hoc tests and inspect",
        "residual diagnostics. All results are downloadable as CSV or PDF."
      ),
      paste(
        "<b>You're ready!</b><br><br>",
        "That's the full PPDAC cycle.<br>",
        "Start by loading the built-in example or uploading your data.<br><br>",
        "Restart this tour anytime with the <b>&#127891; Tour</b> button."
      )
    ),
    position = c(
      "right", "right", "top", "right", "left", "top", "right"
    ),
    stringsAsFactors = FALSE
  )
}

#' @noRd
mod_tutorial_server <- function(id) {
  # intentionally empty — tour is wired in app_server
}
