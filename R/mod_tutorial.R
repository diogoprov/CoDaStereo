# ==============================================================================
# Guided tour using rintrojs
# rintrojs wraps intro.js: each step targets a CSS selector, shows a tooltip,
# and highlights the element with an overlay. No WebSocket timing issues.
# ==============================================================================

#' Build the PPDAC tour steps as a data frame (rintrojs format)
#'
#' IMPORTANT: in shinydashboard, tab content is only rendered in the DOM when
#' that tab is active. Steps that target inactive-tab elements will silently
#' fail and collapse the tour. Safe anchors are:
#'   - sidebar menu items (always in DOM)
#'   - the header tour button
#' Steps for tab content use element = "" (centred floating tooltip).
#' @noRd
build_tour_steps <- function() {
  data.frame(
    element = c(
      "#sidebarMenu",                        # 1 welcome
      "a[data-value='data']",                # 2 data upload tab link
      "a[data-value='data']",                # 3 parts selector (proxy)
      "a[data-value='inspect']",             # 4 data inspection tab link
      "a[data-value='zeros']",               # 5 zero handling tab link
      "a[data-value='analysis']",            # 6 ternary + biplot tab link
      "a[data-value='stats']",               # 7 MANOVA tab link
      "#tour_btn_header"                     # 8 done — point at tour button
    ),
    intro = c(
      # 1 Welcome
      paste(
        "<b>Welcome to CoDa Stereo!</b><br><br>",
        "This app implements the Aitchison geometry for stereological and",
        "histomorphometric data.<br><br>",
        "Follow the sidebar <b>top to bottom</b> to complete the PPDAC cycle.",
        "Click <b>Next</b> to continue."
      ),
      # 2 Data Upload
      paste(
        "<b>1 &mdash; Data Upload</b><br><br>",
        "Upload a <b>CSV or Excel file</b> here.<br><br>",
        "After uploading, the <b>Compositional parts</b> selector lets you",
        "choose which numeric columns are the parts of the composition.",
        "ID columns are excluded automatically.<br><br>",
        "A <b>Pre-flight check</b> alerts you if a selected column has",
        "an incompatible scale (e.g., Glycogen measured separately)."
      ),
      # 3 Parts selector detail
      paste(
        "<b>Parts selector — what to include</b><br><br>",
        "Select <em>only</em> columns that form a <b>closed composition</b>",
        "(volume fractions, area percentages, or cell counts",
        "summing to a constant).<br><br>",
        "Non-compositional covariates (body weight, biochemical assays",
        "on a different scale) must be unchecked."
      ),
      # 4 Data Inspection
      paste(
        "<b>2 &mdash; Data Inspection</b><br><br>",
        "Before any transformation, visualise:<br>",
        "<ul>",
        "<li><b>Structural zeros</b> — rare tissue compartments</li>",
        "<li><b>Missing values (NA)</b></li>",
        "</ul>",
        "The three-state heatmap distinguishes observed values, zeros,",
        "and NAs — a critical distinction for choosing the right",
        "imputation method."
      ),
      # 5 Zero Handling
      paste(
        "<b>3 &mdash; Zero and NA handling</b><br><br>",
        "Choose the imputation method before running:<br>",
        "<ul>",
        "<li><b>CZM</b> &mdash; count zeros (point-counting stereology)</li>",
        "<li><b>multRepl</b> &mdash; rounded zeros (percentages)</li>",
        "<li><b>lrEM</b> &mdash; rounded zeros, larger samples</li>",
        "</ul>",
        "If the data has NAs, choose <em>drop rows</em> or",
        "<em>geometric-mean imputation</em> first."
      ),
      # 6 Compositional Analysis
      paste(
        "<b>4 &mdash; Compositional Analysis</b><br><br>",
        "<b>Ternary plot</b>: visualise 3 parts in the simplex.",
        "Use <em>Amalgamate</em> if D &gt; 3.",
        "Convex hulls delimit each group's territory.<br><br>",
        "<b>clr-PCA Biplot</b>: arrow length = variance; direction =",
        "log-ratio covariance between parts.<br><br>",
        "<b>Compositional descriptives</b>: geometric-mean composition",
        "and total variance per group."
      ),
      # 7 Statistical Modeling
      paste(
        "<b>5 &mdash; Statistical Modeling</b><br><br>",
        "Fits a permutational MANOVA on <b>ilr coordinates</b> via RRPP.",
        "<br><br>",
        "Select one or more factors; toggle additive or factorial design.",
        "The formula is previewed before fitting.<br><br>",
        "After fitting: run <b>pairwise post-hoc tests</b> and inspect",
        "<b>residual diagnostics</b>.",
        "All results are downloadable as CSV or PDF."
      ),
      # 8 Done
      paste(
        "<b>You're ready!</b><br><br>",
        "That covers the full PPDAC cycle.<br><br>",
        "Start by loading the built-in example",
        "(<em>Load Example Data</em> in the Data Upload tab)",
        "or uploading your own file.<br><br>",
        "Click this button anytime to restart the tour."
      )
    ),
    position = c(
      "right",   # 1 sidebarMenu
      "right",   # 2 data tab
      "right",   # 3 data tab (same anchor, different text)
      "right",   # 4 inspect tab
      "right",   # 5 zeros tab
      "right",   # 6 analysis tab
      "right",   # 7 stats tab
      "bottom"   # 8 tour button
    ),
    stringsAsFactors = FALSE
  )
}

#' @noRd
mod_tutorial_server <- function(id) {
  # intentionally empty — tour is wired in app_server
}
