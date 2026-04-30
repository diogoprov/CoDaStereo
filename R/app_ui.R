# ==============================================================================
# Main UI — assembles module UIs into the dashboard
# ==============================================================================

#' @noRd
app_ui <- function(request) {
  dashboardPage(
    dashboardHeader(title = "CoDa Stereo v2.0"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Introduction", tabName = "intro",
                 icon = icon("info-circle")),
        menuItem("Data Upload", tabName = "data",
                 icon = icon("upload")),
        menuItem("Data Inspection", tabName = "inspect",
                 icon = icon("magnifying-glass")),
        menuItem("Zero Handling", tabName = "zeros",
                 icon = icon("eraser")),
        menuItem("Compositional Analysis", tabName = "analysis",
                 icon = icon("chart-pie")),
        menuItem("Statistical Modeling", tabName = "stats",
                 icon = icon("microscope"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "intro",
                h2("Welcome to CoDa Stereo"),
                p("This tool implements the Aitchison geometry for ",
                  "stereological and histomorphometric data."),
                p("Follow the sidebar from top to bottom to complete the ",
                  "PPDAC cycle."),
                hr(),
                tags$ul(
                  tags$li(strong("Data Upload"), ": load a CSV or Excel file ",
                          "and select which columns are compositional parts."),
                  tags$li(strong("Data Inspection"), ": visualise patterns of ",
                          "zeros and missing values across parts."),
                  tags$li(strong("Zero Handling"), ": impute zeros and NAs ",
                          "using Bayesian or log-ratio methods."),
                  tags$li(strong("Compositional Analysis"), ": ternary plots ",
                          "(with optional amalgamation) and clr-PCA biplots."),
                  tags$li(strong("Statistical Modeling"), ": permutational ",
                          "MANOVA, pairwise post-hoc comparisons, and ",
                          "residual diagnostics.")
                )
        ),
        mod_data_upload_ui("data"),
        mod_data_inspect_ui("data"),
        mod_imputation_ui("imputation"),
        mod_analysis_ui("analysis"),
        mod_stats_ui("stats")
      )
    )
  )
}
