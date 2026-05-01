# ==============================================================================
# Main UI — assembles module UIs into the dashboard
# ==============================================================================

#' @noRd
app_ui <- function(request) {
  tagList(
    cicerone::use_cicerone(),  # inject cicerone JS/CSS
    dashboardPage(
      dashboardHeader(
        title = "CoDa Stereo v2.1",
        tags$li(class = "dropdown",
                actionButton("tutorial-start_tour",
                             label = "\U0001f393 Tour",
                             class = "btn btn-default btn-sm",
                             style = "margin-top: 8px; margin-right: 10px;"))
      ),
      dashboardSidebar(
        sidebarMenu(id = "sidebarMenu",
          menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
          menuItem("Data Upload", tabName = "data", icon = icon("upload")),
          menuItem("Data Inspection", tabName = "inspect", icon = icon("magnifying-glass")),
          menuItem("Zero Handling", tabName = "zeros", icon = icon("eraser")),
          menuItem("Compositional Analysis", tabName = "analysis", icon = icon("chart-pie")),
          menuItem("Statistical Modeling", tabName = "stats", icon = icon("microscope"))
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "intro",
                  fluidRow(
                    column(3, tags$img(src = "logo/codastreo_logo.svg",
                                       width = "100%", style = "max-width: 220px;")),
                    column(9,
                           h2("Welcome to CoDa Stereo"),
                           p("This tool implements the Aitchison geometry for ",
                             "stereological and histomorphometric data."),
                           p("Follow the sidebar from top to bottom to complete the ",
                             "PPDAC cycle.")
                    )
                  ),
                  hr(),
                  tags$ul(
                    tags$li(strong("Data Upload"), ": load a CSV or Excel file ",
                            "and select which columns are compositional parts. ",
                            "A pre-flight check alerts you to potential issues."),
                    tags$li(strong("Data Inspection"), ": visualise patterns of ",
                            "zeros and missing values across parts."),
                    tags$li(strong("Zero Handling"), ": impute zeros and NAs ",
                            "using Bayesian or log-ratio methods."),
                    tags$li(strong("Compositional Analysis"), ": ternary plots ",
                            "(with optional amalgamation), clr-PCA biplots, and ",
                            "compositional descriptives per group."),
                    tags$li(strong("Statistical Modeling"), ": permutational ",
                            "MANOVA, pairwise post-hoc comparisons, and ",
                            "residual diagnostics.")
                  ),
                  hr(),
                  actionButton("tutorial-start_tour",
                               "Start guided tour",
                               icon = icon("graduation-cap"),
                               class = "btn-primary btn-lg")
          ),
          mod_data_upload_ui("data"),
          mod_data_inspect_ui("data"),
          mod_imputation_ui("imputation"),
          mod_analysis_ui("analysis"),
          mod_stats_ui("stats")
        )
      )
    )
  )
}
