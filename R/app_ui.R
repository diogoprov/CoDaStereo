# ==============================================================================
# Main UI — assembles module UIs into the dashboard
# ==============================================================================

#' @noRd
app_ui <- function(request) {
  lang <- lang_from_query(request$QUERY_STRING)
  t <- function(x) tr(x, lang)
  intro_item <- function(name, text) tags$li(strong(t(name)), HTML(paste0(": ", t(text))))
  tagList(
    rintrojs::introjsUI(),
    dashboardPage(
      dashboardHeader(
        title = "CoDa Stereo v3.1",
        tags$li(class = "dropdown",
                tags$a(href = "?lang=en", title = "English",
                       style = if (lang == "en") "font-weight: bold;", "EN")),
        tags$li(class = "dropdown",
                tags$a(href = "?lang=pt", title = "Portugu\u00eas",
                       style = if (lang == "pt") "font-weight: bold;", "PT")),
        tags$li(class = "dropdown",
                actionButton("tour_btn_header",
                             label = paste("\U0001f393", t("Tour")),
                             class = "btn btn-default btn-sm",
                             style = "margin-top: 8px; margin-right: 10px;"))
      ),
      dashboardSidebar(
        sidebarMenu(id = "sidebarMenu",
          menuItem(t("Introduction"), tabName = "intro", icon = icon("info-circle")),
          menuItem(t("Data Upload"), tabName = "data", icon = icon("upload")),
          menuItem(t("Data Inspection"), tabName = "inspect", icon = icon("magnifying-glass")),
          menuItem(t("Zero Handling"), tabName = "zeros", icon = icon("eraser")),
          menuItem(t("Compositional Analysis"), tabName = "analysis", icon = icon("chart-pie")),
          menuItem(t("Balances (SBP)"), tabName = "balances", icon = icon("sitemap")),
          menuItem(t("Statistical Modeling"), tabName = "stats", icon = icon("microscope")),
          menuItem(t("Reproducibility"), tabName = "export", icon = icon("file-code"))
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(tabName = "intro",
                  fluidRow(
                    column(3, tags$img(src = "logo/codastreo_logo.svg",
                                       width = "100%", style = "max-width: 220px;")),
                    column(9,
                           h2(t("Welcome to CoDa Stereo")),
                           p(t("This tool implements the Aitchison geometry for stereological and histomorphometric data.")),
                           p(t("Follow the sidebar from top to bottom to complete the PPDAC cycle.")),
                           p(style = "color: #666;",
                             t("The interface is available in English and Portuguese (EN / PT at the top). Changing the language reloads the app: save your session first. Figures, the report and the R script stay in English, for publication."))
                    )
                  ),
                  hr(),
                  tags$ul(
                    intro_item("Data Upload", "load a CSV or Excel file, or one of the example datasets, and select which columns are compositional parts. Fields of view can be aggregated by specimen. A pre-flight check alerts you to potential issues."),
                    intro_item("Data Inspection", "visualise patterns of zeros and missing values across parts."),
                    intro_item("Zero Handling", "impute zeros and NAs using Bayesian or log-ratio methods."),
                    intro_item("Compositional Analysis", "ternary plots (with optional amalgamation and group regions), clr-PCA biplots, the variation matrix, and compositional descriptives per group."),
                    intro_item("Balances (SBP)", "define a sequential binary partition (automatic or edited), view it as a CoDa dendrogram and test each balance."),
                    intro_item("Statistical Modeling", "permutational MANOVA (with covariates, blocks and the total), pairwise post-hoc comparisons of means and dispersions, and residual diagnostics."),
                    intro_item("Reproducibility", "download an R script, an HTML report, or save and reload the session.")
                  ),
                  hr(),
                  actionButton("tour_btn_intro",
                               t("Start guided tour"),
                               icon = icon("graduation-cap"),
                               class = "btn-primary btn-lg")
          ),
          mod_data_upload_ui("data", lang),
          mod_data_inspect_ui("data", lang),
          mod_imputation_ui("imputation", lang),
          mod_analysis_ui("analysis", lang),
          mod_balances_ui("balances", lang),
          mod_stats_ui("stats", lang),
          mod_export_ui("export", lang)
        )
      )
    )
  )
}
