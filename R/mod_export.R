# ==============================================================================
# Module: Reproducibility (R script, HTML report, save / load session)
# ==============================================================================

#' @noRd
mod_export_ui <- function(id, lang = "en") {
  ns <- NS(id)
  t  <- function(x) tr(x, lang)
  tabItem(tabName = "export",
    fluidRow(
      box(title = t("R script"), width = 7, status = "primary",
          p(t("An R script that reproduces the steps you ran in the app (data, aggregation, zero replacement, balances and the permutational MANOVA), for the supplementary material of a paper.")),
          downloadButton(ns("dl_script"), t("Download R script")),
          hr(),
          verbatimTextOutput(ns("script_preview"))
      ),
      box(title = t("Report and session"), width = 5, status = "info",
          h4(t("Report")),
          p(t("A self-contained HTML report with the settings, figures, tables, model output and the R script.")),
          downloadButton(ns("dl_report"), t("Download HTML report")),
          hr(),
          h4(t("Session")),
          p(t("Save the data and every setting to a file and load it later to resume the analysis. Results are recomputed on loading (the same seed gives the same results).")),
          downloadButton(ns("dl_session"), t("Save session")),
          br(), br(),
          fileInput(ns("load_session"), t("Load session (.json)"), accept = ".json",
                    buttonLabel = t("Browse..."), placeholder = t("No file selected"))
      )
    )
  )
}

#' @noRd
mod_export_server <- function(id, state, restore) {
  moduleServer(id, function(input, output, session) {
    t <- session_tr(session)

    output$script_preview <- renderText({
      validate(need(!is.null(tryCatch(state(), error = function(e) NULL)),
                    t("Load data to generate the script.")))
      paste(build_r_script(state()), collapse = "\n")
    })

    output$dl_script <- downloadHandler(
      filename = function() paste0("codastereo_analysis_", Sys.Date(), ".R"),
      content  = function(file) writeLines(build_r_script(state()), file)
    )

    output$dl_report <- downloadHandler(
      filename = function() paste0("codastereo_report_", Sys.Date(), ".html"),
      content  = function(file) {
        withProgress(message = t("Building report..."), build_report(state(), file))
      }
    )

    output$dl_session <- downloadHandler(
      filename = function() paste0("codastereo_session_", Sys.Date(), ".json"),
      content  = function(file) writeLines(session_to_json(state()), file)
    )

    observeEvent(input$load_session, {
      st <- tryCatch(
        session_from_json(paste(readLines(input$load_session$datapath, warn = FALSE),
                                collapse = "\n")),
        error = function(e) e)
      if (inherits(st, "error")) {
        showNotification(t(conditionMessage(st)), type = "error", duration = 8)
      } else {
        restore(st)
        showNotification(t("Session loaded: recomputing results..."), type = "message")
      }
    })
  })
}
