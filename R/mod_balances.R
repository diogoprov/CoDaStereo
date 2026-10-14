# ==============================================================================
# Module: Balances (SBP editor + CoDa dendrogram)
# ==============================================================================

#' @noRd
mod_balances_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "balances",
    fluidRow(
      box(title = "Sequential binary partition (SBP)", width = 5,
          p("The SBP is built automatically by Ward clustering of the parts ",
            "on clr distances. Edit any cell to define your own balances: ",
            "+1 = numerator, -1 = denominator, 0 = not in this balance."),
          uiOutput(ns("group_var_ui")),
          selectInput(ns("palette"), "Colour palette:", choices = plot_palettes),
          actionButton(ns("reset_sbp"), "Reset to automatic SBP",
                       icon = icon("rotate-left")),
          hr(),
          uiOutput(ns("sbp_grid")),
          uiOutput(ns("sbp_status")),
          tableOutput(ns("sbp_table")),
          downloadButton(ns("dl_sbp"), "Download SBP (CSV)")
      ),
      box(title = "CoDa dendrogram", width = 7,
          p("Each horizontal axis is one balance. The vertical bar above it ",
            "is the balance variance, and where the bar meets the axis is the ",
            "balance mean. Coloured boxes show the balance distribution per group."),
          plotOutput(ns("dendrogram"), height = "500px"),
          downloadButton(ns("dl_dendrogram"), "Download PDF")
      )
    )
  )
}

#' @noRd
mod_balances_server <- function(id, df, parts, comp, keep_rows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cell_id <- function(i, j) paste0("sbp_", i, "_", j)

    output$group_var_ui <- renderUI({
      req(df(), parts())
      selectInput(ns("group_var"), "Colour / group by:",
                  choices = factor_candidates(df(), parts()))
    })

    # Automatic SBP: recomputed when the composition changes; the reset button
    # re-renders the grid from it
    sbp_default <- reactive({
      req(comp())
      sbp_auto(comp())
    })

    output$sbp_grid <- renderUI({
      input$reset_sbp
      base <- sbp_default()
      choices <- c("+1" = 1, "-1" = -1, "0" = 0)
      header <- tags$tr(tags$th(""),
                        purrr::map(colnames(base), \(p) tags$th(p, style = "font-size: 85%;")))
      rows <- purrr::map(seq_len(nrow(base)), \(i) tags$tr(
        tags$th(rownames(base)[i]),
        purrr::map(seq_len(ncol(base)), \(j) tags$td(
          selectInput(ns(cell_id(i, j)), label = NULL, choices = choices,
                      selected = base[i, j], selectize = FALSE, width = "65px")
        ))
      ))
      tags$div(style = "overflow-x: auto;",
               tags$table(class = "table table-condensed", header, rows))
    })

    sbp_current <- reactive({
      base <- sbp_default()
      vals <- purrr::map_dbl(seq_along(base), \(k) {
        i <- row(base)[k]; j <- col(base)[k]
        as.numeric(input[[cell_id(i, j)]] %||% base[i, j])
      })
      matrix(vals, nrow = nrow(base), dimnames = dimnames(base))
    })

    sbp_issues <- reactive(sbp_problems(sbp_current()))

    output$sbp_status <- renderUI({
      issues <- sbp_issues()
      if (length(issues) == 0) {
        tags$p(icon("check-circle"), " Valid SBP.",
               style = "color: #28a745; font-weight: bold;")
      } else {
        tags$div(style = "color: #dc3545;",
                 tags$strong("Invalid SBP:"), tags$ul(purrr::map(issues, tags$li)))
      }
    })

    output$sbp_table <- renderTable({
      req(length(sbp_issues()) == 0)
      sbp_labels(sbp_current())
    })

    draw_dendrogram <- function() {
      req(comp(), input$group_var, keep_rows())
      validate(need(length(sbp_issues()) == 0,
                    "Fix the SBP to draw the dendrogram."))
      plot_coda_dendrogram(comp(), sbp_current(),
                           groups  = df()[[input$group_var]][keep_rows()],
                           palette = input$palette)
    }

    output$dendrogram <- renderPlot(draw_dendrogram())

    output$dl_dendrogram <- downloadHandler(
      filename = function() paste0("coda_dendrogram_", Sys.Date(), ".pdf"),
      content  = function(file) {
        grDevices::pdf(file, width = 9, height = 7)
        on.exit(grDevices::dev.off())
        draw_dendrogram()
      }
    )

    output$dl_sbp <- downloadHandler(
      filename = function() paste0("sbp_", Sys.Date(), ".csv"),
      content  = function(file) {
        utils::write.csv(sbp_current(), file)
      }
    )
  })
}
