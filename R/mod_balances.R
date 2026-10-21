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
          downloadButton(ns("dl_dendrogram"), "Download figure"),
          hr(),
          export_settings_ui(ns)
      )
    ),
    fluidRow(
      box(title = "Balances by group", width = 12, status = "info",
          p("Each panel is one balance (numerator / denominator parts). Positive ",
            "values mean the numerator parts dominate. Test each balance with a ",
            "univariate permutational ANOVA; Z is the standardised effect size. ",
            "With several balances, interpret P-values with the multiple ",
            "comparisons in mind."),
          plotOutput(ns("balance_plot"), height = "450px"),
          downloadButton(ns("dl_balance_plot"), "Download figure"),
          actionButton(ns("test_balances"), "Test balances (999 permutations)",
                       icon = icon("vial")),
          downloadButton(ns("dl_balance_tests"), "Download tests (CSV)"),
          tableOutput(ns("balance_tests"))
      )
    )
  )
}

#' @noRd
mod_balances_server <- function(id, df, parts, comp, keep_rows,
                                restore = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    take <- restore_pending(restore, "balances")
    restored_sbp <- reactiveVal(NULL)
    observeEvent(restore(), {
      updateSelectInput(session, "palette", selected = restore()$balances$palette)
      restored_sbp(restore()$balances$sbp)
    })
    cell_id <- function(i, j) paste0("sbp_", i, "_", j)

    output$group_var_ui <- renderUI({
      req(df(), parts())
      selectInput(ns("group_var"), "Colour / group by:",
                  choices = factor_candidates(df(), parts()),
                  selected = take("group_var"))
    })

    # Automatic SBP: recomputed when the composition changes; the reset button
    # re-renders the grid from it
    sbp_default <- reactive({
      req(comp())
      auto <- sbp_auto(comp())
      saved <- restored_sbp()
      if (!is.null(saved) && identical(colnames(saved), colnames(auto))) saved else auto
    })
    observeEvent(input$reset_sbp, restored_sbp(NULL), ignoreInit = TRUE)

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

    # Wait until the user pauses editing before validating and redrawing
    sbp_settled <- debounce(sbp_current, 500)
    sbp_issues  <- reactive(sbp_problems(sbp_settled()))

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
      sbp_labels(sbp_settled())
    })

    draw_dendrogram <- function() {
      req(comp(), input$group_var, keep_rows())
      validate(need(length(sbp_issues()) == 0,
                    "Fix the SBP to draw the dendrogram."))
      plot_coda_dendrogram(comp(), sbp_settled(),
                           groups  = df()[[input$group_var]][keep_rows()],
                           palette = input$palette)
    }

    output$dendrogram <- renderPlot(draw_dendrogram()) |>
      bindCache(comp(), keep_rows(), sbp_settled(), input$group_var, input$palette)
    output$dl_dendrogram <- plot_download(input, "coda_dendrogram",
                                          function() draw_dendrogram)

    # -- Balances by group -----------------------------------------------------
    balances <- reactive({
      req(comp(), length(sbp_issues()) == 0)
      balance_coords(comp(), sbp_settled())
    })
    bal_groups <- reactive({
      req(input$group_var, keep_rows())
      df()[[input$group_var]][keep_rows()]
    })
    balance_plot_build <- function() {
      plot_balances(balances(), bal_groups(), sbp_settled(), palette = input$palette)
    }
    output$balance_plot <- renderPlot(balance_plot_build()) |>
      bindCache(comp(), keep_rows(), sbp_settled(), input$group_var, input$palette)
    output$dl_balance_plot <- plot_download(input, "balances", balance_plot_build)

    tests <- reactiveVal(NULL)
    observeEvent(list(sbp_settled(), input$group_var), tests(NULL))
    observeEvent(input$test_balances, {
      withProgress(message = "Testing balances...", {
        tests(balance_tests(balances(), bal_groups()))
      })
    })
    output$balance_tests <- renderTable({
      req(tests())
      merge(sbp_labels(sbp_settled()), tests(), by = "Balance")
    }, digits = 3)
    output$dl_balance_tests <- downloadHandler(
      filename = function() paste0("balance_tests_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(tests())
        utils::write.csv(merge(sbp_labels(sbp_settled()), tests(), by = "Balance"),
                         file, row.names = FALSE)
      }
    )

    output$dl_sbp <- downloadHandler(
      filename = function() paste0("sbp_", Sys.Date(), ".csv"),
      content  = function(file) {
        utils::write.csv(sbp_current(), file)
      }
    )


    # Inputs built by renderUI must exist even in hidden tabs (session restore)
    outputOptions(output, "group_var_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "sbp_grid", suspendWhenHidden = FALSE)
    list(sbp = reactive(if (length(sbp_issues()) == 0) sbp_settled()),
         group_var = reactive(input$group_var), palette = reactive(input$palette))
  })
}
