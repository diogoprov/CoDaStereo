# ==============================================================================
# Module: Statistical Modeling (MANOVA + Pairwise + Diagnostics + Downloads)
# ==============================================================================

#' @noRd
mod_stats_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "stats",
    fluidRow(
      box(title = "Permutational MANOVA (RRPP)", width = 12, status = "primary",
          p("Build a factorial model on the ilr coordinates. Select one or more ",
            "factors. With two or more factors, choose between an additive model ",
            "(main effects only) or a full factorial (with all interactions)."),
          uiOutput(ns("factors_ui")),
          uiOutput(ns("interaction_ui")),
          verbatimTextOutput(ns("formula_preview")),
          actionButton(ns("run_manova"), "Run Model", icon = icon("play")),
          downloadButton(ns("dl_manova"), "Download ANOVA table (CSV)"),
          hr(),
          verbatimTextOutput(ns("manova_results"))
      )
    ),
    fluidRow(
      box(title = "Pairwise comparisons", width = 6, status = "info",
          p("Permutational pairwise tests on Euclidean distances in ilr space."),
          uiOutput(ns("pairwise_group_ui")),
          actionButton(ns("run_pairwise"), "Run Pairwise", icon = icon("exchange-alt")),
          downloadButton(ns("dl_pairwise"), "Download pairwise (CSV)"),
          hr(),
          verbatimTextOutput(ns("pairwise_results"))
      ),
      box(title = "Model diagnostics", width = 6, status = "warning",
          p("Residual plots from the fitted RRPP model."),
          plotOutput(ns("diag_resid"), height = "300px"),
          plotOutput(ns("diag_pc"), height = "300px")
      )
    )
  )
}

#' @noRd
mod_stats_server <- function(id, df, parts, comp, keep_rows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(model = NULL, manova_out = NULL,
                         pairwise_out = NULL, formula_used = NULL)

    output$factors_ui <- renderUI({
      req(df(), parts())
      cands <- factor_candidates(df(), parts())
      selectInput(ns("factors"), "Factor(s) for the model:",
                  choices = cands, selected = head(cands, 1),
                  multiple = TRUE, width = "100%")
    })

    output$interaction_ui <- renderUI({
      req(input$factors)
      if (length(input$factors) < 2) return(NULL)
      radioButtons(ns("interaction_type"), "Model design:",
                   choices = c("Additive (main effects only)" = "+",
                               "Factorial (with interactions)" = "*"),
                   selected = "+", inline = TRUE)
    })

    manova_formula <- reactive({
      req(input$factors)
      sep <- if (length(input$factors) >= 2 && !is.null(input$interaction_type)) {
        input$interaction_type
      } else { "+" }
      rhs <- paste(input$factors, collapse = paste0(" ", sep, " "))
      stats::as.formula(paste("y ~", rhs))
    })

    output$formula_preview <- renderPrint({
      req(input$factors)
      cat("Formula:  ", deparse(manova_formula()), "\n",
          "(y = ilr coordinates of the imputed composition)", sep = "")
    })

    output$pairwise_group_ui <- renderUI({
      req(input$factors)
      selectInput(ns("pw_group"), "Grouping factor for pairwise tests:",
                  choices = input$factors, selected = input$factors[1])
    })

    observeEvent(input$run_manova, {
      req(comp(), keep_rows(), input$factors)
      fmla <- manova_formula()
      ilr_coords <- compositions::ilr(comp())
      factor_list <- lapply(input$factors, function(f) as.factor(df()[[f]][keep_rows()]))
      names(factor_list) <- input$factors
      df_rrpp <- do.call(RRPP::rrpp.data.frame,
                         c(list(y = as.matrix(ilr_coords)), factor_list))
      rv$model <- RRPP::lm.rrpp(fmla, data = df_rrpp, iter = 999, print.progress = FALSE)
      rv$manova_out  <- anova(rv$model)
      rv$formula_used <- deparse(fmla)
      rv$pairwise_out <- NULL
    })

    output$manova_results <- renderPrint({
      req(rv$manova_out)
      cat("Fitted formula:", rv$formula_used, "\n\n")
      print(rv$manova_out)
    })

    observeEvent(input$run_pairwise, {
      req(rv$model, input$pw_group, keep_rows())
      group_vec <- as.factor(df()[[input$pw_group]][keep_rows()])
      rv$pairwise_out <- RRPP::pairwise(rv$model, groups = group_vec)
    })

    output$pairwise_results <- renderPrint({
      req(rv$pairwise_out)
      cat("Pairwise distances (Euclidean in ilr space)\n\n")
      summary(rv$pairwise_out, test.type = "dist")
    })

    output$diag_resid <- renderPlot({ req(rv$model); plot(rv$model, type = "diagnostics") })
    output$diag_pc    <- renderPlot({ req(rv$model); plot(rv$model, type = "PC", pch = 19) })

    output$dl_manova <- downloadHandler(
      filename = function() paste0("manova_", Sys.Date(), ".csv"),
      content  = function(file) { req(rv$manova_out); utils::write.csv(rv$manova_out$table, file) }
    )

    output$dl_pairwise <- downloadHandler(
      filename = function() paste0("pairwise_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(rv$pairwise_out)
        tbl <- summary(rv$pairwise_out, test.type = "dist")$summary.table
        utils::write.csv(tbl, file)
      }
    )
  })
}
