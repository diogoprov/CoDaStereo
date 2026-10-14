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
          checkboxInput(ns("include_total"),
                        "Include the total (composition + total)", value = FALSE),
          conditionalPanel(
            condition = sprintf("input['%s']", ns("include_total")),
            radioButtons(ns("total_type"), "Total:",
                         choices = c("Sum of parts (Ts; recommended)" = "sum",
                                     "Product of parts^(1/sqrt(D)) (Tp; same as log of each part)" = "product"),
                         selected = "sum"),
            p(style = "color: #666; font-size: 90%;",
              "Adds the log-total as one more response coordinate, so the model ",
              "tests differences in composition and in size jointly ",
              "(Pawlowsky-Glahn, Egozcue & Lovell 2015, Statistical Modelling ",
              "15:175-190). Use it only when parts are on an absolute scale ",
              "(e.g., absolute volumes, cells per microlitre). Point counts with a ",
              "fixed number of grid points or cells have a total fixed by design."),
            uiOutput(ns("total_warning"))
          ),
          verbatimTextOutput(ns("formula_preview")),
          actionButton(ns("run_manova"), "Run Model", icon = icon("play")),
          downloadButton(ns("dl_manova"), "Download ANOVA table (CSV)"),
          hr(),
          verbatimTextOutput(ns("manova_results"))
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s']", ns("include_total")),
      fluidRow(
        box(title = "Total (size) alone", width = 12, status = "info",
            p("Univariate permutational ANOVA of the log-total with the same ",
              "factors, to separate the size effect from the compositional one."),
            fluidRow(
              column(6, verbatimTextOutput(ns("total_results"))),
              column(6, plotOutput(ns("total_plot"), height = "300px"))
            ))
      )
    ),
    fluidRow(
      box(title = "Pairwise comparisons", width = 6, status = "info",
          p("Permutational pairwise tests on Euclidean distances in ilr space ",
            "(plus the log-total when included)."),
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
mod_stats_server <- function(id, df, parts, comp, keep_rows, raw = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(model = NULL, manova_out = NULL,
                         pairwise_out = NULL, formula_used = NULL,
                         total_out = NULL, total_df = NULL)

    use_total <- reactive(isTRUE(input$include_total) && !is.null(raw))

    output$total_warning <- renderUI({
      req(raw, raw())
      cv <- total_cv(raw())
      if (cv >= 0.05) return(NULL)
      tags$p(style = "color: #856404; background: #fff3cd; padding: 8px; border-radius: 4px;",
             icon("exclamation-triangle"),
             sprintf(paste0(" Totals are almost constant (CV = %.1f%%): they are probably ",
                            "fixed by the sampling design and carry no biological information. ",
                            "Do not include the total."), cv * 100))
    })

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
          if (use_total()) {
            sprintf("(y = ilr coordinates of the imputed composition + log-total [%s])",
                    input$total_type)
          } else {
            "(y = ilr coordinates of the imputed composition)"
          }, sep = "")
    })

    output$pairwise_group_ui <- renderUI({
      req(input$factors)
      selectInput(ns("pw_group"), "Grouping factor for pairwise tests:",
                  choices = input$factors, selected = input$factors[1])
    })

    observeEvent(input$run_manova, {
      req(comp(), keep_rows(), input$factors)
      fmla <- manova_formula()
      y <- unclass(compositions::ilr(comp()))
      factor_list <- lapply(input$factors, function(f) as.factor(df()[[f]][keep_rows()]))
      names(factor_list) <- input$factors
      rv$total_out <- NULL
      rv$total_df  <- NULL
      if (use_total()) {
        log_total <- log_total_coord(raw(), comp(), input$total_type)
        y <- cbind(y, log_total = log_total)
        df_tot <- do.call(RRPP::rrpp.data.frame,
                          c(list(y = log_total), factor_list))
        rv$total_out <- anova(RRPP::lm.rrpp(fmla, data = df_tot, iter = 999,
                                            print.progress = FALSE))
        rv$total_df  <- data.frame(group = factor_list[[1]], log_total = log_total)
      }
      df_rrpp <- do.call(RRPP::rrpp.data.frame,
                         c(list(y = as.matrix(y)), factor_list))
      rv$model <- RRPP::lm.rrpp(fmla, data = df_rrpp, iter = 999, print.progress = FALSE)
      rv$manova_out  <- anova(rv$model)
      rv$formula_used <- paste0(deparse(fmla),
                                if (use_total()) paste0("  [y includes log-total: ", input$total_type, "]"))
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

    output$total_results <- renderPrint({
      req(rv$total_out)
      cat("log-total ~", paste(input$factors, collapse = ", "), "\n\n")
      print(rv$total_out)
    })

    output$total_plot <- renderPlot({
      req(rv$total_df)
      ggplot2::ggplot(rv$total_df, ggplot2::aes(x = .data$group, y = .data$log_total)) +
        ggplot2::geom_boxplot(outlier.shape = NA, width = 0.5) +
        ggplot2::geom_jitter(width = 0.1, height = 0, alpha = 0.6) +
        ggplot2::labs(x = input$factors[1], y = "log-total") +
        ggplot2::theme_minimal(base_size = 14)
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
