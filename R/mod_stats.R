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
          fluidRow(
            column(6,
              uiOutput(ns("factors_ui")),
              uiOutput(ns("interaction_ui")),
              uiOutput(ns("covariates_ui")),
              uiOutput(ns("block_ui"))
            ),
            column(6,
              radioButtons(ns("ss_type"), "Sums of squares:",
                           choices = c("Type I (sequential)" = "I",
                                       "Type II" = "II", "Type III" = "III"),
                           selected = "I", inline = TRUE),
              selectInput(ns("iter"), "Permutations:",
                          choices = c("199 (quick look)" = 199,
                                      "999 (default)" = 999,
                                      "9999 (final analysis)" = 9999),
                          selected = 999),
              radioButtons(ns("total_mode"), "Total (size):",
                           choices = c("Not used (composition only)" = "none",
                                       "Joint response (composition + total)" = "joint",
                                       "Covariate (does composition change with size?)" = "covariate"),
                           selected = "none")
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] != 'none'", ns("total_mode")),
            radioButtons(ns("total_type"), "Total:",
                         choices = c("Sum of parts (Ts; recommended)" = "sum",
                                     "Product of parts^(1/sqrt(D)) (Tp; same as log of each part)" = "product"),
                         selected = "sum", inline = TRUE),
            p(style = "color: #666; font-size: 90%;",
              "Uses the log-total of each specimen (Pawlowsky-Glahn, Egozcue & ",
              "Lovell 2015, Statistical Modelling 15:175-190). As a joint response ",
              "it tests composition and size together; as a covariate it asks ",
              "whether the composition shifts with size. Use it only when parts ",
              "are on an absolute scale (e.g., absolute volumes, cells per ",
              "microlitre). Point counts with a fixed number of grid points or ",
              "cells have a total fixed by design."),
            uiOutput(ns("total_warning"))
          ),
          verbatimTextOutput(ns("formula_preview")),
          actionButton(ns("run_manova"), "Run Model", icon = icon("play")),
          downloadButton(ns("dl_manova"), "Download ANOVA table (CSV)"),
          uiOutput(ns("run_status")),
          hr(),
          verbatimTextOutput(ns("manova_results"))
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'joint'", ns("total_mode")),
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
          p("Permutational pairwise tests between group means (Euclidean ",
            "distances in ilr space, plus the log-total when included) and ",
            "between group dispersions (compositional disparity). Z is the ",
            "standardised effect size."),
          uiOutput(ns("pairwise_group_ui")),
          actionButton(ns("run_pairwise"), "Run Pairwise", icon = icon("exchange-alt")),
          downloadButton(ns("dl_pairwise"), "Download pairwise (CSV)"),
          uiOutput(ns("pw_status")),
          hr(),
          h5(strong("Differences in means")),
          verbatimTextOutput(ns("pairwise_results")),
          h5(strong("Differences in dispersion")),
          verbatimTextOutput(ns("dispersion_results"))
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
    rv <- reactiveValues(fit = NULL, pairwise = NULL)
    # Results come back from a worker: register RRPP's print/plot S3 methods here
    loadNamespace("RRPP")

    total_mode <- reactive({
      if (is.null(raw)) "none" else input$total_mode %||% "none"
    })

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

    output$covariates_ui <- renderUI({
      req(df(), parts())
      cands <- covariate_candidates(df(), parts())
      if (length(cands) == 0) return(NULL)
      selectInput(ns("covariates"), "Numeric covariate(s) (ANCOVA):",
                  choices = cands, multiple = TRUE, width = "100%")
    })

    output$block_ui <- renderUI({
      req(df(), parts())
      cands <- setdiff(names(df()), c(parts(), input$factors))
      selectInput(ns("block"), "Block (permutations restricted within blocks):",
                  choices = c("None" = "", cands), selected = "", width = "100%")
    })

    model_spec_rhs <- reactive({
      req(input$factors)
      design <- if (length(input$factors) >= 2) input$interaction_type %||% "+" else "+"
      model_rhs(input$factors, design, input$covariates %||% character(0),
                log_total_cov = total_mode() == "covariate")
    })

    output$formula_preview <- renderPrint({
      req(input$factors)
      cat("Formula:  y ~ ", model_spec_rhs(), "\n",
          switch(total_mode(),
            joint = sprintf("(y = ilr coordinates of the imputed composition + log-total [%s])",
                            input$total_type),
            covariate = sprintf("(y = ilr coordinates; log-total [%s] as covariate)",
                                input$total_type),
            "(y = ilr coordinates of the imputed composition)"),
          if (nzchar(input$block %||% "")) sprintf("\nBlock: %s", input$block),
          sep = "")
    })

    output$pairwise_group_ui <- renderUI({
      req(input$factors)
      selectInput(ns("pw_group"), "Grouping factor for pairwise tests:",
                  choices = input$factors, selected = input$factors[1])
    })

    # -- Model fitting (background worker when available) ----------------------
    build_spec <- function() {
      kr   <- keep_rows()
      cols <- unique(c(input$factors, input$covariates,
                       if (nzchar(input$block %||% "")) input$block))
      d <- df()[kr, cols, drop = FALSE]
      d[] <- purrr::map(names(d), \(v) if (v %in% input$covariates) d[[v]] else as.factor(d[[v]]))
      list(y          = unclass(compositions::ilr(comp())),
           data       = d,
           rhs        = model_spec_rhs(),
           iter       = as.integer(input$iter),
           seed       = 42,
           ss_type    = input$ss_type,
           block      = if (nzchar(input$block %||% "")) input$block,
           total_mode = total_mode(),
           log_total  = if (total_mode() != "none") log_total_coord(raw(), comp(), input$total_type))
    }

    fit_task <- ExtendedTask$new(function(spec) run_in_worker(fit_models, spec))
    pw_task  <- ExtendedTask$new(function(arg) run_in_worker(fit_pairwise, arg))

    observeEvent(input$run_manova, {
      req(comp(), keep_rows(), input$factors)
      spec <- build_spec()
      rv$pairwise <- NULL
      if (use_async()) {
        fit_task$invoke(spec)
      } else {
        withProgress(message = "Running permutations...", {
          rv$fit <- fit_models(spec)
        })
      }
    })

    observeEvent(fit_task$status(), {
      if (fit_task$status() == "success") rv$fit <- fit_task$result()
      if (fit_task$status() == "error") {
        showNotification(paste("Model failed:", conditionMessage(
          tryCatch(fit_task$result(), error = identity))), type = "error", duration = 10)
      }
    })

    status_badge <- function(task) {
      renderUI({
        if (use_async() && task$status() == "running") {
          tags$p(style = "color: #31708f; margin-top: 8px;",
                 icon("spinner", class = "fa-spin"),
                 sprintf(" Running %s permutations in the background...", input$iter))
        }
      })
    }
    output$run_status <- status_badge(fit_task)
    output$pw_status  <- status_badge(pw_task)

    output$manova_results <- renderPrint({
      req(rv$fit)
      cat("Fitted formula: y ~", rv$fit$rhs, "\n\n")
      print(rv$fit$anova)
    })

    observeEvent(input$run_pairwise, {
      req(rv$fit, input$pw_group, keep_rows())
      arg <- list(model  = rv$fit$model,
                  groups = as.factor(df()[[input$pw_group]][keep_rows()]))
      if (use_async()) {
        pw_task$invoke(arg)
      } else {
        withProgress(message = "Running pairwise tests...", {
          rv$pairwise <- fit_pairwise(arg)
        })
      }
    })

    observeEvent(pw_task$status(), {
      if (pw_task$status() == "success") rv$pairwise <- pw_task$result()
    })

    output$total_results <- renderPrint({
      req(rv$fit$total_anova)
      cat("log-total ~", rv$fit$rhs, "\n\n")
      print(rv$fit$total_anova)
    })

    output$total_plot <- renderPlot({
      req(rv$fit$total_df)
      ggplot2::ggplot(rv$fit$total_df, ggplot2::aes(x = .data$group, y = .data$log_total)) +
        ggplot2::geom_boxplot(outlier.shape = NA, width = 0.5) +
        ggplot2::geom_jitter(width = 0.1, height = 0, alpha = 0.6) +
        ggplot2::labs(x = input$factors[1], y = "log-total") +
        ggplot2::theme_minimal(base_size = 14)
    })

    output$pairwise_results <- renderPrint({
      req(rv$pairwise)
      rv$pairwise$dist
    })

    output$dispersion_results <- renderPrint({
      req(rv$pairwise)
      rv$pairwise$var
    })

    output$diag_resid <- renderPlot({ req(rv$fit); plot(rv$fit$model, type = "diagnostics") })
    output$diag_pc    <- renderPlot({ req(rv$fit); plot(rv$fit$model, type = "PC", pch = 19) })

    output$dl_manova <- downloadHandler(
      filename = function() paste0("manova_", Sys.Date(), ".csv"),
      content  = function(file) { req(rv$fit); utils::write.csv(rv$fit$anova$table, file) }
    )

    output$dl_pairwise <- downloadHandler(
      filename = function() paste0("pairwise_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(rv$pairwise)
        tbl <- rbind(
          data.frame(test = "means", comparison = rownames(rv$pairwise$dist$summary.table),
                     rv$pairwise$dist$summary.table, check.names = FALSE, row.names = NULL),
          data.frame(test = "dispersion", comparison = rownames(rv$pairwise$var$summary.table),
                     rv$pairwise$var$summary.table, check.names = FALSE, row.names = NULL))
        utils::write.csv(tbl, file, row.names = FALSE)
      }
    )

    list(fit = reactive(rv$fit), pairwise = reactive(rv$pairwise),
         inputs = reactive(list(factors = input$factors, rhs = if (length(input$factors)) model_spec_rhs(),
                                iter = input$iter, ss_type = input$ss_type, block = input$block,
                                total_mode = total_mode(), total_type = input$total_type,
                                pw_group = input$pw_group)))
  })
}
