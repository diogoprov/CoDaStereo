# ==============================================================================
# Module: Statistical Modeling (MANOVA + Pairwise + Diagnostics + Downloads)
# ==============================================================================

#' @noRd
mod_stats_ui <- function(id, lang = "en") {
  ns <- NS(id)
  t  <- function(x) tr(x, lang)
  tabItem(tabName = "stats",
    fluidRow(
      box(title = t("Permutational MANOVA (RRPP)"), width = 12, status = "primary",
          p(t("Build a factorial model on the ilr coordinates. Select one or more factors. With two or more factors, choose between an additive model (main effects only) or a full factorial (with all interactions).")),
          fluidRow(
            column(6,
              uiOutput(ns("factors_ui")),
              uiOutput(ns("interaction_ui")),
              uiOutput(ns("covariates_ui")),
              uiOutput(ns("block_ui"))
            ),
            column(6,
              radioButtons(ns("ss_type"), t("Sums of squares:"),
                           choices = tr_choices(c("Type I (sequential)" = "I",
                                                  "Type II" = "II", "Type III" = "III"), lang),
                           selected = "I", inline = TRUE),
              selectInput(ns("iter"), t("Permutations:"),
                          choices = tr_choices(c("199 (quick look)" = 199,
                                                 "999 (default)" = 999,
                                                 "9999 (final analysis)" = 9999), lang),
                          selected = 999),
              radioButtons(ns("total_mode"), t("Total (size):"),
                           choices = tr_choices(c(
                             "Not used (composition only)" = "none",
                             "Joint response (composition + total)" = "joint",
                             "Covariate (does composition change with size?)" = "covariate"), lang),
                           selected = "none")
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] != 'none'", ns("total_mode")),
            radioButtons(ns("total_type"), t("Total:"),
                         choices = tr_choices(c(
                           "Sum of parts (Ts; recommended)" = "sum",
                           "Product of parts^(1/sqrt(D)) (Tp; same as log of each part)" = "product"), lang),
                         selected = "sum", inline = TRUE),
            p(style = "color: #666; font-size: 90%;",
              t("Uses the log-total of each specimen (Pawlowsky-Glahn, Egozcue & Lovell 2015, Statistical Modelling 15:175-190). As a joint response it tests composition and size together; as a covariate it asks whether the composition shifts with size. Use it only when parts are on an absolute scale (e.g., absolute volumes, cells per microlitre). Point counts with a fixed number of grid points or cells have a total fixed by design.")),
            uiOutput(ns("total_warning"))
          ),
          verbatimTextOutput(ns("formula_preview")),
          actionButton(ns("run_manova"), t("Run Model"), icon = icon("play")),
          downloadButton(ns("dl_manova"), t("Download ANOVA table (CSV)")),
          uiOutput(ns("run_status")),
          hr(),
          verbatimTextOutput(ns("manova_results"))
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'joint'", ns("total_mode")),
      fluidRow(
        box(title = t("Total (size) alone"), width = 12, status = "info",
            p(t("Univariate permutational ANOVA of the log-total with the same factors, to separate the size effect from the compositional one.")),
            fluidRow(
              column(6, verbatimTextOutput(ns("total_results"))),
              column(6, plotOutput(ns("total_plot"), height = "300px"),
                     downloadButton(ns("dl_total_plot"), t("Download figure")))
            ))
      )
    ),
    fluidRow(
      box(title = t("Pairwise comparisons"), width = 6, status = "info",
          p(t("Permutational pairwise tests between group means (Euclidean distances in ilr space, plus the log-total when included) and between group dispersions (compositional disparity). Z is the standardised effect size.")),
          uiOutput(ns("pairwise_group_ui")),
          actionButton(ns("run_pairwise"), t("Run Pairwise"), icon = icon("exchange-alt")),
          downloadButton(ns("dl_pairwise"), t("Download pairwise (CSV)")),
          uiOutput(ns("pw_status")),
          hr(),
          h5(strong(t("Differences in means"))),
          verbatimTextOutput(ns("pairwise_results")),
          h5(strong(t("Differences in dispersion"))),
          verbatimTextOutput(ns("dispersion_results"))
      ),
      box(title = t("Model diagnostics"), width = 6, status = "warning",
          p(t("Residual plots from the fitted RRPP model.")),
          plotOutput(ns("diag_resid"), height = "300px"),
          downloadButton(ns("dl_diag_resid"), t("Download figure")),
          plotOutput(ns("diag_pc"), height = "300px"),
          downloadButton(ns("dl_diag_pc"), t("Download figure")),
          hr(),
          export_settings_ui(ns, lang)
      )
    )
  )
}

#' @noRd
mod_stats_server <- function(id, df, parts, comp, keep_rows, raw = NULL,
                             restore = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    take <- restore_pending(restore, "stats")
    t    <- session_tr(session)
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
             sprintf(t(" Totals are almost constant (CV = %.1f%%): they are probably fixed by the sampling design and carry no biological information. Do not include the total."), cv * 100))
    })

    output$factors_ui <- renderUI({
      req(df(), parts())
      cands <- factor_candidates(df(), parts())
      selectInput(ns("factors"), t("Factor(s) for the model:"),
                  choices = cands, selected = take("factors") %||% head(cands, 1),
                  multiple = TRUE, width = "100%")
    })

    output$interaction_ui <- renderUI({
      req(input$factors)
      if (length(input$factors) < 2) return(NULL)
      radioButtons(ns("interaction_type"), t("Model design:"),
                   choices = tr_choices(c("Additive (main effects only)" = "+",
                                          "Factorial (with interactions)" = "*"), ui_lang(session)),
                   selected = take("design") %||% "+", inline = TRUE)
    })

    output$covariates_ui <- renderUI({
      req(df(), parts())
      cands <- covariate_candidates(df(), parts())
      if (length(cands) == 0) return(NULL)
      selectInput(ns("covariates"), t("Numeric covariate(s) (ANCOVA):"),
                  choices = cands, multiple = TRUE, width = "100%",
                  selected = take("covariates"))
    })

    output$block_ui <- renderUI({
      req(df(), parts())
      cands <- setdiff(names(df()), c(parts(), input$factors))
      selectInput(ns("block"), t("Block (permutations restricted within blocks):"),
                  choices = c(stats::setNames("", t("None")), cands), selected = take("block") %||% "",
                  width = "100%")
    })

    model_spec_rhs <- reactive({
      req(input$factors)
      design <- if (length(input$factors) >= 2) input$interaction_type %||% "+" else "+"
      model_rhs(input$factors, design, input$covariates %||% character(0),
                log_total_cov = total_mode() == "covariate")
    })

    output$formula_preview <- renderPrint({
      req(input$factors)
      cat(t("Formula:"), " y ~ ", model_spec_rhs(), "\n",
          switch(total_mode(),
            joint = sprintf(t("(y = ilr coordinates of the imputed composition + log-total [%s])"),
                            input$total_type),
            covariate = sprintf(t("(y = ilr coordinates; log-total [%s] as covariate)"),
                                input$total_type),
            t("(y = ilr coordinates of the imputed composition)")),
          if (nzchar(input$block %||% "")) sprintf(t("\nBlock: %s"), input$block),
          sep = "")
    })

    output$pairwise_group_ui <- renderUI({
      req(input$factors)
      selectInput(ns("pw_group"), t("Grouping factor for pairwise tests:"),
                  choices = input$factors, selected = take("pw_group") %||% input$factors[1])
    })

    # -- Model fitting (background worker when available) ----------------------
    build_spec <- function() {
      kr   <- keep_rows()
      cols <- unique(c(input$factors, input$covariates,
                       if (nzchar(input$block %||% "")) input$block))
      d <- df()[kr, cols, drop = FALSE]
      d[] <- lapply(names(d), \(v) if (v %in% input$covariates) d[[v]] else as.factor(d[[v]]))
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

    run_fit <- function() {
      req(comp(), keep_rows(), input$factors)
      spec <- build_spec()
      rv$pairwise <- NULL
      if (use_async()) {
        fit_task$invoke(spec)
      } else {
        withProgress(message = t("Running permutations..."), {
          rv$fit <- fit_models(spec)
        })
      }
    }
    observeEvent(input$run_manova, run_fit())

    observeEvent(fit_task$status(), {
      if (fit_task$status() == "success") rv$fit <- fit_task$result()
      if (fit_task$status() == "error") {
        showNotification(paste(t("Model failed:"), conditionMessage(
          tryCatch(fit_task$result(), error = identity))), type = "error", duration = 10)
      }
    })

    status_badge <- function(task) {
      renderUI({
        if (use_async() && task$status() == "running") {
          tags$p(style = "color: #31708f; margin-top: 8px;",
                 icon("spinner", class = "fa-spin"),
                 sprintf(t(" Running %s permutations in the background..."), input$iter))
        }
      })
    }
    output$run_status <- status_badge(fit_task)
    output$pw_status  <- status_badge(pw_task)

    output$manova_results <- renderPrint({
      req(rv$fit)
      cat(t("Fitted formula:"), "y ~", rv$fit$rhs, "\n\n")
      print(rv$fit$anova)
    })

    run_pw <- function() {
      req(rv$fit, input$pw_group, keep_rows())
      arg <- list(model  = rv$fit$model,
                  groups = as.factor(df()[[input$pw_group]][keep_rows()]))
      if (use_async()) {
        pw_task$invoke(arg)
      } else {
        withProgress(message = t("Running pairwise tests..."), {
          rv$pairwise <- fit_pairwise(arg)
        })
      }
    }
    observeEvent(input$run_pairwise, run_pw())

    observeEvent(pw_task$status(), {
      if (pw_task$status() == "success") rv$pairwise <- pw_task$result()
    })

    output$total_results <- renderPrint({
      req(rv$fit$total_anova)
      cat("log-total ~", rv$fit$rhs, "\n\n")
      print(rv$fit$total_anova)
    })

    total_plot_build <- function() {
      req(rv$fit$total_df)
      ggplot2::ggplot(rv$fit$total_df, ggplot2::aes(x = .data$group, y = .data$log_total)) +
        ggplot2::geom_boxplot(outlier.shape = NA, width = 0.5) +
        ggplot2::geom_jitter(width = 0.1, height = 0, alpha = 0.6) +
        ggplot2::labs(x = input$factors[1], y = "log-total") +
        ggplot2::theme_minimal(base_size = 14)
    }
    output$total_plot    <- renderPlot(total_plot_build())
    output$dl_total_plot <- plot_download(input, "log_total", total_plot_build)

    output$pairwise_results <- renderPrint({
      req(rv$pairwise)
      rv$pairwise$dist
    })

    output$dispersion_results <- renderPrint({
      req(rv$pairwise)
      rv$pairwise$var
    })

    diag_resid_draw <- function() plot(rv$fit$model, type = "diagnostics")
    diag_pc_draw    <- function() plot(rv$fit$model, type = "PC", pch = 19)
    output$diag_resid <- renderPlot({ req(rv$fit); diag_resid_draw() })
    output$diag_pc    <- renderPlot({ req(rv$fit); diag_pc_draw() })
    output$dl_diag_resid <- plot_download(input, "diagnostics", function() { req(rv$fit); diag_resid_draw })
    output$dl_diag_pc    <- plot_download(input, "fitted_pcs", function() { req(rv$fit); diag_pc_draw })

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

    observeEvent(restore(), {
      st <- restore()$stats
      updateRadioButtons(session, "ss_type", selected = st$ss_type)
      updateSelectInput(session, "iter", selected = st$iter)
      updateRadioButtons(session, "total_mode", selected = st$total_mode)
      updateRadioButtons(session, "total_type", selected = st$total_type)
      rv$fit <- NULL
      rv$pairwise <- NULL
      if (isTRUE(st$fitted) && length(st$factors)) pending_fit(st)
    })

    # Restore: refit once every restored input has come back from the browser
    pending_fit <- reactiveVal(NULL)
    pending_pw  <- reactiveVal(NULL)
    same <- function(a, b) identical(as.character(a %||% character(0)),
                                     as.character(b %||% character(0)))
    observe({
      st <- pending_fit()
      req(st, comp(), keep_rows())
      req(same(input$factors, st$factors), same(input$covariates, st$covariates),
          same(input$block, st$block %||% ""), same(input$iter, st$iter),
          same(input$ss_type, st$ss_type), same(total_mode(), st$total_mode),
          length(st$factors) < 2 || same(input$interaction_type, st$design))
      pending_fit(NULL)
      run_fit()
      if (isTRUE(st$pairwise_run)) pending_pw(st$pw_group)
    })
    observe({
      g <- pending_pw()
      req(g, rv$fit, same(input$pw_group, g))
      pending_pw(NULL)
      run_pw()
    })

    state <- reactive(list(
      factors = input$factors, design = input$interaction_type,
      covariates = input$covariates, block = input$block,
      rhs = if (length(input$factors)) model_spec_rhs(),
      iter = input$iter, ss_type = input$ss_type, total_mode = total_mode(),
      total_type = input$total_type, pw_group = input$pw_group,
      fitted = !is.null(rv$fit), pairwise_run = !is.null(rv$pairwise),
      fit = rv$fit, pairwise = rv$pairwise))


    # Inputs built by renderUI must exist even in hidden tabs (session restore)
    outputOptions(output, "factors_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "interaction_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "covariates_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "block_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "pairwise_group_ui", suspendWhenHidden = FALSE)
    list(fit = reactive(rv$fit), pairwise = reactive(rv$pairwise), state = state)
  })
}
