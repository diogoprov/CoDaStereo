# ==============================================================================
# Module: Zero and NA Imputation
# ==============================================================================

#' @noRd
mod_imputation_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "zeros",
    fluidRow(
      box(title = "Zero-replacement method", width = 6,
          p("Compositional data may carry different kinds of zeros:"),
          tags$ul(
            tags$li(strong("CZM"), ": Bayesian-Multiplicative replacement for ",
                    em("count zeros"), " (multinomial sampling). Use with point counts."),
            tags$li(strong("multRepl"), ": multiplicative replacement for ",
                    em("rounded zeros"), " (below detection limit). Use with percentages."),
            tags$li(strong("lrEM"), ": log-ratio EM algorithm, also for ",
                    em("rounded zeros"), ". More efficient for moderate to large n.")
          ),
          radioButtons(ns("zero_method"), "Choose method:",
                       choices = c("CZM (count zeros)"        = "CZM",
                                   "multRepl (rounded zeros)" = "multRepl",
                                   "lrEM (rounded zeros)"     = "lrEM"),
                       selected = "multRepl"),
          hr(),
          h4("Missing values (NA) handling"),
          p("If selected parts contain ", code("NA"),
            " values, choose how to deal with them ",
            em("before"), " zero imputation."),
          radioButtons(ns("na_handling"), NULL,
                       choices = c(
                         "Drop rows containing NAs (listwise deletion)" = "drop",
                         "Replace NAs with column geometric mean"       = "geomean"
                       ), selected = "drop"),
          hr(),
          actionButton(ns("run"), "Run Imputation", icon = icon("magic")),
          hr(),
          verbatimTextOutput(ns("report"))
      )
    )
  )
}

#' @noRd
mod_imputation_server <- function(id, df, parts) {
  moduleServer(id, function(input, output, session) {

    rv <- reactiveValues(comp = NULL, imputed = NULL,
                         keep_rows = NULL, method_used = NULL,
                         na_action = NULL)

    observeEvent(input$run, {
      req(df(), input$zero_method, input$na_handling, parts())

      mat       <- as.matrix(df()[, parts(), drop = FALSE])
      keep_rows <- rep(TRUE, nrow(mat))
      na_action <- "no NAs detected"

      # ---- NA handling ----
      if (anyNA(mat)) {
        n_na_rows  <- sum(!complete.cases(mat))
        n_na_cells <- sum(is.na(mat))

        if (input$na_handling == "drop") {
          keep_rows <- complete.cases(mat)
          mat <- mat[keep_rows, , drop = FALSE]
          na_action <- sprintf(
            "%d row(s) (%d NA cell(s)) dropped via listwise deletion",
            n_na_rows, n_na_cells)
          showNotification(na_action, type = "warning", duration = 6)

        } else {
          mat <- gmean_impute(mat)
          if (anyNA(mat)) {
            keep_rows <- complete.cases(mat)
            mat <- mat[keep_rows, , drop = FALSE]
            na_action <- sprintf(
              "%d NA cell(s) partially imputed; %d residual row(s) dropped",
              n_na_cells, sum(!keep_rows))
          } else {
            na_action <- sprintf(
              "%d NA cell(s) imputed via column geometric mean", n_na_cells)
          }
          showNotification(na_action, type = "message", duration = 6)
        }
      }

      rv$keep_rows <- keep_rows
      rv$na_action <- na_action

      # ---- Zero handling ----
      if (!any(mat == 0, na.rm = TRUE)) {
        showNotification("No zeros detected. Using raw composition.",
                         type = "message", duration = 6)
        rv$imputed     <- mat
        rv$comp        <- compositions::acomp(mat)
        rv$method_used <- "none (no zeros)"
        return()
      }

      rv$imputed <- switch(
        input$zero_method,
        "CZM" = zCompositions::cmultRepl(
          mat, method = "CZM", output = "p-counts",
          label = 0, z.warning = 0.99),
        "multRepl" = {
          dl <- compute_dl(mat, frac = 0.5)
          zCompositions::multRepl(mat, label = 0, dl = dl$mat,
                                  z.warning = 0.99)
        },
        "lrEM" = {
          dl <- compute_dl(mat, frac = 0.5)
          zCompositions::lrEM(mat, label = 0, dl = dl$mat,
                              ini.cov = "multRepl", z.warning = 0.99)
        }
      )
      rv$comp        <- compositions::acomp(rv$imputed)
      rv$method_used <- input$zero_method
    })

    output$report <- renderPrint({
      req(rv$comp)
      cat("Zero method:    ", rv$method_used, "\n")
      cat("NA handling:    ", rv$na_action, "\n")
      cat("Rows analysed:  ", sum(rv$keep_rows), "of",
          length(rv$keep_rows), "\n")
      cat("Parts analysed: ", paste(parts(), collapse = ", "), "\n\n")
      cat("First five imputed rows (closed composition):\n")
      print(round(rv$comp[1:min(5, nrow(rv$comp)), ], 4))
    })

    # -- Return reactives ------------------------------------------------------
    list(
      comp       = reactive(rv$comp),
      imputed    = reactive(rv$imputed),
      keep_rows  = reactive(rv$keep_rows),
      method_used = reactive(rv$method_used)
    )
  })
}
