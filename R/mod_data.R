# ==============================================================================
# Module: Data Upload + Parts Selector + Pre-flight Check + Data Inspection
# ==============================================================================

#' @noRd
mod_data_upload_ui <- function(id, lang = "en") {
  ns <- NS(id)
  t  <- function(x) tr(x, lang)
  tabItem(tabName = "data",
    fluidRow(
      box(title = t("Data Input"), width = 4,
          fileInput(ns("file1"), t("Choose CSV or Excel File"),
                    accept = c(".csv", ".xlsx", ".xls"),
                    buttonLabel = t("Browse..."), placeholder = t("No file selected")),
          checkboxInput(ns("header"), t("First row is header"), TRUE),
          radioButtons(ns("sep"), t("CSV separator (ignored for Excel)"),
                       choices = tr_choices(c(Comma = ",", Semicolon = ";", Tab = "\t"), lang),
                       selected = ","),
          uiOutput(ns("sheet_selector_ui")),
          hr(),
          h4(t("Or try it out:")),
          selectInput(ns("example"), t("Example dataset:"),
                      choices = tr_choices(c(
                        "Simulated liver (2 groups)"                       = "liver_sim",
                        "Spleen, point counts per field (case study 1)"    = "spleen",
                        "Kidney, point counts per field (case study 2)"    = "kidney",
                        "Leukocyte differential counts (case study 3)"     = "leukocytes",
                        "Erythrocyte nuclear abnormalities (case study 4)" = "nuclear"), lang)),
          actionButton(ns("load_example"), t("Load example"),
                       icon = icon("table"), class = "btn-primary")
      ),
      box(title = t("Data Preview"), width = 8, tableOutput(ns("contents")))
    ),
    fluidRow(
      box(title = t("Specimens (nested designs)"), width = 12, status = "info",
          p(t("When rows are fields of view (or sections) nested within specimens, analyse one composition per specimen: fields of the same animal are not independent replicates. Point counts are summed; areas or proportions are averaged.")),
          fluidRow(
            column(3, checkboxInput(ns("agg_on"), t("Aggregate rows by specimen"), FALSE)),
            column(4, uiOutput(ns("agg_id_ui"))),
            column(5, radioButtons(ns("agg_fun"), t("Combine parts by:"),
                                   choices = tr_choices(c("Sum (counts)" = "sum",
                                                          "Mean (areas, proportions)" = "mean"), lang),
                                   inline = TRUE))
          ),
          uiOutput(ns("agg_report"))
      )
    ),
    fluidRow(
      box(title = t("Compositional parts"), width = 8, status = "primary",
          p(HTML(t("<strong>Select which numeric columns are the compositional parts.</strong> Columns whose name looks like an ID are deselected by default; non-compositional covariates (body weight, biochemical assays on a different scale, etc.) should also be unchecked."))),
          uiOutput(ns("select_parts_ui"))
      ),
      box(title = t("Pre-flight check"), width = 4, status = "warning",
          p(t("Automatic diagnostics for the selected parts:")),
          uiOutput(ns("preflight_ui"))
      )
    )
  )
}

#' @noRd
mod_data_inspect_ui <- function(id, lang = "en") {
  ns <- NS(id)
  t  <- function(x) tr(x, lang)
  tabItem(tabName = "inspect",
    fluidRow(
      box(title = t("Pattern of zeros and missing values"), width = 12,
          status = "primary",
          p(HTML(t("Each column is a sample, each row a compositional part. Cells are coloured according to whether the value was <span style='color:#7570B3; font-weight:bold;'>observed</span>, a <span style='color:#D95F02; font-weight:bold;'>structural zero</span> or <span style='color:#1B9E77; font-weight:bold;'>missing (NA)</span>."))),
          plotOutput(ns("zero_na_heatmap"), height = "260px")
      )
    ),
    fluidRow(
      box(title = t("Missing values (naniar::vis_miss)"), width = 6,
          plotOutput(ns("vis_miss_plot"), height = "320px")),
      box(title = t("Counts per part"), width = 6,
          plotOutput(ns("count_plot"), height = "320px"))
    )
  )
}

#' @noRd
mod_data_server <- function(id, restore = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    t  <- session_tr(session)
    rv <- reactiveValues(df = NULL, default_parts = NULL, source = NULL)

    uploaded_ext <- reactive({
      req(input$file1)
      tolower(tools::file_ext(input$file1$name))
    })

    output$sheet_selector_ui <- renderUI({
      req(input$file1)
      if (!uploaded_ext() %in% c("xlsx", "xls")) return(NULL)
      sheets <- tryCatch(readxl::excel_sheets(input$file1$datapath),
                         error = function(e) character(0))
      if (length(sheets) == 0) return(NULL)
      selectInput(ns("excel_sheet"), t("Excel sheet:"),
                  choices = sheets, selected = sheets[1])
    })

    observeEvent(
      list(input$file1, input$header, input$sep, input$excel_sheet),
      {
        req(input$file1)
        ext <- uploaded_ext()
        rv$default_parts <- NULL
        rv$source <- list(type = if (ext == "csv") "csv" else "excel",
                          file_name = input$file1$name, header = input$header,
                          sep = input$sep, sheet = input$excel_sheet)
        rv$df <- if (ext == "csv") {
          read.csv(input$file1$datapath, header = input$header, sep = input$sep)
        } else if (ext %in% c("xlsx", "xls")) {
          sheet <- if (is.null(input$excel_sheet) || !nzchar(input$excel_sheet)) 1 else input$excel_sheet
          as.data.frame(readxl::read_excel(input$file1$datapath,
                                           sheet = sheet, col_names = input$header))
        } else {
          showNotification(paste(t("Unsupported file type:"), ext),
                           type = "error", duration = 6)
          return()
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$load_example, {
      ex <- example_datasets()[[input$example]]
      rv$default_parts <- ex$parts
      rv$df <- load_example(input$example)
      rv$source <- list(type = "example", example = input$example)
      updateCheckboxInput(session, "agg_on", value = !is.null(ex$specimen))
      rv$default_specimen <- ex$specimen
    })

    output$agg_id_ui <- renderUI({
      req(rv$df)
      cols <- names(rv$df)
      sel  <- rv$default_specimen %||% cols[1]
      selectInput(ns("agg_id"), t("Specimen column:"), choices = cols,
                  selected = if (sel %in% cols) sel else cols[1])
    })

    # Data passed downstream: one row per specimen when aggregation is on
    df_out <- reactive({
      req(rv$df)
      if (!isTRUE(input$agg_on) || is.null(input$agg_id) || is.null(input$parts)) {
        return(rv$df)
      }
      validate(need(!input$agg_id %in% input$parts,
                    t("The specimen column cannot be a compositional part.")))
      aggregate_specimens(rv$df, input$agg_id, input$parts, input$agg_fun)
    })

    output$agg_report <- renderUI({
      req(isTRUE(input$agg_on), df_out())
      dropped <- attr(df_out(), "dropped")
      tags$p(style = "color: #31708f;", icon("layer-group"),
             sprintf(t(" %d rows aggregated into %d specimens (%s)."),
                     nrow(rv$df), nrow(df_out()), t(input$agg_fun)),
             if (length(dropped) > 0) {
               sprintf(t(" Columns not constant within specimens were dropped: %s."),
                       paste(dropped, collapse = ", "))
             })
    })

    output$select_parts_ui <- renderUI({
      req(rv$df)
      num_cols    <- names(rv$df)[vapply(rv$df, is.numeric, logical(1))]
      default_sel <- if (!is.null(rv$default_parts) && all(rv$default_parts %in% num_cols)) {
        rv$default_parts
      } else {
        num_cols[!is_id_name(num_cols)]
      }
      if (length(default_sel) < 2) default_sel <- num_cols
      selectInput(ns("parts"),
                  t("Compositional parts (numeric columns to analyse):"),
                  choices = num_cols, selected = default_sel,
                  multiple = TRUE, width = "100%")
    })

    # -- Pre-flight check output -----------------------------------------------
    output$preflight_ui <- renderUI({
      req(rv$df, input$parts)
      check <- preflight_check(df_out(), input$parts, lang = ui_lang(session))
      if (check$ok) {
        tags$div(
          icon("check-circle", style = "color: #28a745; font-size: 1.3em;"),
          tags$span(style = "color: #28a745; font-weight: bold;",
                    t(" All checks passed.")),
          tags$p(style = "margin-top: 8px; color: #666;",
                 sprintf(t("%d parts selected. Row sums look consistent."),
                         length(input$parts)))
        )
      } else {
        warning_items <- lapply(check$warnings, function(w) {
          tags$li(style = "margin-bottom: 6px;", w)
        })
        tags$div(
          icon("exclamation-triangle", style = "color: #dc3545; font-size: 1.3em;"),
          tags$span(style = "color: #dc3545; font-weight: bold;",
                    t(" Issues detected:")),
          tags$ul(style = "margin-top: 8px;", warning_items),
          if (length(check$flagged) > 0) {
            tags$p(style = "color: #856404; background: #fff3cd; padding: 8px; border-radius: 4px;",
                   icon("lightbulb"),
                   sprintf(t(" Consider unchecking: %s"),
                           paste(check$flagged, collapse = ", ")))
          }
        )
      }
    })

    chosen_parts <- reactive({ req(input$parts); input$parts })

    output$contents <- renderTable({ head(df_out(), 10) })

    # -- Data inspection plots -------------------------------------------------
    parts_long <- reactive({
      req(df_out(), input$parts)
      d <- as.matrix(df_out()[, input$parts, drop = FALSE])
      value <- as.vector(t(d))  # one row per sample x part, samples in order
      data.frame(
        Sample = rep(seq_len(nrow(d)), each = ncol(d)),
        Part   = factor(rep(input$parts, times = nrow(d)), levels = input$parts),
        Value  = value,
        State  = ifelse(is.na(value), "Missing (NA)",
                        ifelse(value == 0, "Structural zero", "Observed"))
      )
    })

    output$zero_na_heatmap <- renderPlot({
      ggplot2::ggplot(parts_long(),
                      ggplot2::aes(x = .data$Sample, y = .data$Part, fill = .data$State)) +
        ggplot2::geom_tile(color = NA) +
        ggplot2::scale_fill_manual(values = c("Observed" = "#7570B3",
                                               "Structural zero" = "#D95F02",
                                               "Missing (NA)" = "#1B9E77")) +
        ggplot2::scale_y_discrete(limits = rev) +
        ggplot2::labs(x = "Sample (row index)", y = NULL, fill = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "bottom", panel.grid = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(size = 7))
    })

    output$vis_miss_plot <- renderPlot({
      req(df_out(), input$parts)
      naniar::vis_miss(df_out()[, input$parts, drop = FALSE]) +
        ggplot2::theme(legend.position = "bottom",
                       axis.text.x = ggplot2::element_text(angle = 45, hjust = 0))
    })

    output$count_plot <- renderPlot({
      df_long <- parts_long()
      miss <- df_long[df_long$State != "Observed", ]
      # every part x state, zero counts included
      df_summary <- as.data.frame(
        table(Part  = miss$Part,
              State = factor(miss$State, levels = c("Missing (NA)", "Structural zero"))),
        responseName = "n")
      ggplot2::ggplot(df_summary,
                      ggplot2::aes(x = .data$n, y = .data$Part, fill = .data$State)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
        ggplot2::scale_fill_manual(values = c("Structural zero" = "#D95F02",
                                               "Missing (NA)" = "#1B9E77")) +
        ggplot2::scale_y_discrete(limits = rev) +
        ggplot2::labs(x = "Count", y = NULL, fill = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "bottom")
    })

    # -- Restore a saved session ------------------------------------------------
    observeEvent(restore(), {
      st <- restore()$data
      rv$default_parts    <- st$parts
      rv$default_specimen <- st$agg$id
      rv$source <- st$source
      rv$df     <- st$df
      updateCheckboxInput(session, "agg_on", value = isTRUE(st$agg$on))
      updateRadioButtons(session, "agg_fun", selected = st$agg$fun %||% "sum")
      if (identical(st$source$type, "example")) {
        updateSelectInput(session, "example", selected = st$source$example)
      }
    })

    state <- reactive({
      req(rv$df)
      out <- df_out()
      list(df = rv$df, df_out = out, parts = input$parts, source = rv$source,
           agg = list(on = isTRUE(input$agg_on), id = input$agg_id, fun = input$agg_fun,
                      keep = setdiff(names(out), c(input$agg_id, input$parts, "n_rows"))))
    })


    # Inputs built by renderUI must exist even in hidden tabs (session restore)
    outputOptions(output, "select_parts_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "agg_id_ui", suspendWhenHidden = FALSE)
    list(df = df_out, parts = reactive(input$parts), state = state)
  })
}
