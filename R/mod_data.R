# ==============================================================================
# Module: Data Upload + Parts Selector + Data Inspection
# ==============================================================================

#' @noRd
mod_data_upload_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "data",
    fluidRow(
      box(title = "Data Input", width = 4,
          fileInput(ns("file1"), "Choose CSV or Excel File",
                    accept = c(".csv", ".xlsx", ".xls"),
                    buttonLabel = "Browse..."),
          checkboxInput(ns("header"), "First row is header", TRUE),
          radioButtons(ns("sep"), "CSV separator (ignored for Excel)",
                       choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                       selected = ","),
          uiOutput(ns("sheet_selector_ui")),
          hr(),
          h4("Or try it out:"),
          actionButton(ns("load_example"),
                       "Load Example Data (Liver Histomorphometry)",
                       icon = icon("table"), class = "btn-primary")
      ),
      box(title = "Data Preview", width = 8, tableOutput(ns("contents")))
    ),
    fluidRow(
      box(title = "Compositional parts", width = 12, status = "primary",
          p(strong("Select which numeric columns are the compositional parts."),
            " Columns whose name looks like an ID are deselected by default; ",
            "non-compositional covariates (body weight, biochemical assays on ",
            "a different scale, etc.) should also be unchecked."),
          uiOutput(ns("select_parts_ui"))
      )
    )
  )
}

#' @noRd
mod_data_inspect_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "inspect",
    fluidRow(
      box(title = "Pattern of zeros and missing values", width = 12,
          status = "primary",
          p("Each column is a sample, each row a compositional part. ",
            "Cells coloured according to whether the value was ",
            tags$span(style = "color:#7570B3; font-weight:bold;", "observed"),
            ", a ",
            tags$span(style = "color:#D95F02; font-weight:bold;",
                      "structural zero"),
            " or ",
            tags$span(style = "color:#1B9E77; font-weight:bold;",
                      "missing (NA)"), "."),
          plotOutput(ns("zero_na_heatmap"), height = "260px")
      )
    ),
    fluidRow(
      box(title = "Missing values (naniar::vis_miss)", width = 6,
          plotOutput(ns("vis_miss_plot"), height = "320px")),
      box(title = "Counts per part", width = 6,
          plotOutput(ns("count_plot"), height = "320px"))
    )
  )
}

#' @noRd
mod_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(df = NULL)

    # -- File extension helper -------------------------------------------------
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
      selectInput(ns("excel_sheet"), "Excel sheet:",
                  choices = sheets, selected = sheets[1])
    })

    # -- File reader -----------------------------------------------------------
    observeEvent(
      list(input$file1, input$header, input$sep, input$excel_sheet),
      {
        req(input$file1)
        ext <- uploaded_ext()
        rv$df <- if (ext == "csv") {
          read.csv(input$file1$datapath,
                   header = input$header, sep = input$sep)
        } else if (ext %in% c("xlsx", "xls")) {
          sheet <- if (is.null(input$excel_sheet) ||
                       !nzchar(input$excel_sheet)) 1 else input$excel_sheet
          as.data.frame(readxl::read_excel(input$file1$datapath,
                                           sheet     = sheet,
                                           col_names = input$header))
        } else {
          showNotification(paste("Unsupported file type:", ext),
                           type = "error", duration = 6)
          return()
        }
      },
      ignoreInit = TRUE
    )

    # -- Example data ----------------------------------------------------------
    observeEvent(input$load_example, {
      set.seed(42)
      n <- 20
      raw <- data.frame(
        Hepatocytes       = c(rnorm(n, 80, 1.5), rnorm(n, 65, 2.0)),
        Sinusoids         = c(rnorm(n, 10, 1.0), rnorm(n,  8, 0.9)),
        Stroma            = c(rnorm(n,  8, 0.8), rnorm(n,  6.5, 0.7)),
        Melanomacrophages = c(rnorm(n, 2.5, 0.5), rnorm(n, 20, 2.5))
      )
      raw[raw < 0] <- 0
      bdl_idx <- sample.int(n, size = round(n * 0.25))
      raw$Melanomacrophages[bdl_idx] <- 0
      closed <- 100 * raw / rowSums(raw)
      rv$df <- data.frame(
        Sample_ID = seq_len(2 * n),
        Group     = factor(rep(c("Control", "Impacted"), each = n)),
        closed
      )
    })

    # -- Parts selector --------------------------------------------------------
    output$select_parts_ui <- renderUI({
      req(rv$df)
      num_cols    <- names(rv$df)[sapply(rv$df, is.numeric)]
      default_sel <- num_cols[!is_id_name(num_cols)]
      if (length(default_sel) < 2) default_sel <- num_cols
      selectInput(ns("parts"),
                  "Compositional parts (numeric columns to analyse):",
                  choices = num_cols, selected = default_sel,
                  multiple = TRUE, width = "100%")
    })

    output$contents <- renderTable({ req(rv$df); head(rv$df, 10) })

    # -- Data inspection plots -------------------------------------------------
    parts_long <- reactive({
      req(rv$df, input$parts)
      rv$df[, input$parts, drop = FALSE] |>
        tibble::rowid_to_column("Sample") |>
        tidyr::pivot_longer(-"Sample", names_to = "Part",
                            values_to = "Value") |>
        dplyr::mutate(
          State = dplyr::case_when(
            is.na(.data$Value) ~ "Missing (NA)",
            .data$Value == 0   ~ "Structural zero",
            TRUE               ~ "Observed"
          ),
          Part = factor(.data$Part, levels = input$parts)
        )
    })

    output$zero_na_heatmap <- renderPlot({
      ggplot2::ggplot(parts_long(),
                      ggplot2::aes(x = .data$Sample, y = .data$Part,
                                   fill = .data$State)) +
        ggplot2::geom_tile(color = NA) +
        ggplot2::scale_fill_manual(values = c(
          "Observed"        = "#7570B3",
          "Structural zero" = "#D95F02",
          "Missing (NA)"    = "#1B9E77"
        )) +
        ggplot2::scale_y_discrete(limits = rev) +
        ggplot2::labs(x = "Sample (row index)", y = NULL, fill = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "bottom", panel.grid = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(size = 7))
    })

    output$vis_miss_plot <- renderPlot({
      req(rv$df, input$parts)
      naniar::vis_miss(rv$df[, input$parts, drop = FALSE]) +
        ggplot2::theme(legend.position = "bottom",
                       axis.text.x = ggplot2::element_text(angle = 45, hjust = 0))
    })

    output$count_plot <- renderPlot({
      df_long <- parts_long()
      df_summary <- df_long |>
        dplyr::filter(.data$State != "Observed") |>
        dplyr::count(.data$Part, .data$State, .drop = FALSE) |>
        tidyr::complete(.data$Part,
                        State = c("Structural zero", "Missing (NA)"),
                        fill = list(n = 0))
      ggplot2::ggplot(df_summary,
                      ggplot2::aes(x = .data$n, y = .data$Part,
                                   fill = .data$State)) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8),
                          width = 0.7) +
        ggplot2::scale_fill_manual(values = c(
          "Structural zero" = "#D95F02", "Missing (NA)" = "#1B9E77")) +
        ggplot2::scale_y_discrete(limits = rev) +
        ggplot2::labs(x = "Count", y = NULL, fill = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "bottom")
    })

    # -- Return reactives to parent server -------------------------------------
    list(
      df    = reactive(rv$df),
      parts = reactive(input$parts)
    )
  })
}
