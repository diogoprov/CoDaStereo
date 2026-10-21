# ==============================================================================
# Module: Compositional Analysis
# (Ternary + Biplot + Variation matrix + Compositional Descriptives + Downloads)
# ==============================================================================

#' @noRd
mod_analysis_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "analysis",
    fluidRow(
      box(title = "Plot settings", width = 12, status = "primary",
          fluidRow(
            column(3, uiOutput(ns("group_var_ui"))),
            column(3, selectInput(ns("palette"), "Colour palette:", choices = plot_palettes)),
            column(3, checkboxInput(ns("interactive"),
                                    "Interactive plots (hover shows the sample)", FALSE)),
            column(3, NULL)
          ),
          export_settings_ui(ns)
      )
    ),
    fluidRow(
      box(title = "Ternary Plot", width = 6,
          radioButtons(ns("tern_mode"), "Ternary mode:",
                       choices = c("Select 3 parts" = "direct",
                                   "Amalgamate into 3 groups" = "amalg"),
                       selected = "direct", inline = TRUE),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'direct'", ns("tern_mode")),
            uiOutput(ns("select_ternary_ui"))
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'amalg'", ns("tern_mode")),
            fluidRow(
              column(4, textInput(ns("amalg_name1"), "Group 1:", value = "Group_A"),
                     uiOutput(ns("amalg_parts1_ui"))),
              column(4, textInput(ns("amalg_name2"), "Group 2:", value = "Group_B"),
                     uiOutput(ns("amalg_parts2_ui"))),
              column(4, textInput(ns("amalg_name3"), "Group 3:", value = "Group_C"),
                     uiOutput(ns("amalg_parts3_ui")))
            )
          ),
          fluidRow(
            column(8, radioButtons(ns("tern_regions"), "Group regions:",
                                   choices = c("Convex hull" = "hull",
                                               "95% confidence (mean)" = "confidence",
                                               "95% predictive" = "predictive",
                                               "None" = "none"),
                                   selected = "hull", inline = TRUE)),
            column(4, checkboxInput(ns("tern_centre"), "Show group centres", FALSE))
          ),
          interactive_plot_ui(ns, "ternary_plot"),
          downloadButton(ns("dl_ternary"), "Download figure")
      ),
      box(title = "Compositional PCA (clr-Biplot)", width = 6,
          fluidRow(
            column(3, selectInput(ns("bp_x"), "X axis:", choices = 1:2, selected = 1)),
            column(3, selectInput(ns("bp_y"), "Y axis:", choices = 1:2, selected = 2)),
            column(6, uiOutput(ns("bp_shape_ui")))
          ),
          fluidRow(
            column(6, checkboxInput(ns("bp_labels"), "Label samples", FALSE)),
            column(6, uiOutput(ns("bp_total_ui")))
          ),
          interactive_plot_ui(ns, "biplot"),
          downloadButton(ns("dl_biplot"), "Download figure"),
          tags$details(
            style = "margin-top: 10px;",
            tags$summary(strong("How to read this biplot")),
            tags$ul(
              tags$li(strong("Rays"), " (arrows) are the parts, as clr ",
                      "variables. Ray length is proportional to the clr ",
                      "standard deviation: longer rays are parts whose share ",
                      "varies more relative to the geometric mean."),
              tags$li(strong("Links"), " are the (imaginary) segments joining ",
                      "two ray tips. Link length is proportional to the ",
                      "standard deviation of the log-ratio of those two parts. ",
                      "A short link means the two parts stay nearly ",
                      "proportional across samples."),
              tags$li("The cosine of the angle between two links approximates ",
                      "the correlation between the two log-ratios; links at ",
                      "right angles indicate uncorrelated log-ratios."),
              tags$li("Do ", em("not"), " read the angle between two rays as a ",
                      "correlation between the parts themselves."),
              tags$li(strong("Points"), " are samples; points lying toward a ",
                      "ray tend to have relatively more of that part. Polygons ",
                      "are the convex hulls of each group."),
              tags$li("All of this is a planar approximation: rely on it only ",
                      "when the two axes explain a large share of the total ",
                      "variance (shown in the axis titles and the scree plot).")
            )
          ),
          tags$details(
            style = "margin-top: 6px;",
            tags$summary(strong("Scree plot and ray lengths")),
            fluidRow(
              column(6, plotOutput(ns("scree"), height = "220px")),
              column(6, tableOutput(ns("ray_table")))
            )
          )
      )
    ),
    fluidRow(
      box(title = "Variation matrix", width = 6, status = "info",
          p("Variance of the log-ratio of each pair of parts. Values near zero ",
            "mean the two parts stay proportional; high values mark the ",
            "trade-offs that drive compositional change. These are the squared ",
            "link lengths of the biplot, in full dimension."),
          plotOutput(ns("variation_plot"), height = "380px"),
          downloadButton(ns("dl_variation"), "Download figure"),
          downloadButton(ns("dl_variation_csv"), "Download matrix (CSV)")
      ),
      box(title = "Compositional center (geometric mean) by group", width = 6,
          status = "info",
          p("Stacked bars showing the closed geometric mean composition per group.",
            " Unlike arithmetic means, the compositional center respects the simplex geometry."),
          plotOutput(ns("center_barplot"), height = "350px"),
          downloadButton(ns("dl_center"), "Download figure")
      )
    ),
    fluidRow(
      box(title = "Total variance by group", width = 6, status = "info",
          p("Total metric variance (mvar) of the composition per group.",
            " Higher values indicate greater internal dispersion in the simplex."),
          tableOutput(ns("totvar_table")),
          plotOutput(ns("totvar_barplot"), height = "250px"),
          downloadButton(ns("dl_totvar"), "Download figure")
      ),
      box(title = "Download imputed data", width = 6,
          downloadButton(ns("dl_imputed"), "Download imputed data (CSV)"))
    )
  )
}

#' Static plot or ggiraph widget, switched by the "interactive" checkbox
#' @noRd
interactive_plot_ui <- function(ns, id, height = "400px") {
  tagList(
    conditionalPanel(sprintf("!input['%s']", ns("interactive")),
                     plotOutput(ns(id), height = height)),
    conditionalPanel(sprintf("input['%s']", ns("interactive")),
                     ggiraph::girafeOutput(ns(paste0(id, "_i")), height = height))
  )
}

#' @noRd
mod_analysis_server <- function(id, df, parts, comp, keep_rows, raw = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$group_var_ui <- renderUI({
      req(df(), parts())
      cands <- factor_candidates(df(), parts())
      selectInput(ns("group_var"), "Colour / group by:", choices = cands)
    })

    output$select_ternary_ui <- renderUI({
      req(parts())
      selectInput(ns("tern_vars"), "Select 3 Parts:",
                  choices = parts(), multiple = TRUE, selected = head(parts(), 3))
    })

    output$amalg_parts1_ui <- renderUI({
      req(parts())
      selectInput(ns("amalg_p1"), "Parts:", choices = parts(),
                  multiple = TRUE, selected = parts()[1])
    })
    output$amalg_parts2_ui <- renderUI({
      req(parts())
      selectInput(ns("amalg_p2"), "Parts:", choices = parts(),
                  multiple = TRUE,
                  selected = if (length(parts()) >= 2) parts()[2] else NULL)
    })
    output$amalg_parts3_ui <- renderUI({
      req(parts())
      selectInput(ns("amalg_p3"), "Parts:", choices = parts(),
                  multiple = TRUE,
                  selected = if (length(parts()) >= 3) parts()[3] else NULL)
    })

    amalg_typed <- reactive({
      if (input$tern_mode != "amalg") return(NULL)
      req(input$amalg_p1, input$amalg_p2, input$amalg_p3,
          input$amalg_name1, input$amalg_name2, input$amalg_name3)
      stats::setNames(
        list(input$amalg_p1, input$amalg_p2, input$amalg_p3),
        c(input$amalg_name1, input$amalg_name2, input$amalg_name3)
      )
    })
    amalg_list <- debounce(amalg_typed, 500)

    group_vec <- reactive({
      req(input$group_var, keep_rows())
      as.factor(df()[[input$group_var]][keep_rows()])
    })

    # -- Ternary ---------------------------------------------------------------
    ternary_build <- function(interactive) {
      req(df(), input$group_var)
      region_data <- if (!is.null(comp()) && !is.null(keep_rows())) {
        rd <- as.data.frame(unclass(comp()))
        rd[[input$group_var]] <- df()[[input$group_var]][keep_rows()]
        rd
      }
      common <- list(data = df(), group_var = input$group_var, palette = input$palette,
                     regions = input$tern_regions, centre = isTRUE(input$tern_centre),
                     ids = sample_ids(df()), interactive = interactive,
                     region_data = region_data)
      if (input$tern_mode == "direct") {
        req(input$tern_vars, length(input$tern_vars) == 3)
        do.call(plot_ternary_manual, c(common, list(parts = input$tern_vars)))
      } else {
        req(amalg_list())
        do.call(plot_ternary_manual, c(common, list(parts = NULL, amalg = amalg_list())))
      }
    }
    ternary_keys <- function() {
      list(df(), comp(), input$group_var, input$palette, input$tern_mode, input$tern_vars,
           amalg_list(), input$tern_regions, input$tern_centre)
    }
    output$ternary_plot   <- renderPlot(ternary_build(FALSE)) |> bindCache(ternary_keys())
    output$ternary_plot_i <- ggiraph::renderGirafe(ggiraph::girafe(ggobj = ternary_build(TRUE)))
    output$dl_ternary <- plot_download(input, "ternary", function() ternary_build(FALSE))

    # -- Biplot ----------------------------------------------------------------
    output$bp_shape_ui <- renderUI({
      req(df(), parts())
      cands <- setdiff(factor_candidates(df(), parts()), input$group_var)
      selectInput(ns("bp_shape"), "Shape by:", choices = c("None" = "", cands))
    })

    output$bp_total_ui <- renderUI({
      if (is.null(raw)) return(NULL)
      selectInput(ns("bp_total"), "Add log-total (T-space biplot):",
                  choices = c("No" = "none", "Sum of parts (Ts)" = "sum",
                              "Product total (Tp)" = "product"))
    })

    pca <- reactive({
      req(comp())
      lt <- if (!is.null(raw) && (input$bp_total %||% "none") != "none") {
        log_total_coord(raw(), comp(), input$bp_total)
      }
      clr_pca(comp(), log_total = lt)
    }) |> bindCache(comp(), input$bp_total)

    observeEvent(pca(), {
      n_axes <- sum(pca()$eig[, 1] > 1e-10 * sum(pca()$eig[, 1]))
      updateSelectInput(session, "bp_x", choices = seq_len(n_axes),
                        selected = min(as.integer(input$bp_x %||% 1), n_axes))
      updateSelectInput(session, "bp_y", choices = seq_len(n_axes),
                        selected = min(as.integer(input$bp_y %||% 2), n_axes))
    })

    biplot_build <- function(interactive) {
      req(group_vec(), input$bp_x, input$bp_y)
      validate(need(input$bp_x != input$bp_y, "Choose two different axes."))
      kr <- keep_rows()
      plot_clr_biplot(pca(), group_vec(), palette = input$palette,
                      axes = as.integer(c(input$bp_x, input$bp_y)),
                      shape = if (nzchar(input$bp_shape %||% "")) df()[[input$bp_shape]][kr],
                      ids = sample_ids(df())[kr], show_labels = isTRUE(input$bp_labels),
                      interactive = interactive)
    }
    biplot_keys <- function() {
      list(comp(), keep_rows(), input$group_var, input$palette, input$bp_x, input$bp_y,
           input$bp_shape, input$bp_labels, input$bp_total)
    }
    output$biplot   <- renderPlot(biplot_build(FALSE)) |> bindCache(biplot_keys())
    output$biplot_i <- ggiraph::renderGirafe(ggiraph::girafe(ggobj = biplot_build(TRUE)))
    output$dl_biplot <- plot_download(input, "clr_biplot", function() biplot_build(FALSE))

    output$scree     <- renderPlot(plot_scree(pca(), base_size = 12)) |> bindCache(pca())
    output$ray_table <- renderTable(ray_table(pca()), digits = 3)

    # -- Variation matrix ------------------------------------------------------
    output$variation_plot <- renderPlot({
      req(comp())
      plot_variation_matrix(comp())
    }) |> bindCache(comp())
    output$dl_variation <- plot_download(input, "variation_matrix",
                                         function() plot_variation_matrix(comp()))
    output$dl_variation_csv <- downloadHandler(
      filename = function() paste0("variation_matrix_", Sys.Date(), ".csv"),
      content  = function(file) {
        vm <- compositions::variation(comp(), robust = FALSE)
        dimnames(vm) <- list(colnames(comp()), colnames(comp()))
        utils::write.csv(vm, file)
      }
    )

    # -- Compositional Descriptives -------------------------------------------
    center_build <- function() {
      req(comp(), group_vec())
      center_df <- comp_center_by_group(comp(), group_vec())
      ggplot2::ggplot(center_df,
                      ggplot2::aes(x = .data$Group, y = .data$Proportion,
                                   fill = .data$Part)) +
        ggplot2::geom_col(position = "stack", width = 0.7) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_palette(input$palette, length(unique(center_df$Part))) +
        ggplot2::labs(x = NULL, y = "Compositional center (closed %)", fill = "Part") +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(legend.position = "bottom")
    }
    output$center_barplot <- renderPlot(center_build()) |>
      bindCache(comp(), keep_rows(), input$group_var, input$palette)
    output$dl_center <- plot_download(input, "compositional_center", center_build)

    output$totvar_table <- renderTable({
      req(comp(), group_vec())
      comp_totvar_by_group(comp(), group_vec())
    }, digits = 4)

    totvar_build <- function() {
      req(comp(), group_vec())
      tv_df <- comp_totvar_by_group(comp(), group_vec())
      ggplot2::ggplot(tv_df, ggplot2::aes(x = .data$Group, y = .data$Total_Variance,
                                           fill = .data$Group)) +
        ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
        scale_palette(input$palette, nrow(tv_df)) +
        ggplot2::labs(x = NULL, y = "Total metric variance (mvar)") +
        ggplot2::theme_minimal(base_size = 14)
    }
    output$totvar_barplot <- renderPlot(totvar_build()) |>
      bindCache(comp(), keep_rows(), input$group_var, input$palette)
    output$dl_totvar <- plot_download(input, "total_variance", totvar_build)

    # -- Download imputed data -------------------------------------------------
    output$dl_imputed <- downloadHandler(
      filename = function() paste0("imputed_data_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(comp(), keep_rows())
        out <- as.data.frame(unclass(comp()))
        non_part <- setdiff(names(df()), parts())
        if (length(non_part) > 0) out <- cbind(df()[keep_rows(), non_part, drop = FALSE], out)
        utils::write.csv(out, file, row.names = FALSE)
      }
    )

    list(inputs = reactive(list(group_var = input$group_var, palette = input$palette,
                                tern_vars = input$tern_vars, tern_mode = input$tern_mode,
                                amalg = amalg_list(), tern_regions = input$tern_regions,
                                bp_axes = c(input$bp_x, input$bp_y),
                                bp_total = input$bp_total %||% "none")),
         pca = pca)
  })
}
