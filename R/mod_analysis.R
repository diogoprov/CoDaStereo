# ==============================================================================
# Module: Compositional Analysis (Ternary + Biplot + Downloads)
# ==============================================================================

#' @noRd
mod_analysis_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "analysis",
    fluidRow(
      box(title = "Ternary Plot", width = 6,
          uiOutput(ns("group_var_ui")),
          hr(),
          radioButtons(ns("tern_mode"), "Ternary mode:",
                       choices = c("Select 3 parts" = "direct",
                                   "Amalgamate into 3 groups" = "amalg"),
                       selected = "direct", inline = TRUE),
          # -- Direct mode UI --
          conditionalPanel(
            condition = sprintf("input['%s'] == 'direct'", ns("tern_mode")),
            uiOutput(ns("select_ternary_ui"))
          ),
          # -- Amalgamation mode UI --
          conditionalPanel(
            condition = sprintf("input['%s'] == 'amalg'", ns("tern_mode")),
            fluidRow(
              column(4,
                textInput(ns("amalg_name1"), "Group 1:", value = "Group_A"),
                uiOutput(ns("amalg_parts1_ui"))),
              column(4,
                textInput(ns("amalg_name2"), "Group 2:", value = "Group_B"),
                uiOutput(ns("amalg_parts2_ui"))),
              column(4,
                textInput(ns("amalg_name3"), "Group 3:", value = "Group_C"),
                uiOutput(ns("amalg_parts3_ui")))
            )
          ),
          plotOutput(ns("ternary_plot")),
          downloadButton(ns("dl_ternary"), "Download PDF")
      ),
      box(title = "Compositional PCA (clr-Biplot)", width = 6,
          plotOutput(ns("biplot")),
          downloadButton(ns("dl_biplot"), "Download PDF"))
    ),
    fluidRow(
      box(title = "Download imputed data", width = 4,
          downloadButton(ns("dl_imputed"), "Download imputed data (CSV)"))
    )
  )
}

#' @noRd
mod_analysis_server <- function(id, df, parts, comp, keep_rows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -- Group variable selector -----------------------------------------------
    output$group_var_ui <- renderUI({
      req(df(), parts())
      cands <- factor_candidates(df(), parts())
      selectInput(ns("group_var"), "Colour / group by:", choices = cands)
    })

    # -- Direct ternary selector -----------------------------------------------
    output$select_ternary_ui <- renderUI({
      req(parts())
      selectInput(ns("tern_vars"), "Select 3 Parts:",
                  choices = parts(), multiple = TRUE,
                  selected = head(parts(), 3))
    })

    # -- Amalgamation selectors ------------------------------------------------
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

    # -- Build amalgamation list (or NULL) -------------------------------------
    amalg_list <- reactive({
      if (input$tern_mode != "amalg") return(NULL)
      req(input$amalg_p1, input$amalg_p2, input$amalg_p3,
          input$amalg_name1, input$amalg_name2, input$amalg_name3)
      stats::setNames(
        list(input$amalg_p1, input$amalg_p2, input$amalg_p3),
        c(input$amalg_name1, input$amalg_name2, input$amalg_name3)
      )
    })

    # -- Ternary plot ----------------------------------------------------------
    ternary_obj <- reactive({
      req(df(), input$group_var)
      if (input$tern_mode == "direct") {
        req(input$tern_vars, length(input$tern_vars) == 3)
        plot_ternary_manual(df(), input$tern_vars, input$group_var)
      } else {
        req(amalg_list())
        plot_ternary_manual(df(), parts = NULL,
                            group_var = input$group_var,
                            amalg = amalg_list())
      }
    })

    output$ternary_plot <- renderPlot({ ternary_obj() })

    output$dl_ternary <- downloadHandler(
      filename = function() paste0("ternary_", Sys.Date(), ".pdf"),
      content  = function(file) {
        ggplot2::ggsave(file, plot = ternary_obj(),
                        width = 7, height = 7, device = "pdf")
      }
    )

    # -- Biplot ----------------------------------------------------------------
    biplot_obj <- reactive({
      req(comp(), input$group_var, keep_rows())
      clr_data  <- as.data.frame(unclass(compositions::clr(comp())))
      res_pca   <- FactoMineR::PCA(clr_data, graph = FALSE)
      group_vec <- as.factor(df()[[input$group_var]][keep_rows()])
      factoextra::fviz_pca_biplot(
        res_pca, habillage = group_vec, addEllipses = TRUE,
        ellipse.type = "convex",
        title = "Compositional Ray Biplot (clr-PCA)") +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(legend.position = "bottom")
    })

    output$biplot <- renderPlot({ biplot_obj() })

    output$dl_biplot <- downloadHandler(
      filename = function() paste0("clr_biplot_", Sys.Date(), ".pdf"),
      content  = function(file) {
        ggplot2::ggsave(file, plot = biplot_obj(),
                        width = 8, height = 7, device = "pdf")
      }
    )

    # -- Download imputed data -------------------------------------------------
    output$dl_imputed <- downloadHandler(
      filename = function() paste0("imputed_data_", Sys.Date(), ".csv"),
      content  = function(file) {
        req(comp(), keep_rows())
        out <- as.data.frame(unclass(comp()))
        # Re-attach non-part columns (subset by keep_rows)
        non_part <- setdiff(names(df()), parts())
        if (length(non_part) > 0) {
          out <- cbind(df()[keep_rows(), non_part, drop = FALSE], out)
        }
        utils::write.csv(out, file, row.names = FALSE)
      }
    )
  })
}
