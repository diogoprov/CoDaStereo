# Simulated dataset mirroring the built-in example (mod_data.R)
sim_data <- function(groups = c("Control", "Impacted")) {
  set.seed(42)
  n <- 20
  k <- length(groups)
  raw <- data.frame(
    Hepatocytes       = stats::rnorm(n * k, 80, 1.5),
    Sinusoids         = stats::rnorm(n * k, 10, 1.0),
    Stroma            = stats::rnorm(n * k,  8, 0.8),
    Melanomacrophages = stats::rnorm(n * k, 2.5, 0.5)
  )
  data.frame(Group = factor(rep(groups, each = n)), 100 * raw / rowSums(raw))
}

test_that("every palette returns n valid colours, also beyond 8", {
  for (pal in plot_palettes) {
    for (n in c(1, 2, 8, 12)) {
      cols <- palette_colours(pal, n)
      expect_length(cols, n)
      expect_false(anyNA(cols))
      expect_silent(grDevices::col2rgb(cols))
    }
  }
  expect_error(palette_colours("nope", 3), "Unknown palette")
})

test_that("ternary plot uses the selected palette", {
  df <- sim_data()
  parts <- c("Hepatocytes", "Sinusoids", "Stroma")
  for (pal in plot_palettes) {
    p  <- plot_ternary_manual(df, parts, "Group", palette = pal)
    pts <- ggplot2::layer_data(p, 4)  # geom_point layer
    expect_setequal(unique(pts$colour), palette_colours(pal, 2))
  }
})

test_that("analysis module renders all plots for each palette", {
  df    <- sim_data(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"))
  part_names <- setdiff(names(df), "Group")
  comp  <- compositions::acomp(df[, part_names])
  shiny::testServer(
    mod_analysis_server,
    args = list(df = shiny::reactive(df), parts = shiny::reactive(part_names),
                comp = shiny::reactive(comp),
                keep_rows = shiny::reactive(seq_len(nrow(df)))),
    {
      for (pal in plot_palettes) {
        session$setInputs(group_var = "Group", palette = pal,
                          tern_mode = "direct", tern_vars = part_names[1:3],
                          tern_regions = "confidence", tern_centre = TRUE,
                          bp_x = "1", bp_y = "2", bp_shape = "", bp_labels = FALSE,
                          interactive = FALSE)
        expect_s3_class(ternary_build(FALSE), "ggplot")
        expect_s3_class(biplot_build(TRUE), "ggplot")
        expect_no_error(output$ternary_plot)
        expect_no_error(output$biplot)
        expect_no_error(output$biplot_i)
        expect_no_error(output$variation_plot)
        expect_no_error(output$scree)
        expect_no_error(output$center_barplot)
        expect_no_error(output$totvar_barplot)
      }
    }
  )
})
