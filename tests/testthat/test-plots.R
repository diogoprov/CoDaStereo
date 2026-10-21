spleen_specimens <- function() {
  ex <- example_datasets()$spleen
  d  <- aggregate_specimens(load_example("spleen"), "Specimen", ex$parts)
  list(d = d, x = compositions::acomp(as.matrix(d[, ex$parts]) + 0.5), parts = ex$parts)
}

test_that("save_plot writes every format for ggplot and base graphics", {
  s <- spleen_specimens()
  p <- plot_scree(clr_pca(s$x))
  for (fmt in c("pdf", "png", "svg", "tiff")) {
    f <- tempfile(fileext = paste0(".", fmt))
    save_plot(f, p, fmt, width = 10, height = 8, dpi = 150)
    expect_gt(file.size(f), 1000)
    g <- tempfile(fileext = paste0(".", fmt))
    save_plot(g, function() plot_coda_dendrogram(s$x, sbp_auto(s$x), s$d$Site), fmt,
              width = 10, height = 8, dpi = 150)
    expect_gt(file.size(g), 1000)
  }
})

test_that("biplot rays keep relative clr SDs after rescaling", {
  s   <- spleen_specimens()
  pca <- clr_pca(s$x)
  p   <- plot_clr_biplot(pca, s$d$Site, axes = c(1, 2))
  rays <- ggplot2::layer_data(p, which(purrr::map_lgl(p$layers, \(l)
            inherits(l$geom, "GeomSegment"))))
  len_plot <- sqrt(rays$xend^2 + rays$yend^2)
  len_pca  <- sqrt(rowSums(pca$var$coord[, 1:2]^2))
  expect_equal(len_plot / len_plot[1], unname(len_pca / len_pca[1]), tolerance = 1e-8)
})

test_that("T-space PCA adds the log-total as a variable", {
  s   <- spleen_specimens()
  lt  <- log_total_coord(as.matrix(s$d[, s$parts]), s$x, "sum")
  pca <- clr_pca(s$x, log_total = lt)
  expect_true("log_total" %in% rownames(pca$var$coord))
  expect_s3_class(plot_clr_biplot(pca, s$d$Site), "ggplot")
})

test_that("ternary regions lie inside the simplex and contain the group centre", {
  set.seed(2)
  x <- cbind(stats::rlnorm(30, 2), stats::rlnorm(30, 1), stats::rlnorm(30, 0))
  for (type in c("confidence", "predictive")) {
    reg <- ternary_region(x, type)
    expect_true(all(reg > 0 & reg < 1))
    expect_equal(unname(rowSums(reg)), rep(1, nrow(reg)), tolerance = 1e-10)
  }
  conf <- ternary_region(x, "confidence")
  pred <- ternary_region(x, "predictive")
  # predictive region is wider than the confidence region of the mean
  spread <- function(r) diff(range(log(r[, 1] / r[, 2])))
  expect_gt(spread(pred), spread(conf))
  expect_null(ternary_region(x[1:3, ], "confidence"))
})

test_that("variation matrix plot and ray table are consistent", {
  s  <- spleen_specimens()
  vm <- compositions::variation(s$x, robust = FALSE)
  rt <- ray_table(clr_pca(s$x))
  # sum of clr variances = total variance = sum(variation) / (2 D)
  D <- ncol(s$x)
  expect_equal(sum(rt$clr_SD^2), sum(vm) / (2 * D), tolerance = 1e-8)
  expect_s3_class(plot_variation_matrix(s$x), "ggplot")
})

test_that("the app works without ggiraph and TIFF falls back without cairo", {
  local_mocked_bindings(has_ggiraph = function() FALSE)
  ui <- as.character(app_ui(list(QUERY_STRING = "")))
  expect_false(grepl("girafe", ui))
  expect_false(grepl("Interactive plots", ui))

  s <- spleen_specimens()
  p <- plot_clr_biplot(clr_pca(s$x), s$d$Site, interactive = TRUE)  # silently static
  expect_false(any(purrr::map_lgl(p$layers, \(l) grepl("Interactive", class(l$geom)[1]))))

  # cairo reported as missing, or failing with a warning (as on the macOS runner)
  local_mocked_bindings(tiff = function(..., type) {
    if (!missing(type) && identical(type, "cairo")) {
      warning("failed to load cairo DLL")
      return(invisible(NULL))
    }
    grDevices::png(...)  # stand-in device
  }, .package = "grDevices")
  for (avail in c(TRUE, FALSE)) {
    local_mocked_bindings(cairo_available = function() avail)
    f <- tempfile(fileext = ".tiff")
    expect_no_error(save_plot(f, function() plot(1:3), "tiff", width = 8, height = 8, dpi = 100))
    expect_gt(file.size(f), 500)
  }
})

test_that("biplot keeps equal scales unless one axis would be squashed", {
  expect_equal(biplot_aspect(c(-2, 2), c(-1, 1))$ratio, 1)
  squashed <- biplot_aspect(c(-2, 2), c(-0.1, 0.1))
  expect_null(squashed[[1]]$ratio)
  sim <- simulate_liver()
  x   <- compositions::acomp(as.matrix(sim[, -(1:2)]) + 0.1)
  p   <- plot_clr_biplot(clr_pca(x), sim$Group)
  expect_match(p$labels$caption, "different scales")
})
