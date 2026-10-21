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
