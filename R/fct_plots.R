# ==============================================================================
# Plot builders and export (pure functions; ggplot2 unless noted)
# ==============================================================================

#' Save a ggplot or a base-graphics drawing function to a file
#'
#' @param file Output path.
#' @param plot A ggplot object, or a function that draws with base graphics.
#' @param format "pdf", "png", "svg" or "tiff".
#' @param width,height Size in centimetres.
#' @param dpi Resolution for raster formats.
#' @param base_size Base font size (pt).
#' @noRd
save_plot <- function(file, plot, format = "pdf", width = 17, height = 15,
                      dpi = 300, base_size = 11) {
  w <- width / 2.54
  h <- height / 2.54
  if (inherits(plot, "ggplot")) {
    plot <- plot + ggplot2::theme(text = ggplot2::element_text(size = base_size))
    dev  <- switch(format, svg = svglite::svglite, format)
    args <- list(filename = file, plot = plot, device = dev, width = w, height = h,
                 units = "in", dpi = dpi, bg = "white")
    if (format == "tiff") args$compression <- "lzw"
    do.call(ggplot2::ggsave, args)
  } else {
    switch(format,
      pdf  = grDevices::pdf(file, width = w, height = h, pointsize = base_size),
      png  = grDevices::png(file, width = w, height = h, units = "in", res = dpi,
                            pointsize = base_size),
      svg  = svglite::svglite(file, width = w, height = h, pointsize = base_size),
      tiff = {
        # LZW needs the cairo device; without system cairo (e.g. macOS without
        # XQuartz) it only warns and opens nothing, so check before using it
        opened <- cairo_available() && tryCatch({
          grDevices::tiff(file, width = w, height = h, units = "in", res = dpi,
                          pointsize = base_size, compression = "lzw", type = "cairo")
          TRUE
        }, error = function(e) FALSE, warning = function(w) FALSE)
        if (!opened) {
          grDevices::tiff(file, width = w, height = h, units = "in", res = dpi,
                          pointsize = base_size)
        }
      })
    on.exit(grDevices::dev.off(), add = TRUE)
    plot()
  }
  invisible(file)
}

#' Export settings shown once per tab (format, size, resolution, font)
#' @noRd
export_settings_ui <- function(ns, lang = "en") {
  t <- function(x) tr(x, lang)
  tags$details(
    tags$summary(strong(t("Figure export settings"))),
    fluidRow(
      column(3, selectInput(ns("exp_format"), t("Format:"),
                            choices = c("PDF" = "pdf", "PNG" = "png",
                                        "SVG" = "svg", "TIFF" = "tiff"))),
      column(2, numericInput(ns("exp_width"), t("Width (cm):"), 17, min = 4, max = 60)),
      column(2, numericInput(ns("exp_height"), t("Height (cm):"), 15, min = 4, max = 60)),
      column(2, selectInput(ns("exp_dpi"), t("DPI:"), choices = c(300, 600, 1200))),
      column(3, numericInput(ns("exp_font"), t("Font size (pt):"), 11, min = 6, max = 24))
    )
  )
}

#' downloadHandler that saves `make_plot()` with the tab's export settings
#' @noRd
plot_download <- function(input, name, make_plot) {
  downloadHandler(
    filename = function() paste0(name, "_", Sys.Date(), ".", input$exp_format %||% "pdf"),
    content  = function(file) {
      save_plot(file, make_plot(), format = input$exp_format %||% "pdf",
                width = input$exp_width %||% 17, height = input$exp_height %||% 15,
                dpi = as.numeric(input$exp_dpi %||% 300),
                base_size = input$exp_font %||% 11)
    }
  )
}

#' Sample labels for tooltips: a Specimen or ID-like column, else row numbers
#' @noRd
sample_ids <- function(df) {
  id_col <- intersect(c("Specimen", "Sample", "Sample_ID"), names(df))
  if (length(id_col) == 0) id_col <- names(df)[is_id_name(names(df))]
  if (length(id_col) > 0) as.character(df[[id_col[1]]]) else as.character(seq_len(nrow(df)))
}

#' Can interactive plots be used? ggiraph needs gdtools, whose binary links
#' to system cairo (missing e.g. on macOS without XQuartz)
#' @noRd
has_ggiraph <- function() {
  isTRUE(tryCatch(requireNamespace("ggiraph", quietly = TRUE), error = function(e) FALSE))
}

#' Is the cairo graphics device usable? (needs system cairo libraries)
#' @noRd
cairo_available <- function() {
  if (!isTRUE(capabilities("cairo"))) return(FALSE)
  # capabilities() can claim cairo while its system libraries are missing
  # (macOS without XQuartz): try opening a device for real
  f <- tempfile(fileext = ".tiff")
  on.exit(unlink(f))
  tryCatch({
    grDevices::tiff(f, type = "cairo")
    grDevices::dev.off()
    TRUE
  }, warning = function(w) FALSE, error = function(e) FALSE)
}

#' Point layer that is interactive (hover tooltip) when requested
#' @noRd
geom_point_maybe <- function(interactive, ...) {
  if (interactive && has_ggiraph()) ggiraph::geom_point_interactive(...) else ggplot2::geom_point(...)
}

# ------------------------------------------------------------------------------
# clr biplot, scree plot, ray table, variation matrix
# ------------------------------------------------------------------------------

#' Compositional ray biplot (covariance biplot of the unscaled clr-PCA)
#'
#' Rays are the column coordinates (covariances with the axes), rescaled by a
#' single constant to fit the sample cloud, so relative ray lengths and links
#' keep their meaning.
#' @param pca FactoMineR PCA from `clr_pca()`.
#' @param groups Factor for colour (and hulls).
#' @param shape Optional second factor for point shape.
#' @param ids Sample labels (tooltips or text labels).
#' @param axes Two axis numbers.
#' @noRd
plot_clr_biplot <- function(pca, groups, palette = "okabe_ito", axes = c(1, 2),
                            shape = NULL, ids = NULL, show_labels = FALSE,
                            hulls = TRUE, interactive = FALSE, base_size = 14) {
  ind <- pca$ind$coord[, axes, drop = FALSE]
  var <- pca$var$coord[, axes, drop = FALSE]
  k   <- 0.8 * max(abs(ind)) / max(abs(var))
  pts <- data.frame(x = ind[, 1], y = ind[, 2], group = as.factor(groups),
                    id = ids %||% as.character(seq_len(nrow(ind))))
  if (!is.null(shape)) pts$shape <- as.factor(shape)
  rays <- data.frame(x = var[, 1] * k, y = var[, 2] * k, part = rownames(var))
  pct  <- pca$eig[axes, 2]
  hull_df <- do.call(rbind, lapply(split(pts, pts$group), function(s) {
    if (nrow(s) < 3) s else s[grDevices::chull(s$x, s$y), ]
  }))
  pt_aes <- if (is.null(shape)) {
    ggplot2::aes(x = .data$x, y = .data$y, colour = .data$group,
                 tooltip = .data$id, data_id = .data$id)
  } else {
    ggplot2::aes(x = .data$x, y = .data$y, colour = .data$group, shape = .data$shape,
                 tooltip = .data$id, data_id = .data$id)
  }
  p <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60")
  if (hulls) {
    p <- p + ggplot2::geom_polygon(data = hull_df,
                                   ggplot2::aes(x = .data$x, y = .data$y,
                                                fill = .data$group, colour = .data$group),
                                   alpha = 0.12, linewidth = 0.5, show.legend = FALSE)
  }
  p <- p + suppressWarnings(geom_point_maybe(interactive, data = pts, mapping = pt_aes,
                                             size = 2.5, alpha = 0.85))
  if (show_labels) {
    p <- p + ggplot2::geom_text(data = pts, ggplot2::aes(x = .data$x, y = .data$y,
                                                        label = .data$id),
                                size = base_size / 5, vjust = -0.8, colour = "grey30")
  }
  p + ggplot2::geom_segment(data = rays,
                            ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y),
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
                            colour = "grey15") +
    ggrepel::geom_label_repel(data = rays, ggplot2::aes(x = .data$x, y = .data$y,
                                                        label = .data$part),
                              size = base_size / 4, fill = "white", alpha = 0.85,
                              label.size = 0, min.segment.length = 1, seed = 1) +
    scale_palette(palette, nlevels(pts$group)) +
    ggplot2::labs(x = sprintf("Dim%d (%.1f%%)", axes[1], pct[1]),
                  y = sprintf("Dim%d (%.1f%%)", axes[2], pct[2]),
                  colour = NULL, fill = NULL, shape = NULL) +
    biplot_aspect(ind[, 1], ind[, 2]) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(legend.position = "bottom")
}

#' Equal axis scales for a biplot unless one axis would be squashed
#'
#' Equal scales preserve angles and distances, so they are kept whenever the
#' second axis spans at least a quarter of the first. Beyond that (e.g. Dim1
#' 99% vs Dim2 0.4%) the plot collapses into a line, so the scales are freed
#' and a caption says that angles are distorted.
#' @noRd
biplot_aspect <- function(x, y, min_ratio = 0.25) {
  ratio <- diff(range(y)) / diff(range(x))
  if (is.finite(ratio) && ratio >= min_ratio && ratio <= 1 / min_ratio) {
    return(ggplot2::coord_equal())
  }
  list(ggplot2::coord_cartesian(),
       ggplot2::labs(caption = "Axes drawn at different scales: angles and lengths are distorted."))
}

#' Scree plot of the clr-PCA (percentage of variance per axis)
#' @noRd
plot_scree <- function(pca, base_size = 14) {
  keep <- pca$eig[, 1] > 1e-10 * sum(pca$eig[, 1])  # clr has D - 1 non-null axes
  eig  <- data.frame(axis = factor(seq_len(sum(keep))), pct = pca$eig[keep, 2])
  ggplot2::ggplot(eig, ggplot2::aes(x = .data$axis, y = .data$pct)) +
    ggplot2::geom_col(fill = "grey45", width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", .data$pct)), vjust = -0.4,
                       size = base_size / 4) +
    ggplot2::labs(x = "Axis", y = "Variance explained (%)") +
    ggplot2::theme_minimal(base_size = base_size)
}

#' Ray lengths (clr standard deviations) and share of the total variance
#'
#' FactoMineR uses 1/n weights; lengths are rescaled to sample SDs (n - 1),
#' consistent with the variation matrix.
#' @noRd
ray_table <- function(pca) {
  v   <- pca$var$coord
  n   <- nrow(pca$ind$coord)
  len <- sqrt(rowSums(v^2) * n / (n - 1))
  data.frame(Part = rownames(v), clr_SD = len,
             Share_of_variance = 100 * len^2 / sum(len^2), row.names = NULL)
}

#' Heatmap of the variation matrix (variances of all pairwise log-ratios)
#' @noRd
plot_variation_matrix <- function(comp, base_size = 14) {
  vm <- compositions::variation(compositions::acomp(comp), robust = FALSE)
  dimnames(vm) <- list(colnames(comp), colnames(comp))
  long <- data.frame(row = rep(rownames(vm), ncol(vm)),
                     col = rep(colnames(vm), each = nrow(vm)),
                     var = as.vector(vm))
  long$row <- factor(long$row, levels = rownames(vm))
  long$col <- factor(long$col, levels = rev(colnames(vm)))
  long <- long[long$row != long$col, ]
  ggplot2::ggplot(long, ggplot2::aes(x = .data$row, y = .data$col, fill = .data$var)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$var),
                                    colour = .data$var > stats::median(.data$var)),
                       size = base_size / 4, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey10")) +
    ggplot2::scale_fill_viridis_c(option = "magma", direction = -1,
                                  name = "var(log-ratio)") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   panel.grid = ggplot2::element_blank())
}

# ------------------------------------------------------------------------------
# Ternary regions
# ------------------------------------------------------------------------------

#' 95% confidence (of the mean) or predictive region of a 3-part composition
#'
#' Computed in ilr coordinates under a normal model and mapped back to the
#' simplex, so the region is an ellipse in the Aitchison geometry.
#' @param x Matrix of 3 strictly positive parts.
#' @param type "confidence" or "predictive".
#' @return Matrix of closed compositions tracing the region boundary.
#' @noRd
ternary_region <- function(x, type = c("confidence", "predictive"), level = 0.95,
                           n_points = 100) {
  type <- match.arg(type)
  z <- unclass(compositions::ilr(compositions::acomp(x)))
  n <- nrow(z)
  if (n < 4) return(NULL)
  m <- colMeans(z)
  S <- stats::cov(z)
  q <- 2 * (n - 1) / (n - 2) * stats::qf(level, 2, n - 2)
  r2 <- if (type == "confidence") q / n else q * (1 + 1 / n)
  a  <- seq(0, 2 * pi, length.out = n_points)
  circle <- cbind(cos(a), sin(a)) * sqrt(r2)
  ell <- sweep(circle %*% chol(S), 2, m, "+")
  unclass(compositions::ilrInv(ell))
}

# ------------------------------------------------------------------------------
# Balances
# ------------------------------------------------------------------------------

#' ilr balances defined by an SBP, one column per balance (b1, b2, ...)
#' @noRd
balance_coords <- function(comp, sbp) {
  bal <- unclass(compositions::ilr(compositions::acomp(comp),
                                   V = compositions::gsi.buildilrBase(t(sbp))))
  bal <- as.data.frame(matrix(bal, ncol = nrow(sbp)))
  names(bal) <- rownames(sbp)
  bal
}

#' Univariate permutational ANOVA of each balance against a grouping factor
#' @return Data frame: Balance, F, Z (effect size), P.
#' @noRd
balance_tests <- function(bal, groups, iter = 999, seed = 42) {
  groups <- as.factor(groups)
  purrr::map_dfr(names(bal), \(b) {
    rdf <- RRPP::rrpp.data.frame(y = bal[[b]], g = groups)
    tab <- stats::anova(RRPP::lm.rrpp(y ~ g, data = rdf, iter = iter, seed = seed,
                                      print.progress = FALSE))$table
    data.frame(Balance = b, F = tab[1, "F"], Z = tab[1, "Z"], P = tab[1, "Pr(>F)"])
  })
}

#' Boxplots of each balance per group, facets labelled numerator / denominator
#' @noRd
plot_balances <- function(bal, groups, sbp, palette = "okabe_ito", base_size = 13) {
  lab  <- sbp_labels(sbp)
  labs <- stats::setNames(
    purrr::map_chr(sprintf("%s: %s / %s", lab$Balance, lab$Numerator, lab$Denominator),
                   \(t) paste(strwrap(t, 40), collapse = "\n")),
    lab$Balance)
  long <- data.frame(Balance = factor(rep(names(bal), each = nrow(bal)), levels = names(bal)),
                     value   = unlist(bal, use.names = FALSE),
                     group   = rep(as.factor(groups), times = ncol(bal)))
  ggplot2::ggplot(long, ggplot2::aes(x = .data$group, y = .data$value, fill = .data$group)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.6) +
    ggplot2::geom_jitter(width = 0.12, height = 0, size = 1.3, alpha = 0.6) +
    ggplot2::facet_wrap(~ Balance, scales = "free_y",
                        labeller = ggplot2::as_labeller(labs)) +
    scale_palette(palette, nlevels(long$group)) +
    ggplot2::labs(x = NULL, y = "Balance (ilr)", fill = NULL) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}
