# ==============================================================================
# Helper and utility functions for CoDa Stereo
# ==============================================================================

#' Compute per-column detection limits
#' @noRd
compute_dl <- function(mat, frac = 0.5) {
  vec <- apply(mat, 2, function(x) {
    nz <- x[x > 0 & !is.na(x)]
    if (length(nz) == 0) 1e-6 else max(frac * min(nz), 1e-6)
  })
  list(
    vec = vec,
    mat = matrix(vec, nrow = nrow(mat), ncol = ncol(mat), byrow = TRUE,
                 dimnames = dimnames(mat))
  )
}

#' Heuristic: does a column name look like an ID?
#' @noRd
is_id_name <- function(x) {
  grepl("^id$|_id$|^id_", x, ignore.case = TRUE)
}

#' Replace NAs with column geometric mean of positive values
#' @noRd
gmean_impute <- function(mat) {
  for (j in seq_len(ncol(mat))) {
    col <- mat[, j]
    valid <- col[col > 0 & !is.na(col)]
    if (length(valid) > 0 && anyNA(col)) {
      mat[is.na(col), j] <- exp(mean(log(valid)))
    }
  }
  mat
}

#' Get non-numeric, non-part column names (factor candidates)
#'
#' Columns whose values are mostly unique (IDs) or constant cannot act as
#' grouping factors and are left out.
#' @noRd
factor_candidates <- function(df, parts) {
  cands <- setdiff(names(df)[!purrr::map_lgl(df, is.numeric)], parts)
  purrr::keep(cands, \(v) {
    k <- dplyr::n_distinct(df[[v]])
    k >= 2 && k <= nrow(df) / 2
  })
}

#' Path to a file shipped in inst/, both installed and from the source tree
#' (the root app.R runs from the repository without installing the package)
#' @noRd
app_file <- function(...) {
  p <- system.file(..., package = "CoDaStereo")
  if (nzchar(p)) p else file.path("inst", ...)
}

#' Simulated liver histomorphometry example (two groups, count zeros)
#' @noRd
simulate_liver <- function() {
  set.seed(42)
  n <- 20
  raw <- data.frame(
    Hepatocytes       = c(stats::rnorm(n, 80, 1.5), stats::rnorm(n, 65, 2.0)),
    Sinusoids         = c(stats::rnorm(n, 10, 1.0), stats::rnorm(n,  8, 0.9)),
    Stroma            = c(stats::rnorm(n,  8, 0.8), stats::rnorm(n,  6.5, 0.7)),
    Melanomacrophages = c(stats::rnorm(n, 2.5, 0.5), stats::rnorm(n, 20, 2.5))
  )
  raw[raw < 0] <- 0
  bdl_idx <- sample.int(n, size = round(n * 0.25))
  raw$Melanomacrophages[bdl_idx] <- 0
  data.frame(Sample_ID = seq_len(2 * n),
             Group     = factor(rep(c("Control", "Impacted"), each = n)),
             100 * raw / rowSums(raw))
}

#' Example datasets offered in the Data Upload tab
#'
#' Real datasets are the case studies of the companion paper (built by
#' data-raw/prepare_examples.R). `specimen` names the column to aggregate
#' fields by, when rows are fields of view nested in specimens.
#' @noRd
example_datasets <- function() {
  list(
    liver_sim  = list(file = NULL, specimen = NULL,
                      parts = c("Hepatocytes", "Sinusoids", "Stroma", "Melanomacrophages")),
    spleen     = list(file = "spleen_fields.csv", specimen = "Specimen",
                      parts = c("Sinusoids", "Red_pulp", "White_pulp", "MMCs", "Blood_vessels")),
    kidney     = list(file = "kidney_fields.csv", specimen = "Specimen",
                      parts = c("Proximal_tubule", "Distal_tubule", "Renal_corpuscle",
                                "Collecting_duct", "Interstitial_space")),
    leukocytes = list(file = "leukocytes.csv", specimen = NULL,
                      parts = c("Monocytes", "Lymphocytes", "Neutrophils", "Eosinophils")),
    nuclear    = list(file = "nuclear_abnormalities.csv", specimen = NULL,
                      parts = c("Abnormal", "Normal"))
  )
}

#' Load one example dataset by key
#' @noRd
load_example <- function(key) {
  ex <- example_datasets()[[key]]
  if (is.null(ex$file)) return(simulate_liver())
  df <- utils::read.csv(app_file("extdata", ex$file), stringsAsFactors = FALSE)
  df[] <- purrr::map(df, \(x) if (is.character(x)) factor(x) else x)
  df
}

#' Aggregate rows (e.g. fields of view) into specimens
#'
#' Parts are summed (counts) or averaged (areas, proportions). Other columns
#' are kept only if constant within every specimen.
#' @param df Data frame.
#' @param id Name of the specimen column.
#' @param parts Character vector of part names.
#' @param fun "sum" or "mean".
#' @return Aggregated data frame with attribute `dropped` (non-constant columns).
#' @noRd
aggregate_specimens <- function(df, id, parts, fun = c("sum", "mean")) {
  fun   <- match.arg(fun)
  f     <- if (fun == "sum") sum else mean
  other <- setdiff(names(df), c(id, parts))
  is_const <- purrr::map_lgl(other, \(v)
    all(tapply(df[[v]], df[[id]], \(x) dplyr::n_distinct(x) == 1)))
  keep  <- other[is_const]
  out <- df |>
    dplyr::group_by(.data[[id]]) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(keep), dplyr::first),
                     dplyr::across(dplyr::all_of(parts), f),
                     n_rows = dplyr::n(), .groups = "drop") |>
    as.data.frame()
  attr(out, "dropped") <- other[!is_const]
  out
}

# ==============================================================================
# Pre-flight check: detect parts with incompatible scales
# ==============================================================================

#' Check whether selected parts are on comparable scales
#'
#' Flags parts whose column mean is either < 1% or > 50× the median of
#' all part means, which strongly suggests a non-compositional covariate
#' (e.g., Glicogen measured on a different scale, or an ID column).
#'
#' @param df Data frame.
#' @param parts Character vector of selected part names.
#' @return A list with \code{ok} (logical), \code{warnings} (character vector
#'   of human-readable warnings), and \code{flagged} (names of flagged parts).
#' @noRd
preflight_check <- function(df, parts) {
  if (length(parts) < 2) {
    return(list(ok = FALSE,
                warnings = "At least 2 parts are required for compositional analysis.",
                flagged = character(0)))
  }

  mat <- df[, parts, drop = FALSE]
  col_means <- colMeans(mat, na.rm = TRUE)
  median_mean <- stats::median(col_means)
  warnings <- character(0)
  flagged  <- character(0)

  for (p in parts) {
    m <- col_means[p]

    # Ratio relative to the median of all part means
    if (median_mean > 0 && m > 0) {
      ratio <- m / median_mean
      if (ratio > 50 || ratio < 0.01) {
        flagged  <- c(flagged, p)
        warnings <- c(warnings, sprintf(
          "'%s' has mean = %.2f, which is %.0f\u00d7 the median part mean (%.2f). Is it really a compositional part?",
          p, m, ratio, median_mean
        ))
      }
    }

    # Negative values check
    if (any(mat[[p]] < 0, na.rm = TRUE)) {
      flagged  <- c(flagged, p)
      warnings <- c(warnings, sprintf(
        "'%s' contains negative values, which are undefined in the simplex.", p
      ))
    }
  }

  # Closure check: do rows sum to ~constant?
  row_sums <- rowSums(mat, na.rm = TRUE)
  cv_sums  <- stats::sd(row_sums) / mean(row_sums)
  if (cv_sums > 0.20) {
    warnings <- c(warnings, sprintf(
      "Row sums have high variability (CV = %.1f%%). Parts may not form a closed composition, or a non-compositional column may be included.",
      cv_sums * 100
    ))
  }

  list(ok = length(warnings) == 0, warnings = warnings, flagged = flagged)
}

# ==============================================================================
# Compositional descriptives per group
# ==============================================================================

#' Compute compositional center (geometric mean) per group
#'
#' @param comp An \code{acomp} object.
#' @param groups Factor vector of group labels (same length as nrow(comp)).
#' @return A data frame in long format: Group, Part, Proportion.
#' @noRd
comp_center_by_group <- function(comp, groups) {
  lvls <- levels(as.factor(groups))
  out  <- lapply(lvls, function(g) {
    sub <- comp[groups == g, ]
    ctr <- compositions::mean.acomp(compositions::acomp(sub), robust = FALSE)
    data.frame(
      Group      = g,
      Part       = names(ctr),
      Proportion = as.numeric(ctr),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

#' Compute total variance per group
#'
#' @param comp An \code{acomp} object.
#' @param groups Factor vector of group labels.
#' @return A data frame: Group, Total_Variance.
#' @noRd
comp_totvar_by_group <- function(comp, groups) {
  lvls <- levels(as.factor(groups))
  out  <- lapply(lvls, function(g) {
    sub <- comp[groups == g, ]
    tv  <- compositions::mvar(compositions::acomp(sub), robust = FALSE)
    data.frame(Group = g, Total_Variance = tv, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# ==============================================================================
# Colour palettes (shared by every group/part-coloured plot)
# ==============================================================================

#' Palettes offered in the UI (label = value)
#' @noRd
plot_palettes <- c("Okabe-Ito (colour-blind safe)" = "okabe_ito",
                   "Dark2 (ColorBrewer)"           = "dark2",
                   "Viridis"                       = "viridis")

#' Resolve n colours from a named palette
#'
#' Discrete palettes are recycled when n exceeds their length.
#' @noRd
palette_colours <- function(name, n) {
  switch(name,
    okabe_ito = rep_len(c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                          "#0072B2", "#D55E00", "#CC79A7", "#000000"), n),
    dark2     = rep_len(scales::brewer_pal(palette = "Dark2")(8), n),
    viridis   = scales::viridis_pal(end = 0.9)(n),
    stop("Unknown palette: ", name)
  )
}

#' ggplot2 colour + fill scale for a named palette
#' @noRd
scale_palette <- function(name, n) {
  ggplot2::scale_colour_manual(values = palette_colours(name, n),
                               aesthetics = c("colour", "fill"))
}

# ==============================================================================
# Composition + total (Pawlowsky-Glahn, Egozcue & Lovell 2015,
# Statistical Modelling 15(2):175-190, doi:10.1177/1471082X14535526)
# ==============================================================================

#' Log-total coordinate to analyse jointly with the ilr coordinates
#'
#' "sum": ln of the sum of parts (product space Ts; recommended by the authors
#' when the relevant total is the sum). "product": ln of prod(x)^(1/sqrt(D)) =
#' sum(ln x) / sqrt(D) (space Tp), which makes [ilr, coordinate] isometric to
#' the log of each part. For "product", zero-replaced parts are rescaled to the
#' observed sum.
#' @param raw Numeric matrix of parts on their original (absolute) scale.
#' @param comp \code{acomp} of the same rows (zero-replaced).
#' @noRd
log_total_coord <- function(raw, comp, type = c("sum", "product")) {
  type <- match.arg(type)
  t_s  <- rowSums(raw)
  if (type == "sum") return(log(t_s))
  x_abs <- unclass(compositions::acomp(comp)) * t_s
  rowSums(log(x_abs)) / sqrt(ncol(x_abs))
}

#' Coefficient of variation of the totals (flags totals fixed by design)
#' @noRd
total_cv <- function(raw) {
  t_s <- rowSums(raw)
  stats::sd(t_s) / mean(t_s)
}

# ==============================================================================
# clr-PCA (basis of the compositional biplot)
# ==============================================================================

#' PCA of centred, unscaled clr coordinates
#'
#' clr variables must not be standardised: with unit variance every ray has the
#' same length and links no longer approximate log-ratio standard deviations.
#' @param comp An \code{acomp} object.
#' @param log_total Optional log-total coordinate (see `log_total_coord()`),
#'   added as a variable for the T-space biplot of Pawlowsky-Glahn et al. (2015).
#' @return A FactoMineR \code{PCA} object.
#' @noRd
clr_pca <- function(comp, log_total = NULL) {
  clr_data <- as.data.frame(unclass(compositions::clr(comp)))
  # T-space biplot: the (centred) log-total enters as one more variable
  if (!is.null(log_total)) clr_data$log_total <- log_total
  FactoMineR::PCA(clr_data, scale.unit = FALSE, ncp = ncol(clr_data),
                  graph = FALSE)
}

# ==============================================================================
# Sequential binary partition (SBP) and CoDa dendrogram
# ==============================================================================

#' Default SBP from Ward clustering of parts on clr distances
#'
#' @param comp An \code{acomp} object.
#' @return Numeric matrix, (D-1) balances x D parts, entries in {-1, 0, 1};
#'   b1 is the first (root) partition.
#' @noRd
sbp_auto <- function(comp) {
  hc  <- stats::hclust(stats::dist(t(compositions::clr(comp))), method = "ward.D")
  sig <- compositions::gsi.merge2signary(hc$merge)  # parts x balances, root last
  sbp <- t(sig)[rev(seq_len(ncol(sig))), , drop = FALSE]
  dimnames(sbp) <- list(paste0("b", seq_len(nrow(sbp))), colnames(comp))
  sbp
}

#' Problems that make a matrix an invalid SBP (empty = valid)
#' @noRd
sbp_problems <- function(sbp) {
  D <- ncol(sbp)
  if (nrow(sbp) != D - 1) {
    return(sprintf("%d parts need exactly %d balances.", D, D - 1))
  }
  one_sided <- rownames(sbp)[rowSums(sbp == 1) == 0 | rowSums(sbp == -1) == 0]
  if (length(one_sided) > 0) {
    return(sprintf("Balance %s needs at least one +1 and one -1 part.",
                   one_sided))
  }
  out <- character(0)
  if (sum(rowSums(sbp != 0) == D) != 1) {
    out <- c(out, "Exactly one balance (the first split) must involve all parts.")
  }
  nested_in <- function(a, b) {  # support of row a lies on one side of row b
    sa <- sbp[a, ] != 0
    all(sbp[b, sa] == 1) || all(sbp[b, sa] == -1)
  }
  pairs <- utils::combn(nrow(sbp), 2, simplify = FALSE)
  clash <- purrr::keep(pairs, \(ij) {
    i <- ij[1]; j <- ij[2]
    any(sbp[i, ] != 0 & sbp[j, ] != 0) && !nested_in(i, j) && !nested_in(j, i)
  })
  out <- c(out, purrr::map_chr(clash, \(ij) sprintf(
    "Balances %s and %s are not nested: each balance must split one group of its parent.",
    rownames(sbp)[ij[1]], rownames(sbp)[ij[2]])))
  out
}

#' Human-readable balance definitions: numerator / denominator parts
#' @noRd
sbp_labels <- function(sbp) {
  data.frame(
    Balance     = rownames(sbp),
    Numerator   = purrr::map_chr(seq_len(nrow(sbp)), \(i)
                    paste(colnames(sbp)[sbp[i, ] == 1], collapse = ", ")),
    Denominator = purrr::map_chr(seq_len(nrow(sbp)), \(i)
                    paste(colnames(sbp)[sbp[i, ] == -1], collapse = ", ")),
    stringsAsFactors = FALSE
  )
}

#' CoDa dendrogram with per-group boxplots of each balance (base graphics)
#'
#' Vertical bar length = variance of the balance; its contact point with the
#' horizontal axis = mean balance. Boxes show the balance distribution per group.
#' @noRd
plot_coda_dendrogram <- function(comp, sbp, groups, palette = "okabe_ito") {
  # CoDaDendrogram reads getOption("robust"), set only when compositions is attached
  op <- options(robust = getOption("robust", FALSE))
  on.exit(options(op), add = TRUE)
  x    <- compositions::acomp(comp)
  sig  <- t(sbp)
  bal  <- compositions::ilr(x, V = compositions::gsi.buildilrBase(sig))
  lim  <- ceiling(max(abs(unclass(bal))))
  grp  <- as.factor(groups)
  k    <- nlevels(grp)
  cols <- palette_colours(palette, k)
  # box.space is relative to the smallest balance variance; scale it so each
  # group box is ~4% of the tallest variance bar
  vars  <- apply(unclass(bal), 2, stats::var)
  space <- 0.04 * max(vars) / max(min(vars), .Machine$double.eps)

  op_par <- graphics::par(mar = c(9, 4, 1, 1))
  on.exit(graphics::par(op_par), add = TRUE)
  compositions::CoDaDendrogram(X = x, signary = sig, type = "lines",
                               range = c(-lim, lim))
  graphics::title(ylab = "Balance variance")
  xs <- split(x, grp)
  for (i in seq_len(k)) {
    compositions::CoDaDendrogram(X = xs[[i]], col = cols[i], border = cols[i],
                                 type = "boxplot", box.pos = i - (k + 1) / 2,
                                 box.space = space, add = TRUE)
  }
  graphics::legend("topright", legend = levels(grp), fill = cols, bty = "n")
  invisible(NULL)
}

# ==============================================================================
# Manual ternary plot (ggplot2 only — no ggtern dependency)
# ==============================================================================

#' Build a ternary plot using plain ggplot2
#' @noRd
plot_ternary_manual <- function(data, parts, group_var,
                                amalg = NULL, base_size = 14,
                                palette = "okabe_ito", regions = "hull",
                                centre = FALSE, ids = NULL, interactive = FALSE,
                                region_data = NULL) {
  h <- sqrt(3) / 2
  vL <- c(0, 0);  vT <- c(0.5, h);  vR <- c(1, 0)

  three_parts <- function(d) {
    if (!is.null(amalg) && length(amalg) == 3) {
      purrr::map(amalg, \(p) rowSums(d[, p, drop = FALSE]))
    } else {
      purrr::map(parts[1:3], \(p) d[[p]])
    }
  }
  abc <- three_parts(data)
  L <- abc[[1]]; Tp <- abc[[2]]; R <- abc[[3]]
  labels <- if (!is.null(amalg) && length(amalg) == 3) names(amalg) else parts
  S <- L + Tp + R

  to_xy <- function(l, t, r) {
    s <- l + t + r
    data.frame(xc = (r / s) + 0.5 * (t / s), yc = (t / s) * h)
  }
  pts <- data.frame(to_xy(L, Tp, R), group = data[[group_var]],
                    id = ids %||% as.character(seq_len(length(L))))
  pts$group <- as.factor(pts$group)
  # Regions and centres are log-ratio summaries: use the zero-replaced data
  # (region_data, same columns plus group_var) when given, else positive rows
  rd   <- region_data %||% data
  rabc <- three_parts(rd)
  rL <- rabc[[1]]; rT <- rabc[[2]]; rR <- rabc[[3]]
  lr_ok   <- rL > 0 & rT > 0 & rR > 0 & !is.na(rL + rT + rR)
  grp_idx <- split(which(lr_ok), as.factor(rd[[group_var]])[lr_ok])

  tri <- data.frame(x = c(vL[1], vR[1], vT[1], vL[1]),
                    y = c(vL[2], vR[2], vT[2], vL[2]))

  fracs <- c(0.2, 0.4, 0.6, 0.8)
  grid  <- do.call(rbind, lapply(seq_along(fracs), function(i) {
    f <- fracs[i]
    rbind(
      data.frame(x = c(0.5 * (1 - f), 1 - f),
                 y = c((1 - f) * h, 0),            gid = paste0("iL", i)),
      data.frame(x = c(0.5 * f, 1 - 0.5 * f),
                 y = c(f * h, f * h),               gid = paste0("iT", i)),
      data.frame(x = c(f, 0.5 + 0.5 * f),
                 y = c(0, (1 - f) * h),             gid = paste0("iR", i))
    )
  }))

  pct <- paste0(fracs * 100)
  off <- 0.04
  ticks <- rbind(
    data.frame(x = 1 - fracs, y = -off,                       label = pct, hj = 0.5, vj = 1),
    data.frame(x = 0.5 * fracs - off, y = fracs * h,          label = pct, hj = 1,   vj = 0.5),
    data.frame(x = 0.5 + 0.5 * fracs + off, y = (1 - fracs) * h, label = pct, hj = 0,   vj = 0.5)
  )

  # -- Group regions: convex hulls or 95% ellipses (ilr normal model) ----------
  hulls <- if (regions == "hull") {
    do.call(rbind, lapply(split(pts, pts$group), function(sub) {
      if (nrow(sub) < 3) return(sub)  # need >= 3 points for a hull
      sub[grDevices::chull(sub$xc, sub$yc), ]
    }))
  } else if (regions %in% c("confidence", "predictive")) {
    do.call(rbind, purrr::imap(grp_idx, \(i, g) {
      reg <- ternary_region(cbind(rL[i], rT[i], rR[i]), regions)
      if (is.null(reg)) return(NULL)
      data.frame(to_xy(reg[, 1], reg[, 2], reg[, 3]), group = g)
    }))
  }
  centres <- if (centre) {
    do.call(rbind, purrr::imap(grp_idx, \(i, g) {
      gm <- exp(colMeans(log(cbind(rL[i], rT[i], rR[i]))))
      data.frame(to_xy(gm[1], gm[2], gm[3]), group = g)
    }))
  }

  # -- Assemble ---------------------------------------------------------------
  ggplot2::ggplot() +
    ggplot2::geom_line(data = grid, ggplot2::aes(x = .data$x, y = .data$y, group = .data$gid),
                       colour = "grey85", linewidth = 0.25) +
    ggplot2::geom_polygon(data = tri, ggplot2::aes(x = .data$x, y = .data$y),
                          fill = NA, colour = "grey30", linewidth = 0.6) +
    (if (!is.null(hulls)) {
      ggplot2::geom_polygon(data = hulls,
                            ggplot2::aes(x = .data$xc, y = .data$yc,
                                         fill = .data$group, colour = .data$group),
                            alpha = 0.15, linewidth = 0.5)
    }) +
    suppressWarnings(geom_point_maybe(interactive, data = pts,
                                      mapping = ggplot2::aes(x = .data$xc, y = .data$yc,
                                                             colour = .data$group,
                                                             tooltip = .data$id,
                                                             data_id = .data$id),
                                      size = 3, alpha = 0.8)) +
    (if (!is.null(centres)) {
      ggplot2::geom_point(data = centres,
                          ggplot2::aes(x = .data$xc, y = .data$yc, fill = .data$group),
                          shape = 23, size = 5, colour = "black", stroke = 1)
    }) +
    ggplot2::geom_text(data = ticks, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                                                   hjust = .data$hj, vjust = .data$vj),
                       size = base_size / 4, colour = "grey50") +
    ggplot2::annotate("text", x = vL[1], y = vL[2] - 0.08, label = labels[1],
                      fontface = "bold", size = base_size / 3, hjust = 0.5) +
    ggplot2::annotate("text", x = vT[1], y = vT[2] + 0.06, label = labels[2],
                      fontface = "bold", size = base_size / 3, hjust = 0.5) +
    ggplot2::annotate("text", x = vR[1], y = vR[2] - 0.08, label = labels[3],
                      fontface = "bold", size = base_size / 3, hjust = 0.5) +
    scale_palette(palette, length(unique(pts$group))) +
    ggplot2::labs(colour = group_var, fill = group_var) +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(legend.position = "bottom",
                   plot.margin = ggplot2::margin(20, 20, 30, 20))
}
