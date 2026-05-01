# ==============================================================================
# Helper and utility functions for CoDa Stereo
# ==============================================================================

#' Compute per-column detection limits
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
is_id_name <- function(x) {
  grepl("^id$|_id$|^id_", x, ignore.case = TRUE)
}

#' Replace NAs with column geometric mean of positive values
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
factor_candidates <- function(df, parts) {
  setdiff(names(df)[!sapply(df, is.numeric)], parts)
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
comp_center_by_group <- function(comp, groups) {
  lvls <- levels(as.factor(groups))
  out  <- lapply(lvls, function(g) {
    sub <- comp[groups == g, ]
    ctr <- compositions::mean.acomp(compositions::acomp(sub))
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
comp_totvar_by_group <- function(comp, groups) {
  lvls <- levels(as.factor(groups))
  out  <- lapply(lvls, function(g) {
    sub <- comp[groups == g, ]
    tv  <- compositions::mvar(compositions::acomp(sub))
    data.frame(Group = g, Total_Variance = tv, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# ==============================================================================
# Manual ternary plot (ggplot2 only — no ggtern dependency)
# ==============================================================================

#' Build a ternary plot using plain ggplot2
plot_ternary_manual <- function(data, parts, group_var,
                                amalg = NULL, base_size = 14) {
  h <- sqrt(3) / 2
  vL <- c(0, 0);  vT <- c(0.5, h);  vR <- c(1, 0)

  if (!is.null(amalg) && length(amalg) == 3) {
    L  <- rowSums(data[, amalg[[1]], drop = FALSE])
    Tp <- rowSums(data[, amalg[[2]], drop = FALSE])
    R  <- rowSums(data[, amalg[[3]], drop = FALSE])
    labels <- names(amalg)
  } else {
    L  <- data[[parts[1]]]
    Tp <- data[[parts[2]]]
    R  <- data[[parts[3]]]
    labels <- parts
  }
  S <- L + Tp + R

  pts <- data.frame(
    xc    = (R / S) + 0.5 * (Tp / S),
    yc    = (Tp / S) * h,
    group = data[[group_var]]
  )

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

  # -- Convex hulls per group -------------------------------------------------
  hulls <- do.call(rbind, lapply(split(pts, pts$group), function(sub) {
    if (nrow(sub) < 3) return(sub)  # need ≥3 points for a hull
    sub[grDevices::chull(sub$xc, sub$yc), ]
  }))

  # -- Assemble ---------------------------------------------------------------
  ggplot2::ggplot() +
    ggplot2::geom_line(data = grid, ggplot2::aes(x = .data$x, y = .data$y, group = .data$gid),
                       colour = "grey85", linewidth = 0.25) +
    ggplot2::geom_polygon(data = tri, ggplot2::aes(x = .data$x, y = .data$y),
                          fill = NA, colour = "grey30", linewidth = 0.6) +
    ggplot2::geom_polygon(data = hulls,
                          ggplot2::aes(x = .data$xc, y = .data$yc,
                                       fill = .data$group, colour = .data$group),
                          alpha = 0.15, linewidth = 0.5) +
    ggplot2::geom_point(data = pts, ggplot2::aes(x = .data$xc, y = .data$yc, colour = .data$group),
                        size = 3, alpha = 0.8) +
    ggplot2::geom_text(data = ticks, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                                                   hjust = .data$hj, vjust = .data$vj),
                       size = base_size / 4, colour = "grey50") +
    ggplot2::annotate("text", x = vL[1], y = vL[2] - 0.08, label = labels[1],
                      fontface = "bold", size = base_size / 3, hjust = 0.5) +
    ggplot2::annotate("text", x = vT[1], y = vT[2] + 0.06, label = labels[2],
                      fontface = "bold", size = base_size / 3, hjust = 0.5) +
    ggplot2::annotate("text", x = vR[1], y = vR[2] - 0.08, label = labels[3],
                      fontface = "bold", size = base_size / 3, hjust = 0.5) +
    ggplot2::labs(colour = group_var, fill = group_var) +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(legend.position = "bottom",
                   plot.margin = ggplot2::margin(20, 20, 30, 20))
}
