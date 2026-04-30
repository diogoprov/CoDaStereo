# ==============================================================================
# Helper and utility functions for CoDa Stereo
# ==============================================================================

#' Compute per-column detection limits
#'
#' For each column, DL = frac * min(non-zero, non-NA values).
#' Returns both a vector and a row-replicated matrix.
#' @param mat Numeric matrix.
#' @param frac Fraction of the minimum non-zero value (default 0.5).
#' @return Named list: \code{vec} (numeric vector) and \code{mat} (matrix).
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
#' @param x Character vector of column names.
#' @return Logical vector.
is_id_name <- function(x) {
  grepl("^id$|_id$|^id_", x, ignore.case = TRUE)
}

#' Replace NAs with column geometric mean of positive values
#' @param mat Numeric matrix.
#' @return Matrix with NAs replaced.
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
#' @param df Data frame.
#' @param parts Character vector of compositional part names.
#' @return Character vector of candidate factor names.
factor_candidates <- function(df, parts) {
  setdiff(names(df)[!sapply(df, is.numeric)], parts)
}

# ==============================================================================
# Manual ternary plot (ggplot2 only — no ggtern dependency)
# ==============================================================================

#' Build a ternary plot using plain ggplot2
#'
#' Projects compositional data onto an equilateral triangle via barycentric
#' coordinates. Supports both direct 3-part selection and amalgamation of D > 3
#' parts into 3 groups.
#'
#' @param data Data frame.
#' @param parts Character vector of length 3 (column names: left, top, right).
#' @param group_var Name of the grouping column.
#' @param amalg Named list of 3 character vectors (part names per group), or
#'   NULL to use \code{parts} directly.
#' @param base_size Base font size for the plot.
#' @return A ggplot object.
plot_ternary_manual <- function(data, parts, group_var,
                                amalg = NULL, base_size = 14) {

  h <- sqrt(3) / 2
  vL <- c(0, 0);  vT <- c(0.5, h);  vR <- c(1, 0)

  # -- Compute the three columns (direct or amalgamated) ----------------------
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

  # -- Triangle frame ---------------------------------------------------------
  tri <- data.frame(x = c(vL[1], vR[1], vT[1], vL[1]),
                    y = c(vL[2], vR[2], vT[2], vL[2]))

  # -- Grid lines at 20 % intervals ------------------------------------------
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

  # -- Tick labels ------------------------------------------------------------
  pct <- paste0(fracs * 100)
  off <- 0.04
  ticks <- rbind(
    data.frame(x = 1 - fracs, y = -off,                       label = pct,
               hj = 0.5, vj = 1),
    data.frame(x = 0.5 * fracs - off, y = fracs * h,          label = pct,
               hj = 1,   vj = 0.5),
    data.frame(x = 0.5 + 0.5 * fracs + off,
               y = (1 - fracs) * h,                            label = pct,
               hj = 0,   vj = 0.5)
  )

  # -- Assemble ---------------------------------------------------------------
  ggplot2::ggplot() +
    ggplot2::geom_line(data = grid,
                       ggplot2::aes(x = .data$x, y = .data$y, group = .data$gid),
                       colour = "grey85", linewidth = 0.25) +
    ggplot2::geom_polygon(data = tri, ggplot2::aes(x = .data$x, y = .data$y),
                          fill = NA, colour = "grey30", linewidth = 0.6) +
    ggplot2::geom_point(data = pts,
                        ggplot2::aes(x = .data$xc, y = .data$yc,
                                     colour = .data$group),
                        size = 3, alpha = 0.8) +
    ggplot2::geom_text(data = ticks,
                       ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                                    hjust = .data$hj, vjust = .data$vj),
                       size = base_size / 4, colour = "grey50") +
    ggplot2::annotate("text", x = vL[1], y = vL[2] - 0.08,
                      label = labels[1], fontface = "bold",
                      size = base_size / 3, hjust = 0.5) +
    ggplot2::annotate("text", x = vT[1], y = vT[2] + 0.06,
                      label = labels[2], fontface = "bold",
                      size = base_size / 3, hjust = 0.5) +
    ggplot2::annotate("text", x = vR[1], y = vR[2] - 0.08,
                      label = labels[3], fontface = "bold",
                      size = base_size / 3, hjust = 0.5) +
    ggplot2::labs(colour = group_var) +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(legend.position = "bottom",
                   plot.margin     = ggplot2::margin(20, 20, 30, 20))
}
