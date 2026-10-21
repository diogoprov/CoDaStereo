# Composition + total (Pawlowsky-Glahn, Egozcue & Lovell 2015)

abs_data <- function(n = 20, size_ratio = 3, seed = 7) {
  set.seed(seed)
  base <- c(A = 50, B = 30, C = 15, D = 5)
  # identical expected composition; group 2 is size_ratio times larger
  size <- rep(c(1, size_ratio), each = n)
  m <- t(vapply(seq_len(2 * n), \(i)
    base * size[i] * exp(stats::rnorm(4, 0, 0.15)), numeric(4)))
  colnames(m) <- names(base)
  list(raw = m, group = factor(rep(c("small", "large"), each = n)))
}

test_that("Tp coordinates are isometric to the log of each part (Prop. 4.2)", {
  m <- abs_data()$raw
  x <- compositions::acomp(m)
  coords <- cbind(unclass(compositions::ilr(x)),
                  log_total_coord(m, x, "product"))
  expect_equal(as.matrix(stats::dist(coords)),
               as.matrix(stats::dist(log(m))), tolerance = 1e-8)
})

test_that("Ts distance = squared log-ratio of sums + Aitchison distance (eq. 3.2)", {
  m <- abs_data()$raw
  x <- compositions::acomp(m)
  coords <- cbind(unclass(compositions::ilr(x)), log_total_coord(m, x, "sum"))
  d2_T <- sum((coords[1, ] - coords[2, ])^2)
  d2_a <- sum((unclass(compositions::clr(x))[1, ] -
               unclass(compositions::clr(x))[2, ])^2)
  expect_equal(d2_T, log(sum(m[1, ]) / sum(m[2, ]))^2 + d2_a, tolerance = 1e-10)
})

test_that("total_cv flags totals fixed by design", {
  fixed <- t(apply(matrix(stats::runif(30), 10), 1, \(r) 120 * r / sum(r)))
  expect_lt(total_cv(fixed), 1e-8)
  expect_gt(total_cv(abs_data()$raw), 0.3)
})

test_that("stats module detects a size-only effect only when the total is included", {
  d    <- abs_data()
  x    <- compositions::acomp(d$raw)
  df   <- data.frame(Group = d$group, d$raw)
  shiny::testServer(
    mod_stats_server,
    args = list(df = shiny::reactive(df), parts = shiny::reactive(colnames(d$raw)),
                comp = shiny::reactive(x),
                keep_rows = shiny::reactive(rep(TRUE, nrow(df))),
                raw = shiny::reactive(d$raw)),
    {
      session$setInputs(factors = "Group", total_mode = "none", iter = "999",
                        ss_type = "I", block = "", total_type = "sum")
      session$setInputs(run_manova = 1)
      p_comp <- rv$fit$anova$table[1, "Pr(>F)"]
      expect_null(rv$fit$total_anova)

      session$setInputs(total_mode = "joint")
      session$setInputs(run_manova = 2)
      p_joint <- rv$fit$anova$table[1, "Pr(>F)"]
      p_total <- rv$fit$total_anova$table[1, "Pr(>F)"]
      expect_equal(ncol(rv$fit$model$LM$Y), ncol(d$raw))  # D - 1 ilr + log-total

      expect_gt(p_comp, 0.05)
      expect_lt(p_joint, 0.01)
      expect_lt(p_total, 0.01)
      expect_no_error(output$total_plot)
      expect_null(output$total_warning$html)

      session$setInputs(total_mode = "covariate")
      session$setInputs(run_manova = 3)
      expect_match(rv$fit$rhs, "log_total")
      expect_equal(ncol(rv$fit$model$LM$Y), ncol(d$raw) - 1)
    }
  )
})
