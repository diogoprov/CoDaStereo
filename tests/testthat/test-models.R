sim_groups <- function(n = 15, shift = 1, seed = 3) {
  set.seed(seed)
  g <- factor(rep(c("a", "b", "c"), each = n))
  m <- cbind(A = stats::rlnorm(3 * n, 3 + shift * (g == "c")),
             B = stats::rlnorm(3 * n, 2), C = stats::rlnorm(3 * n, 1))
  list(x = compositions::acomp(m), g = g, cov = stats::rnorm(3 * n),
       blk = factor(rep(1:n, times = 3)))
}

test_that("model_rhs builds additive, factorial and covariate terms", {
  expect_equal(model_rhs("Site"), "`Site`")
  expect_equal(model_rhs(c("A", "B"), "*"), "`A` * `B`")
  expect_equal(model_rhs("A", covariates = "W", log_total_cov = TRUE),
               "`A` + `W` + log_total")
})

test_that("fit_models supports covariates, blocks, SS types and seeds", {
  s <- sim_groups()
  spec <- list(y = unclass(compositions::ilr(s$x)),
               data = data.frame(g = s$g, w = s$cov, blk = s$blk),
               rhs = model_rhs("g", covariates = "w"), iter = 199L, seed = 42,
               ss_type = "II", block = "blk", total_mode = "none", log_total = NULL)
  a <- fit_models(spec)
  b <- fit_models(spec)
  expect_s3_class(a$model, "lm.rrpp")
  expect_equal(rownames(a$anova$table)[1:2], c("g", "w"))
  expect_equal(a$anova$table, b$anova$table)  # same seed, same P-values
  expect_lt(a$anova$table["g", "Pr(>F)"], 0.05)
})

test_that("fit_pairwise returns location and dispersion summaries", {
  s <- sim_groups()
  spec <- list(y = unclass(compositions::ilr(s$x)), data = data.frame(g = s$g),
               rhs = "g", iter = 199L, seed = 1, ss_type = "I", block = NULL,
               total_mode = "none", log_total = NULL)
  pw <- fit_pairwise(list(model = fit_models(spec)$model, groups = s$g))
  expect_equal(nrow(pw$dist$summary.table), 3)
  expect_equal(nrow(pw$var$summary.table), 3)
})

test_that("fit_models runs in a mirai worker without the package attached", {
  skip_if_not_installed("mirai")
  skip_on_cran()
  s <- sim_groups()
  spec <- list(y = unclass(compositions::ilr(s$x)), data = data.frame(g = s$g),
               rhs = "g", iter = 99L, seed = 1, ss_type = "I", block = NULL,
               total_mode = "none", log_total = NULL)
  res <- mirai::collect_mirai(run_in_worker(fit_models, spec))
  expect_s3_class(res$model, "lm.rrpp")
})

test_that("covariate_candidates excludes parts, IDs and near-constant columns", {
  df <- data.frame(Animal_ID = 1:10, n_rows = 3, Weight = stats::rnorm(10),
                   A = 1:10, B = 10:1)
  expect_equal(covariate_candidates(df, c("A", "B")), "Weight")
})
