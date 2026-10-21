test_that("clr-PCA is unscaled: rays and links reproduce clr and log-ratio SDs", {
  set.seed(1)
  m <- matrix(stats::rlnorm(200, sdlog = rep(c(0.2, 0.5, 1, 1.5), each = 50)),
              50, 4, dimnames = list(NULL, c("A", "B", "C", "D")))
  x   <- compositions::acomp(m)
  res <- clr_pca(x)
  v   <- res$var$coord  # all D - 1 non-null components retained

  # FactoMineR uses 1/n weights, so compare with population SDs
  pop_sd <- function(z) sqrt(mean((z - mean(z))^2))
  clr_sd <- apply(unclass(compositions::clr(x)), 2, pop_sd)
  expect_equal(unname(sqrt(rowSums(v^2))), unname(clr_sd), tolerance = 1e-6)

  link_AD <- sqrt(sum((v["A", ] - v["D", ])^2))
  expect_equal(link_AD, pop_sd(log(m[, "A"] / m[, "D"])), tolerance = 1e-6)
})
