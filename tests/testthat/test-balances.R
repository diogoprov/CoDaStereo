rand_comp <- function(D, n = 30, seed = 1) {
  set.seed(seed)
  m <- matrix(stats::rlnorm(n * D), n, D,
              dimnames = list(NULL, paste0("P", seq_len(D))))
  compositions::acomp(m)
}

test_that("automatic SBP is a valid orthonormal partition for D = 3..8", {
  for (D in 3:8) {
    sbp <- sbp_auto(rand_comp(D, seed = D))
    expect_equal(dim(sbp), c(D - 1, D))
    expect_equal(sbp_problems(sbp), character(0))
    expect_equal(sum(sbp[1, ] != 0), D)  # b1 is the root split
    V <- compositions::gsi.buildilrBase(t(sbp))
    expect_equal(unname(crossprod(V)), diag(D - 1), tolerance = 1e-10)
  }
})

test_that("sbp_problems flags invalid partitions", {
  sbp <- rbind(b1 = c(1, 1, -1, -1), b2 = c(1, -1, 0, 0), b3 = c(0, 0, 1, -1))
  colnames(sbp) <- c("A", "B", "C", "D")
  expect_equal(sbp_problems(sbp), character(0))

  one_sided <- sbp; one_sided["b2", ] <- c(1, 1, 0, 0)
  expect_match(sbp_problems(one_sided), "b2 needs at least one")

  crossed <- sbp; crossed["b3", ] <- c(1, 0, -1, 0)
  expect_match(sbp_problems(crossed), "not nested", all = FALSE)

  no_root <- sbp; no_root["b1", ] <- c(1, 1, -1, 0)
  expect_match(sbp_problems(no_root), "first split", all = FALSE)

  expect_match(sbp_problems(sbp[-3, ]), "need exactly 3 balances")
})

test_that("sbp_labels lists numerator and denominator parts", {
  sbp <- rbind(b1 = c(1, 1, -1), b2 = c(1, -1, 0))
  colnames(sbp) <- c("A", "B", "C")
  lab <- sbp_labels(sbp)
  expect_equal(lab$Numerator,   c("A, B", "A"))
  expect_equal(lab$Denominator, c("C", "B"))
})

test_that("balances module renders, validates edits and resets", {
  x5 <- rand_comp(5)
  df   <- data.frame(Group = factor(rep(c("A", "B", "C"), each = 10)),
                     as.data.frame(unclass(x5)))
  part_names <- colnames(x5)
  shiny::testServer(
    mod_balances_server,
    args = list(df = shiny::reactive(df), parts = shiny::reactive(part_names),
                comp = shiny::reactive(x5),
                keep_rows = shiny::reactive(seq_len(nrow(df)))),
    {
      session$setInputs(group_var = "Group", palette = "okabe_ito")
      expect_equal(sbp_current(), sbp_auto(x5))
      expect_no_error(output$dendrogram)

      # Make b2 one-sided: every non-zero cell becomes +1
      b2 <- sbp_current()[2, ]
      edits <- stats::setNames(
        as.list(rep("1", sum(b2 != 0))),
        paste0("sbp_2_", which(b2 != 0)))
      do.call(session$setInputs, edits)
      expect_true(length(sbp_issues()) > 0)
      expect_error(output$dendrogram, "Fix the SBP")
    }
  )
})

test_that("dendrogram draws without compositions attached", {
  comp <- rand_comp(4)
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  expect_no_error(plot_coda_dendrogram(comp, sbp_auto(comp),
                                       groups = rep(c("x", "y"), 15)))
  grDevices::dev.off()
  expect_null(getOption("robust"))  # option restored
})
