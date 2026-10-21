test_that("every example dataset loads with its declared parts", {
  for (k in names(example_datasets())) {
    d  <- load_example(k)
    ex <- example_datasets()[[k]]
    expect_true(all(ex$parts %in% names(d)), info = k)
    expect_true(all(purrr::map_lgl(d[ex$parts], is.numeric)), info = k)
    expect_gte(length(factor_candidates(d, ex$parts)), 1)
  }
})

test_that("aggregate_specimens sums fields and drops non-constant columns", {
  df <- data.frame(Specimen = c("a", "a", "b"), Site = c("x", "x", "y"),
                   Field = 1:3, P1 = c(1, 2, 3), P2 = c(10, 20, 30))
  out <- aggregate_specimens(df, "Specimen", c("P1", "P2"), "sum")
  expect_equal(out$P1, c(3, 3))
  expect_equal(out$P2, c(30, 30))
  expect_equal(out$Site, c("x", "y"))
  expect_equal(out$n_rows, c(2L, 1L))
  expect_equal(attr(out, "dropped"), "Field")

  avg <- aggregate_specimens(df, "Specimen", c("P1", "P2"), "mean")
  expect_equal(avg$P1, c(1.5, 3))
})

test_that("spleen fields aggregate to one row per animal", {
  ex  <- example_datasets()$spleen
  out <- aggregate_specimens(load_example("spleen"), ex$specimen, ex$parts)
  expect_equal(nrow(out), 37)
  expect_equal(factor_candidates(out, ex$parts), "Site")
})

test_that("data module loads an example and aggregates it", {
  shiny::testServer(mod_data_server, {
    session$setInputs(example = "spleen", load_example = 1)
    session$setInputs(parts = example_datasets()$spleen$parts,
                      agg_on = TRUE, agg_id = "Specimen", agg_fun = "sum")
    expect_equal(nrow(df_out()), 37)
    session$setInputs(agg_on = FALSE)
    expect_equal(nrow(df_out()), 111)
  })
})
