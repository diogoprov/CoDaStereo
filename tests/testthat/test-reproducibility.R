# Drive the whole app server on the spleen example
run_spleen_session <- function(code) {
  out <- new.env()
  shiny::testServer(app_server, {
    parts <- example_datasets()$spleen$parts
    session$setInputs(`data-example` = "spleen", `data-load_example` = 1)
    session$setInputs(`data-parts` = parts, `data-agg_on` = TRUE,
                      `data-agg_id` = "Specimen", `data-agg_fun` = "sum")
    session$setInputs(`imputation-zero_method` = "CZM", `imputation-na_handling` = "drop",
                      `imputation-run` = 1)
    session$setInputs(`analysis-group_var` = "Site", `analysis-palette` = "okabe_ito",
                      `analysis-tern_mode` = "direct", `analysis-tern_vars` = parts[2:4],
                      `analysis-tern_regions` = "hull")
    session$setInputs(`balances-group_var` = "Site", `balances-palette` = "okabe_ito")
    session$elapse(600)
    session$setInputs(`stats-factors` = "Site", `stats-iter` = "999", `stats-ss_type` = "I",
                      `stats-block` = "", `stats-total_mode` = "none",
                      `stats-total_type` = "sum", `stats-pw_group` = "Site")
    session$setInputs(`stats-run_manova` = 1)
    session$setInputs(`stats-run_pairwise` = 1)
    out$value <- eval(code)
  })
  out$value
}

test_that("the generated R script reproduces the app's MANOVA", {
  run_spleen_session(quote({
    st <- session_state()
    expect_equal(nrow(st$data$df_out), 37)
    script <- build_r_script(st)
    expect_true(any(grepl("lm.rrpp", script)))
    # run the script outside the app (example file read from the source tree)
    script <- sub('system.file("extdata", "spleen_fields.csv", package = "CoDaStereo")',
                  shQuote(app_file("extdata", "spleen_fields.csv")), script, fixed = TRUE)
    env <- new.env(parent = globalenv())
    suppressMessages(eval(parse(text = script), envir = env))
    app_tab    <- st$stats$fit$anova$table
    script_tab <- stats::anova(env$fit)$table
    expect_equal(script_tab[, c("Df", "SS", "F", "Pr(>F)")],
                 app_tab[, c("Df", "SS", "F", "Pr(>F)")], tolerance = 1e-10)
    expect_equal(unname(unclass(env$comp)), unname(unclass(st$imputation$result$comp)),
                 tolerance = 1e-10)
  }))
})

test_that("the HTML report is written and self-contained", {
  run_spleen_session(quote({
    f <- tempfile(fileext = ".html")
    build_report(session_state(), f)
    html <- paste(readLines(f, warn = FALSE), collapse = "\n")
    expect_match(html, "Permutational MANOVA")
    expect_match(html, "data:image/png;base64")
    expect_match(html, "Pairwise comparisons: dispersion")
  }))
})

test_that("a session survives a JSON round trip", {
  run_spleen_session(quote({
    st   <- session_state()
    back <- session_from_json(session_to_json(st))
    expect_equal(back$data$df, st$data$df, ignore_attr = TRUE)
    expect_equal(back$data$parts, st$data$parts)
    expect_true(back$data$agg$on)
    expect_equal(back$balances$sbp, st$balances$sbp)
    expect_equal(back$stats$factors, "Site")
    expect_true(back$stats$fitted)
    expect_true(back$stats$pairwise_run)
  }))
})

test_that("loading a session restores data and recomputes the model", {
  json <- run_spleen_session(quote(session_to_json(session_state())))
  shiny::testServer(app_server, {
    restore(session_from_json(json))
    session$flushReact()
    expect_equal(nrow(data_r$df()), 111)  # raw rows restored
    # the browser would echo the restored inputs back; simulate it
    session$setInputs(`data-parts` = example_datasets()$spleen$parts,
                      `data-agg_on` = TRUE, `data-agg_id` = "Specimen", `data-agg_fun` = "sum",
                      `imputation-zero_method` = "CZM", `imputation-na_handling` = "drop")
    expect_equal(nrow(impute_r$comp()), 37)  # imputation re-run
    session$setInputs(`stats-factors` = "Site", `stats-iter` = "999", `stats-ss_type` = "I",
                      `stats-block` = "", `stats-total_mode` = "none", `stats-pw_group` = "Site")
    expect_s3_class(stats_r$fit()$model, "lm.rrpp")
    expect_false(is.null(stats_r$pairwise()))
  })
})

test_that("invalid session files are rejected", {
  expect_error(session_from_json('{"kind": "other"}'), "not a CoDa Stereo session")
  bad <- '{"kind": "CoDaStereo-session", "data": {"columns": {"a": ["x"]},
          "classes": {"a": "character"}, "parts": ["a"]}}'
  expect_error(session_from_json(bad), "not numeric")
})
