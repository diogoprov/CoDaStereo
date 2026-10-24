# ==============================================================================
# Model fitting (pure functions, run in background workers via mirai)
#
# fit_models() and fit_pairwise() only call fully qualified functions, so they
# can be shipped to a mirai worker that does not have CoDaStereo installed
# (the root app.R runs the app from source).
# ==============================================================================

#' Right-hand side of the RRPP model formula
#'
#' @param factors Character vector of factor names.
#' @param design "+" (additive) or "*" (factorial) between factors.
#' @param covariates Character vector of numeric covariate names (additive).
#' @param log_total_cov Logical: add the log-total as a covariate.
#' @noRd
model_rhs <- function(factors, design = "+", covariates = character(0),
                      log_total_cov = FALSE) {
  bt    <- function(x) paste0("`", x, "`")
  terms <- paste(bt(factors), collapse = paste0(" ", design, " "))
  extra <- c(if (length(covariates)) bt(covariates), if (log_total_cov) "log_total")
  paste(c(terms, extra), collapse = " + ")
}

#' Fit the permutational MANOVA (and the total-only model when requested)
#'
#' @param spec List with elements `y` (ilr matrix), `data` (data frame of
#'   factors, covariates and block), `rhs` (from `model_rhs()`), `iter`,
#'   `seed`, `ss_type`, `block` (column name or NULL), `total_mode`
#'   ("none", "joint" or "covariate") and `log_total` (numeric or NULL).
#' @return List with `model`, `anova`, `rhs`, and for `total_mode = "joint"`
#'   also `total_anova` and `total_df`.
#' @noRd
fit_models <- function(spec) {
  y <- spec$y
  d <- spec$data
  if (spec$total_mode == "joint")     y <- cbind(y, log_total = spec$log_total)
  if (spec$total_mode == "covariate") d$log_total <- spec$log_total
  blk  <- if (!is.null(spec$block)) d[[spec$block]]
  fmla <- stats::as.formula(paste("y ~", spec$rhs))
  fit_one <- function(resp) {
    rdf <- do.call(RRPP::rrpp.data.frame, c(list(y = resp), as.list(d)))
    RRPP::lm.rrpp(fmla, data = rdf, iter = spec$iter, seed = spec$seed,
                  block = blk, SS.type = spec$ss_type, print.progress = FALSE)
  }
  model <- fit_one(as.matrix(y))
  out <- list(model = model, anova = stats::anova(model), rhs = spec$rhs)
  if (spec$total_mode == "joint") {
    out$total_anova <- stats::anova(fit_one(spec$log_total))
    out$total_df    <- data.frame(group = d[[1]], log_total = spec$log_total)
  }
  out
}

#' Pairwise comparisons of location (distances) and dispersion (variances)
#'
#' @param arg List with `model` (a fitted `lm.rrpp`) and `groups` (factor).
#' @return List with the pairwise object and the two summaries.
#' @noRd
fit_pairwise <- function(arg) {
  pw <- RRPP::pairwise(arg$model, groups = arg$groups)
  list(pw   = pw,
       dist = summary(pw, test.type = "dist"),
       var  = summary(pw, test.type = "var"))
}

#' Numeric columns that can act as covariates (not parts, not IDs, not constant)
#' @noRd
covariate_candidates <- function(df, parts) {
  num <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], parts)
  Filter(\(v) !is_id_name(v) && dplyr::n_distinct(df[[v]]) >= 3, num)
}

# ------------------------------------------------------------------------------
# Background workers
# ------------------------------------------------------------------------------

#' Should models run asynchronously? (off in tests via options())
#' @noRd
use_async <- function() {
  isTRUE(getOption("CoDaStereo.async", TRUE)) && requireNamespace("mirai", quietly = TRUE)
}

#' Start persistent mirai workers for the lifetime of the app
#'
#' A persistent worker avoids paying the R start-up and RRPP load (about 1 s)
#' on every model fit.
#' @noRd
start_workers <- function(n = 2) {
  if (!use_async()) return(invisible(FALSE))
  ok <- tryCatch({
    if (mirai::status()$connections == 0) mirai::daemons(n)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    # e.g. a host that does not allow extra processes: fit synchronously
    options(CoDaStereo.async = FALSE)
    return(invisible(FALSE))
  }
  shiny::onStop(function() mirai::daemons(0))
  invisible(TRUE)
}

#' Run `fun(arg)` in a mirai worker (returns a mirai, usable by ExtendedTask)
#' @noRd
run_in_worker <- function(fun, arg) {
  environment(fun) <- globalenv()
  mirai::mirai(fun(arg), fun = fun, arg = arg)
}
