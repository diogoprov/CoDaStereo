# ==============================================================================
# Reproducibility: R script generator, HTML report, session state
# ==============================================================================

#' App version, from the installed package or the source DESCRIPTION
#' @noRd
app_version <- function() {
  tryCatch(as.character(utils::packageVersion("CoDaStereo")),
           error = function(e) unname(read.dcf("DESCRIPTION", "Version")[1, 1]))
}

#' Deparse a value as compact R code
#' @noRd
r_code <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = " ")

#' Generate an R script that reproduces the analysis in the app
#'
#' @param state Session state (see `collect_state()` in app_server).
#' @return Character vector, one element per line.
#' @noRd
build_r_script <- function(state) {
  d   <- state$data
  imp <- state$imputation
  st  <- state$stats
  out <- character(0)
  add <- function(...) out <<- c(out, ...)

  add(sprintf("# CoDa Stereo analysis script - generated %s with CoDaStereo %s",
              format(Sys.Date()), state$version),
      "# Reproduces the steps run in the app. Requires: compositions, zCompositions,",
      "# FactoMineR, RRPP and dplyr.",
      "",
      "library(compositions)",
      "options(robust = FALSE)",
      "")

  # -- 1. Data -----------------------------------------------------------------
  add("# 1. Data ----------------------------------------------------------------")
  src <- d$source
  if (identical(src$type, "example")) {
    ex <- example_datasets()[[src$example]]
    if (is.null(ex$file)) {
      add("df <- CoDaStereo:::simulate_liver()  # simulated example")
    } else {
      add(sprintf("df <- read.csv(system.file(\"extdata\", %s, package = \"CoDaStereo\"),",
                  r_code(ex$file)),
          "               stringsAsFactors = TRUE)")
    }
  } else if (identical(src$type, "excel")) {
    add(sprintf("df <- as.data.frame(readxl::read_excel(%s, sheet = %s, col_names = %s))",
                r_code(src$file_name), r_code(src$sheet %||% 1), r_code(src$header)))
  } else {
    add(sprintf("df <- read.csv(%s, header = %s, sep = %s, stringsAsFactors = TRUE)",
                r_code(src$file_name), r_code(src$header), r_code(src$sep)))
  }
  add(sprintf("parts <- %s", r_code(d$parts)), "")

  if (isTRUE(d$agg$on)) {
    add("# One composition per specimen (fields of view are not independent)",
        sprintf("df <- df |>\n  dplyr::group_by(%s) |>", d$agg$id),
        if (length(d$agg$keep)) {
          sprintf("  dplyr::summarise(dplyr::across(%s, dplyr::first),", r_code(d$agg$keep))
        } else "  dplyr::summarise(",
        sprintf("                   dplyr::across(dplyr::all_of(parts), %s), .groups = \"drop\") |>",
                d$agg$fun),
        "  as.data.frame()", "")
  }

  # -- 2. Missing values and zeros ---------------------------------------------
  add("# 2. Missing values and zeros ---------------------------------------------",
      "X <- as.matrix(df[, parts])")
  if (identical(imp$na_handling, "geomean")) {
    add("# Replace NAs with the column geometric mean of the positive values",
        "for (j in seq_len(ncol(X))) {",
        "  ok <- X[, j] > 0 & !is.na(X[, j])",
        "  X[is.na(X[, j]), j] <- exp(mean(log(X[ok, j])))",
        "}")
  }
  add("keep <- complete.cases(X)",
      "X <- X[keep, , drop = FALSE]",
      "df <- df[keep, , drop = FALSE]",
      "raw <- X  # observed amounts (used for the total)")
  method <- imp$method_used %||% "none"
  if (identical(method, "CZM")) {
    add("X <- zCompositions::cmultRepl(X, method = \"CZM\", output = \"p-counts\",",
        "                               label = 0, z.warning = 0.99)")
  } else if (method %in% c("multRepl", "lrEM")) {
    add("# Detection limit per part: half the smallest positive value",
        "dl <- apply(X, 2, function(x) max(0.5 * min(x[x > 0]), 1e-6))",
        "dl <- matrix(dl, nrow(X), ncol(X), byrow = TRUE)",
        if (method == "multRepl") {
          "X <- zCompositions::multRepl(X, label = 0, dl = dl, z.warning = 0.99)"
        } else {
          "X <- zCompositions::lrEM(X, label = 0, dl = dl, ini.cov = \"multRepl\", z.warning = 0.99)"
        })
  } else {
    add("# No zeros: the composition is used as observed")
  }
  add("comp <- acomp(X)", "")

  # -- 3. Exploration ----------------------------------------------------------
  add("# 3. Exploration -----------------------------------------------------------",
      "variation(comp)  # variances of all pairwise log-ratios",
      "# clr-PCA on centred, unscaled clr coordinates (basis of the clr-biplot)",
      "pca <- FactoMineR::PCA(as.data.frame(unclass(clr(comp))), scale.unit = FALSE,",
      "                       graph = FALSE)",
      "pca$eig", "")

  # -- 4. Balances -------------------------------------------------------------
  if (!is.null(state$balances$sbp)) {
    sbp <- state$balances$sbp
    add("# 4. Balances (sequential binary partition) ------------------------------",
        sprintf("sbp <- matrix(%s,", r_code(as.vector(t(sbp)))),
        sprintf("              nrow = %d, byrow = TRUE, dimnames = %s)", nrow(sbp),
                r_code(dimnames(sbp))),
        "balances <- ilr(comp, V = gsi.buildilrBase(t(sbp)))",
        if (!is.null(state$balances$group_var)) {
          sprintf("CoDaDendrogram(comp, signary = t(sbp), type = \"lines\")")
        },
        "")
  }

  # -- 5. Model ----------------------------------------------------------------
  if (length(st$factors)) {
    add("# 5. Permutational MANOVA on ilr coordinates (RRPP) ----------------------",
        "y <- unclass(ilr(comp))")
    tot <- st$total_mode %||% "none"
    if (tot != "none") {
      add("# log-total (Pawlowsky-Glahn, Egozcue & Lovell 2015)",
          if (identical(st$total_type, "product")) {
            "log_total <- rowSums(log(unclass(comp) * rowSums(raw))) / sqrt(ncol(raw))"
          } else "log_total <- log(rowSums(raw))")
      if (tot == "joint") add("y <- cbind(y, log_total = log_total)")
    }
    vars <- unique(c(st$factors, st$covariates, if (nzchar(st$block %||% "")) st$block))
    cols <- vapply(vars, \(v) {
      if (v %in% st$covariates) sprintf("%s = df[[%s]]", v, r_code(v))
      else sprintf("%s = factor(df[[%s]])", v, r_code(v))
    }, character(1), USE.NAMES = FALSE)
    if (tot == "covariate") cols <- c(cols, "log_total = log_total")
    add(sprintf("dat <- RRPP::rrpp.data.frame(y = y, %s)", paste(cols, collapse = ", ")),
        sprintf("fit <- RRPP::lm.rrpp(y ~ %s, data = dat, iter = %s, seed = 42,",
                st$rhs, st$iter),
        sprintf("                     SS.type = %s%s, print.progress = FALSE)",
                r_code(st$ss_type),
                if (nzchar(st$block %||% "")) sprintf(", block = dat$%s", st$block) else ""),
        "anova(fit)")
    if (tot == "joint") {
      add("# Total (size) alone",
          sprintf("anova(RRPP::lm.rrpp(log_total ~ %s, data = dat, iter = %s, seed = 42,",
                  st$rhs, st$iter),
          sprintf("                    SS.type = %s, print.progress = FALSE))",
                  r_code(st$ss_type)))
    }
    if (!is.null(st$pw_group)) {
      add("",
          sprintf("pw <- RRPP::pairwise(fit, groups = dat$%s)", st$pw_group),
          "summary(pw, test.type = \"dist\")  # differences in means",
          "summary(pw, test.type = \"var\")   # differences in dispersion")
    }
  }
  out
}

# ------------------------------------------------------------------------------
# HTML report (self-contained; no pandoc needed)
# ------------------------------------------------------------------------------

#' Embed a plot as a base64 PNG <img>
#' @noRd
report_img <- function(plot, width = 17, height = 13) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f))
  save_plot(f, plot, "png", width = width, height = height, dpi = 150)
  png <- readBin(f, "raw", file.info(f)$size)
  htmltools::tags$img(src = paste0("data:image/png;base64,", jsonlite::base64_enc(png)),
                      style = "max-width: 100%; height: auto;")
}

#' Captured printed output as <pre>
#' @noRd
report_pre <- function(x) htmltools::tags$pre(paste(utils::capture.output(print(x)), collapse = "\n"))

#' Data frame as an HTML table (numbers rounded to `digits`)
#' @noRd
report_table <- function(x, digits = 3) {
  x <- as.data.frame(x)
  cells <- lapply(x, \(v) if (is.numeric(v)) format(round(v, digits), trim = TRUE) else as.character(v))
  rn <- if (!identical(rownames(x), as.character(seq_len(nrow(x))))) rownames(x)
  tags <- htmltools::tags
  head <- tags$tr(if (!is.null(rn)) tags$th(""), lapply(names(x), tags$th))
  body <- lapply(seq_len(nrow(x)), \(i)
    tags$tr(if (!is.null(rn)) tags$td(rn[i]), lapply(cells, \(v) tags$td(v[i]))))
  tags$table(class = "tbl", tags$thead(head), tags$tbody(body))
}

#' Write a self-contained HTML report of the session
#'
#' @param state Session state.
#' @param file Output .html path.
#' @noRd
build_report <- function(state, file) {
  d    <- state$data
  comp <- state$imputation$result$comp
  kr   <- state$imputation$result$keep_rows
  an   <- state$analysis
  H    <- htmltools::tags
  sec  <- list()
  add  <- function(...) sec <<- c(sec, list(...))

  add(H$h1("CoDa Stereo report"),
      H$p(sprintf("Generated %s with CoDaStereo %s.", format(Sys.time(), "%Y-%m-%d %H:%M"),
                  state$version)),
      H$h2("Data"),
      H$p(sprintf("%d rows analysed; parts: %s.", nrow(d$df_out),
                  paste(d$parts, collapse = ", "))),
      if (isTRUE(d$agg$on)) H$p(sprintf("Rows aggregated by %s (%s).", d$agg$id, d$agg$fun)),
      H$p(sprintf("Zero replacement: %s. Missing values: %s.",
                  state$imputation$result$method_used %||% "not run",
                  state$imputation$result$na_action %||% "-")))

  if (!is.null(comp) && !is.null(an$group_var)) {
    groups <- d$df_out[[an$group_var]][kr]
    pca    <- clr_pca(comp)
    add(H$h2("Exploration"),
        H$h3("clr-biplot"),
        report_img(plot_clr_biplot(pca, groups, palette = an$palette %||% "okabe_ito")),
        report_table(ray_table(pca)),
        H$h3("Variation matrix"),
        report_img(plot_variation_matrix(comp)),
        H$h3("Compositional centre and total variance by group"),
        report_table(stats::reshape(comp_center_by_group(comp, as.factor(groups)),
                                    idvar = "Group", timevar = "Part", direction = "wide")),
        report_table(comp_totvar_by_group(comp, as.factor(groups)), digits = 4))
  }

  if (!is.null(state$balances$sbp) && !is.null(comp) && !is.null(state$balances$group_var)) {
    groups <- d$df_out[[state$balances$group_var]][kr]
    sbp    <- state$balances$sbp
    add(H$h2("Balances"),
        report_table(sbp_labels(sbp)),
        report_img(function() plot_coda_dendrogram(comp, sbp, groups,
                                                   state$balances$palette %||% "okabe_ito")),
        report_img(plot_balances(balance_coords(comp, sbp), groups, sbp,
                                 state$balances$palette %||% "okabe_ito")))
  }

  fit <- state$stats$fit
  if (!is.null(fit)) {
    add(H$h2("Permutational MANOVA"),
        H$p(sprintf("Model: y ~ %s; %s permutations; seed 42; SS type %s%s.",
                    fit$rhs, state$stats$iter, state$stats$ss_type,
                    if (nzchar(state$stats$block %||% "")) paste0("; block ", state$stats$block) else "")),
        report_pre(fit$anova))
    if (!is.null(fit$total_anova)) add(H$h3("Total (size) alone"), report_pre(fit$total_anova))
    if (!is.null(state$stats$pairwise)) {
      add(H$h3("Pairwise comparisons: means"), report_pre(state$stats$pairwise$dist),
          H$h3("Pairwise comparisons: dispersion"), report_pre(state$stats$pairwise$var))
    }
  }

  add(H$h2("R script"),
      H$p("Running this script reproduces the analysis outside the app."),
      H$pre(paste(build_r_script(state), collapse = "\n")))

  page <- htmltools::tags$html(
    H$head(H$meta(charset = "utf-8"), H$title("CoDa Stereo report"),
           H$style(paste(
             "body{font-family:Helvetica,Arial,sans-serif;max-width:900px;margin:2em auto;",
             "padding:0 1em;color:#222;line-height:1.45}",
             "pre{background:#f5f5f5;padding:.8em;overflow-x:auto;font-size:12px}",
             ".tbl{border-collapse:collapse;margin:.5em 0}",
             ".tbl td,.tbl th{border-bottom:1px solid #ddd;padding:3px 8px;text-align:right}",
             "h2{border-bottom:2px solid #E69F00;padding-bottom:3px}"))),
    H$body(sec))
  htmltools::save_html(page, file)
  invisible(file)
}

#' Serialise the session to JSON (data and settings only)
#'
#' Results (imputed composition, fitted models) are not stored: they are
#' recomputed on load, which is deterministic (fixed seed). JSON, unlike RDS,
#' cannot carry executable code, so loading an uploaded file is safe.
#' @noRd
session_to_json <- function(state) {
  df  <- state$data$df
  sbp <- state$balances$sbp
  keep_inputs <- function(x) x[setdiff(names(x), c("fit", "pairwise", "result"))]
  out <- list(
    kind = "CoDaStereo-session", version = state$version,
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    data = list(columns = lapply(df, function(x) if (is.factor(x)) as.character(x) else x),
                classes = lapply(df, function(x) class(x)[1]),
                parts = state$data$parts, source = state$data$source,
                agg = state$data$agg[c("on", "id", "fun")]),
    imputation = keep_inputs(state$imputation),
    analysis = state$analysis,
    balances = list(group_var = state$balances$group_var, palette = state$balances$palette,
                    sbp = if (!is.null(sbp)) list(values = as.vector(sbp), nrow = nrow(sbp),
                                                  rownames = rownames(sbp),
                                                  colnames = colnames(sbp))),
    stats = keep_inputs(state$stats))
  jsonlite::toJSON(out, auto_unbox = TRUE, null = "null", na = "null", digits = NA,
                   pretty = TRUE)
}

#' Read a session JSON back into a state list (validated)
#' @noRd
session_from_json <- function(txt) {
  s <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
  if (!is.list(s) || !identical(s$kind, "CoDaStereo-session")) {
    stop("This file is not a CoDa Stereo session.")
  }
  cols <- s$data$columns
  cls  <- s$data$classes
  if (!is.list(cols) || length(cols) == 0) stop("The session has no data.")
  df <- as.data.frame(Map(\(x, nm) {
    x <- unlist(lapply(x, function(v) if (is.null(v)) NA else v))
    switch(cls[[nm]] %||% "character",
           factor = factor(x), numeric = as.numeric(x), integer = as.integer(x),
           logical = as.logical(x), as.character(x))
  }, cols, names(cols)), stringsAsFactors = FALSE, check.names = FALSE)
  parts <- s$data$parts
  if (!all(parts %in% names(df)) || !all(vapply(df[parts], is.numeric, logical(1)))) {
    stop("The session's parts are not numeric columns of its data.")
  }
  sb <- s$balances$sbp
  if (!is.null(sb)) {
    sb <- matrix(as.numeric(sb$values), nrow = sb$nrow,
                 dimnames = list(sb$rownames, sb$colnames))
  }
  list(kind = s$kind, version = s$version,
       data = list(df = df, parts = parts, source = s$data$source, agg = s$data$agg),
       imputation = s$imputation, analysis = s$analysis,
       balances = list(group_var = s$balances$group_var, palette = s$balances$palette, sbp = sb),
       stats = s$stats)
}

#' One-shot restored values for inputs created by renderUI
#'
#' Returns a function `take(name)`: the restored value the first time it is
#' asked for, NULL afterwards, so later re-renders fall back to defaults.
#' @param restore Reactive holding a loaded session state (or NULL).
#' @param key Module key in the state (e.g. "analysis").
#' @noRd
restore_pending <- function(restore, key) {
  pending <- reactiveValues()
  observeEvent(restore(), {
    st <- restore()[[key]]
    for (n in names(st)) pending[[n]] <- st[[n]]
  }, priority = 100)
  function(name) {
    isolate({
      v <- pending[[name]]
      if (!is.null(v)) pending[[name]] <- NULL
      v
    })
  }
}
