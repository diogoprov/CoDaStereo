# Lists every English UI string passed to t(), tr() or tr_choices() in R/,
# plus the tour texts; used to maintain inst/i18n/translations.csv.
files <- list.files("R", pattern = "[.]R$", full.names = TRUE)
keys <- character(0)
for (f in files) {
  pd <- utils::getParseData(parse(f, keep.source = TRUE))
  calls <- pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% c("t", "tr", "tr_choices"), ]
  for (k in seq_len(nrow(calls))) {
    # the call expression is the grandparent of the function symbol
    expr_id <- pd$parent[pd$id == pd$parent[pd$id == calls$id[k]]]
    kids <- pd[pd$parent == expr_id, ]
    sub_ids <- expr_id
    repeat {  # all descendants
      new <- pd$id[pd$parent %in% sub_ids & !pd$id %in% sub_ids]
      if (!length(new)) break
      sub_ids <- c(sub_ids, new)
    }
    strs <- pd[pd$id %in% sub_ids & pd$token == "STR_CONST", ]
    if (calls$text[k] == "tr_choices") {
      # only the names (left of =) are labels
      eq <- pd[pd$id %in% sub_ids & pd$token == "EQ_SUB", ]
      is_name <- vapply(seq_len(nrow(strs)), \(r)
        any(eq$line1 == strs$line1[r] & eq$col1 > strs$col2[r] &
              eq$col1 - strs$col2[r] < 60), logical(1))
      strs <- strs[is_name, , drop = FALSE]
    }
    keys <- c(keys, vapply(strs$text, function(x) eval(parse(text = x)), character(1)))
  }
}
pkgload::load_all(quiet = TRUE)
keys <- c(keys, build_tour_steps("en")$intro, names(plot_palettes), "sum", "mean",
          "This file is not a CoDa Stereo session.", "The session has no data.",
          "The session's parts are not numeric columns of its data.")
keys <- unique(keys[nzchar(keys) & !grepl("^[[:punct:][:space:]0-9]*$", keys)])
writeLines(keys, "data-raw/i18n_keys.txt", useBytes = TRUE)
cat(length(keys), "keys\n")
