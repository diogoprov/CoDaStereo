# ==============================================================================
# Internationalisation (English / Portuguese / Spanish)
#
# The English text is the key. Translations live in inst/i18n/translations.csv
# (UTF-8, one column per language: en, pt, es), so R code stays ASCII. Figures, the HTML report
# and the generated R script stay in English: they are meant for publication.
# ==============================================================================

i18n_cache <- new.env(parent = emptyenv())

#' Languages offered in the interface (code = label shown in the header)
#' @noRd
app_languages <- c(en = "EN", pt = "PT", es = "ES")

#' Translation table (read once)
#' @noRd
i18n_table <- function() {
  if (is.null(i18n_cache$table)) {
    tab <- utils::read.csv(app_file("i18n", "translations.csv"), encoding = "UTF-8",
                           fileEncoding = "UTF-8", stringsAsFactors = FALSE,
                           na.strings = character(0))
    i18n_cache$table <- tab
  }
  i18n_cache$table
}

#' Translate English text (vectorised); falls back to English
#' @param x Character vector of English texts.
#' @param lang A code in `app_languages`.
#' @noRd
tr <- function(x, lang = "en") {
  tab <- i18n_table()
  if (identical(lang, "en") || !lang %in% names(tab)) return(x)
  out <- tab[[lang]][match(x, tab$en)]
  ifelse(is.na(out) | !nzchar(out), x, out)
}

#' Translate the names of a named choices vector (labels), keep the values
#' @noRd
tr_choices <- function(choices, lang = "en") {
  stats::setNames(unname(choices), tr(names(choices), lang))
}

#' Language from a URL query string ("?lang=pt", "?lang=es")
#' @noRd
lang_from_query <- function(qs) {
  l <- parseQueryString(qs %||% "")$lang
  if (!is.null(l) && l %in% names(app_languages)) l else "en"
}

#' Language of the current session (set in app_server); "en" in tests
#' @noRd
ui_lang <- function(session = getDefaultReactiveDomain()) {
  if (is.null(session)) return("en")
  session$userData$lang %||% "en"
}

#' Translator bound to the current session, for server code
#' @noRd
session_tr <- function(session = getDefaultReactiveDomain()) {
  function(x) tr(x, ui_lang(session))
}
