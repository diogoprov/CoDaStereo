test_that("tr() translates to Portuguese and falls back to English", {
  expect_equal(tr("Run Model", "pt"), "Ajustar modelo")
  expect_equal(tr("Run Model", "en"), "Run Model")
  expect_equal(tr("not a known string", "pt"), "not a known string")
  expect_equal(tr(c("Next", "Back"), "pt"), c("Próximo", "Voltar"))
  ch <- tr_choices(c("Sum (counts)" = "sum"), "pt")
  expect_equal(unname(ch), "sum")
  expect_equal(names(ch), "Soma (contagens)")
})

test_that("printf templates keep their placeholders in Portuguese", {
  tab <- i18n_table()
  ph  <- function(x) regmatches(x, gregexpr("%[.0-9]*[sdf%]", x))
  expect_equal(ph(names(tab)), ph(unname(tab)))
  expect_match(sprintf(tr("%d parts need exactly %d balances.", "pt"), 4L, 3L), "4 partes")
})

test_that("the UI renders in both languages", {
  en <- as.character(app_ui(list(QUERY_STRING = "")))
  pt <- as.character(app_ui(list(QUERY_STRING = "?lang=pt")))
  expect_match(en, "Data Upload")
  expect_match(pt, "Carregar dados")
  expect_match(pt, "Modelagem estatística")
  expect_false(grepl("Statistical Modeling<", pt, fixed = TRUE))
})

test_that("every tour step is translated", {
  en <- build_tour_steps("en")
  pt <- build_tour_steps("pt")
  expect_equal(nrow(pt), nrow(en))
  expect_true(all(pt$intro != en$intro))
})

test_that("SBP and pre-flight messages are translated", {
  sbp <- rbind(b1 = c(1, 1, -1), b2 = c(1, 1, 0))
  colnames(sbp) <- c("A", "B", "C")
  expect_match(sbp_problems(sbp, lang = "pt"), "precisa de pelo menos")
  chk <- preflight_check(data.frame(A = 1), "A", lang = "pt")
  expect_match(chk$warnings, "pelo menos 2 partes")
})
