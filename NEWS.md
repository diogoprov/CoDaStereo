# CoDaStereo 3.0.0

## New features

* New **Balances (SBP)** tab. A sequential binary partition is built
  automatically by Ward clustering of the parts on clr distances. Users can
  edit any cell (+1 / -1 / 0) or reset to the automatic SBP. Edits are
  validated on the fly (D - 1 balances, two-sided balances, one root split,
  nested partitions). The tab shows the balance definitions and a CoDa
  dendrogram (`compositions::CoDaDendrogram()`) with per-group boxplots of
  each balance. The dendrogram downloads as PDF and the SBP as CSV.

* **Composition + total** in the permutational MANOVA. The log-total can be
  added as an extra response coordinate next to the ilr coordinates,
  following Pawlowsky-Glahn, Egozcue & Lovell (2015, *Statistical
  Modelling* 15:175-190, doi:10.1177/1471082X14535526). Two totals are
  available: the sum of parts (space Ts, the default) and the product of
  parts to the power 1/sqrt(D) (space Tp, isometric to analysing the log of
  each part). A separate univariate model and a boxplot of the log-total
  separate size effects from compositional effects. The app warns when the
  totals are almost constant (CV < 5%), as happens when the number of grid
  points or cells counted is fixed by the sampling design.

* **Selectable colour palettes** (Okabe-Ito, ColorBrewer Dark2, viridis) for
  the ternary plot, clr-biplot and compositional descriptives.

* A collapsible **"How to read this biplot"** note explains rays, links,
  link angles and the limits of the planar approximation.

* New hex sticker.

## Changes that affect results

* The clr-biplot now runs PCA on centred, **unscaled** clr coordinates.
  Earlier versions called `FactoMineR::PCA()` with its default
  `scale.unit = TRUE`, which standardised the clr variables. That equalised
  ray lengths, so links no longer approximated log-ratio standard
  deviations. Biplots produced with 2.x will differ.

## Bug fixes

* The compositional descriptives and the CoDa dendrogram no longer fail
  when the app runs from the installed package (`run_app()`).
  `compositions` reads `getOption("robust")`, which is only set when the
  package is attached.
* The logo is now shown when the app is launched with `run_app()`.
* The deployment entry point (`app.R`) loads `rintrojs` instead of the
  removed `cicerone`.

## Package infrastructure

* The package passes `R CMD check` with no errors, warnings or notes:
  roxygen2 documentation, a complete `NAMESPACE`, and declared imports.
  Continuous integration was re-enabled on Linux, macOS and Windows.
* Added a testthat suite (palettes, SBP validity, composition + total
  geometry, clr-biplot geometry, and module smoke tests on simulated data).
* Added a getting-started vignette.
* `manifest.json` was regenerated for Posit Connect Cloud.

# CoDaStereo 2.1.0

* Guided tour (now based on `rintrojs`), pre-flight check of selected
  parts, compositional descriptives per group, convex hulls on ternary
  plots and biplots, and a logo.
