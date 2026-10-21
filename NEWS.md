# CoDaStereo 3.1.0

## Data

* The four case-study datasets of the companion paper (spleen and kidney
  point counts per field, leukocyte differentials, erythrocyte nuclear
  abnormalities) are available as examples.
* Nested designs: fields of view can be aggregated into one composition per
  specimen (counts summed, areas averaged). A block term in the model
  restricts permutations within blocks.

## Visualisation

* Every figure can be exported as PDF, PNG, SVG or TIFF at a chosen size
  (cm), resolution and font size.
* The clr-biplot is drawn in ggplot2 (factoextra is no longer a dependency).
  It adds axis choice, shape by a second factor, sample labels, a scree
  plot and a table of ray lengths. It can include the log-total as an extra
  variable (T-space biplot; Pawlowsky-Glahn, Egozcue & Lovell 2015).
* New heatmap of the variation matrix.
* Ternary plots can show 95% confidence or predictive regions (ilr normal
  model) and group geometric centres, computed on the zero-replaced
  composition.
* Balances tab: boxplots of each balance per group and a permutational test
  of each balance (F, Z, P).
* Interactive mode: hovering shows the sample on the ternary plot and
  biplot (ggiraph).

## Statistics

* Numeric covariates (ANCOVA), block term, sums of squares of type I, II or
  III, and 199, 999 or 9999 permutations with a fixed seed.
* The log-total can enter as a covariate ("does composition change with
  size?") as well as a joint response.
* Pairwise tests also compare group dispersions (compositional disparity).

## Speed

* Model fits and pairwise tests run in background workers (mirai), so the
  interface stays responsive. Plots are cached. The SBP grid and
  amalgamation inputs are debounced. Progress bars show long steps. The
  deployment entry point starts faster.

## Reproducibility

* New Reproducibility tab with three outputs:
  * an R script that reproduces the analysis (tested to match the app's
    results);
  * a self-contained HTML report;
  * saving and reloading the session as JSON, with results recomputed on
    load.

## Interface

* The interface is available in English, Portuguese and Spanish
  (EN / PT / ES in the header), including the guided tour. Figures, the report and the R script
  stay in English.
* Package website built with pkgdown.

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
