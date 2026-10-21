# Getting started with CoDa Stereo

CoDa Stereo is a ‘shiny’ application for compositional data analysis
(CoDA) of stereological, histomorphometric and blood-cell count data.
Volume fractions from point counting, area percentages and differential
cell counts carry only relative information: each part is meaningful
only relative to the others. CoDa Stereo analyses these data with
log-ratios (the Aitchison geometry) and does not apply standard
statistics to raw percentages.

## Launching the app

``` r

library(CoDaStereo)
run_app()
```

Follow the sidebar from top to bottom. The guided tour (**Tour** button)
walks through each step.

## Workflow

1.  **Data Upload.** Load a CSV or Excel file and select the columns
    that form the composition (the *parts*). Non-numeric columns such as
    treatment or site become candidate grouping factors. A pre-flight
    check flags parts on an incompatible scale. Five examples are built
    in: a simulated liver dataset and the four case studies of the
    companion paper.

    When rows are fields of view nested within specimens (for example
    three fields per animal), tick *Aggregate rows by specimen*: fields
    of the same animal are not independent replicates. Point counts are
    summed; areas or proportions are averaged.

2.  **Data Inspection.** A three-state heatmap distinguishes observed
    values, zeros and missing values.

3.  **Zero Handling.** Log-ratios are undefined for zeros, so zeros are
    replaced before analysis. Use *CZM* for count zeros (point
    counting), and *multRepl* or *lrEM* for rounded zeros (values below
    detection). Missing values can be dropped or replaced by the column
    geometric mean.

4.  **Compositional Analysis.** Ternary diagrams (with optional
    amalgamation when there are more than three parts, and 95%
    confidence or predictive regions per group), the clr-biplot, the
    variation matrix, and the compositional centre and total variance
    per group. Open *How to read this biplot* for guidance on
    interpreting rays and links. Every figure can be exported as PDF,
    PNG, SVG or TIFF.

5.  **Balances (SBP).** The automatic sequential binary partition can be
    edited cell by cell. The CoDa dendrogram shows the mean and variance
    of each balance and its distribution per group; each balance can be
    tested between groups.

6.  **Statistical Modeling.** Permutational MANOVA on ilr coordinates
    ([`RRPP::lm.rrpp()`](https://rdrr.io/pkg/RRPP/man/lm.rrpp.html))
    with factors, covariates and a block term, pairwise comparisons of
    means and dispersions, and residual diagnostics.

7.  **Reproducibility.** Download an R script that reproduces the
    analysis, a self-contained HTML report, or save the session to
    resume it later.

The interface is available in English, Portuguese and Spanish (*EN / PT
/ ES* in the header); a link can open the app directly in one language,
e.g. `?lang=es`.

## When to include the total

By default only the composition is analysed, which discards the total.
When parts are measured on an **absolute** scale (for example,
compartment volumes obtained as volume fraction times Cavalieri organ
volume, or cells per microlitre), the total can carry biological
information. Choose *Total (size)* in the Statistical Modeling tab to
analyse the log-total jointly with the ilr coordinates, or as a
covariate to ask whether the composition changes with size. The sum of
parts (Ts) is the default; the product total (Tp) is equivalent to
analysing the log of each part (Pawlowsky-Glahn, Egozcue and Lovell
2015, <doi:10.1177/1471082X14535526>).

In point-counting stereology the total is usually **fixed by design**:
the same number of grid points, fields or cells is counted for every
specimen. That total carries no biological information and should not be
included. The app warns when totals are almost constant.

## Deployment

The repository root contains an `app.R` entry point and a
`manifest.json` for deployment on Posit Connect Cloud.
