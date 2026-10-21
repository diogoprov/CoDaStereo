# Run the CoDa Stereo Application

Run the CoDa Stereo Application

## Usage

``` r
run_app(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
)
```

## Arguments

- onStart, options, enableBookmarking, uiPattern:

  Passed to
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

- ...:

  Arguments passed to golem_opts.

## Value

A Shiny app object.

## Examples

``` r
if (interactive()) {
  run_app()
}
```
