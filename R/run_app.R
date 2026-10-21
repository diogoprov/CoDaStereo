#' Run the CoDa Stereo Application
#'
#' @param onStart,options,enableBookmarking,uiPattern Passed to
#'   [shiny::shinyApp()].
#' @param ... Arguments passed to golem_opts.
#' @return A Shiny app object.
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {
  # Serve inst/logo at "logo/" (the root app.R serves www/ instead)
  logo_dir <- system.file("logo", package = "CoDaStereo")
  if (nzchar(logo_dir)) addResourcePath("logo", logo_dir)
  golem::with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = function() {
        start_workers()
        if (is.function(onStart)) onStart()
      },
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
