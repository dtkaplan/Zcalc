#' Run the Drill app locally
#'
#' @export
Drill <- function() {
  shiny::runApp(system.file('Drill', package='Zcalc'))
}
