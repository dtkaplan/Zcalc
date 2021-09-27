#' Run the sandbox app locally
#'
#' @export
Command_trainer <- function() {
  shiny::runApp(
    system.file('Command_trainer', package='Zcalc')
  )
}
