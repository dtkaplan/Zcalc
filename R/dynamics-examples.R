#' Examples of dynamical functions
#'
#' These are implementations of the dynamical functions for examples
#' shown in the text.
#' - `dt_rabbit()` and `dt_fox()` are for the rabbit/fox system
#'
#' @name dynmodels
#'
#'
#'
#' @param r, f, V, ... state variables.
#' @param alpha, beta, gamma, delta, ... parameters in the dynamical functions.
#'
#' @rdname dynmodels
#'
#' @export
dt_rabbit <- function(r, f, alpha=0.66, beta=1.33) {
  alpha*r - beta*r*f
}
#' @rdname dynmodels
#' @export
dt_fox <- function(r, f, delta=-1, beta=1) {
  delta*r - gamma*r*f
}
