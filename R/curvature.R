#' Transform a function into it's curvature function
#'
#' curvature works much like `D()`, but instead of returning a function
#' containing the first or second derivative, the function instead
#' gives the curvature of the parent function.
#'
#' @param ftilde A tilde expression describing the function and the
#' variable with respect to which the curvature should be computed.
#'
#' @details Works only for functions of a single variable.
#'
#' Curvature is 1/radius of the tangent circle that can be inscribed
#' in the function at any given input.
#'
#' @return A function with the same argument as that described by the
#' tilde expression argument.
#'
#' @examples
#' cfun <- curvature(sin(x) ~ x)
#'
#' @export
curvature <- function(ftilde) {
  ff <- makeFun(ftilde)
  df <- mosaicCalc::D(ff(x) ~ x)
  ddf <- mosaicCalc::D(ff(x) ~ x & x)
  Kfun <- function(x) {
    abs(ddf(x)) / abs(1+ df(x)^2)^(3/2)
  }

  Kfun
}
