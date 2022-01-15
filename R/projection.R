#' Utilities for vector calculations
#'
#' `%dot%`, `%onto%`, and `%perp%` are infix operators.
#' The left-hand argument is a vector.
#'
#' Convenience functions for basic operations relating to vector projection. These use
#' the Zcalc conventions that require vectors to be one column matrices.
#'
#'
#' @param A a matrix
#' @param b a column vector
#' @param u a row vector, but a column vector is acceptable too
#' @param v like `u`
#'
#' @name vectors
NULL
#> NULL
#'
#' @rdname vectors
#' @export
`%dot%` <- function(u, A) {
  if (!inherits(u, "matrix")) {
    u <- matrix(u, nrow=1)
  }
  if (inherits(A, "matrix")) {
    if (nrow(A) != nrow(u))
      stop("Matrix A must have the right number of rows to match vector u.")
  } else {
    if (length(A) != ncol(u))
      stop("A and u must have the same number of elements.")
    A <- matrix(A, ncol=1)
  }

  u %*% A
}
#' @rdname vectors
#' @export
`%onto%` <- function(b, A) {
  b <- matrix(b, ncol=1) # force a column vector
  if (!is.matrix(A))
    A <- matrix(A, nrow=nrow(b))
  else if (nrow(A) != nrow(b))
    stop("<A> must have same number of rows as <b>.")
  A %*% qr.solve(A, b)
}
#' @rdname vectors
#' @export
`%perp%` <- function(b, A) {
  b <- matrix(b, ncol=1) # force a column vector
  b - (b %onto% A)
}
#' @rdname vectors
#' @export
normalize <- function(A) {
  if (!inherits(A, "matrix"))
    stop("Argument <A> must be a matrix.")
  helper <- function(v) { v / sqrt(sum(v^2)) }
  apply(A, 2, helper)
}
#' @rdname vectors
#' @export
as_magnitude <- function(A, method=c("2", "O", "I", "F", "M")) {
  if (!inherits(A, "matrix"))
    stop("Argument <A> must be a matrix.")
  method <- match.arg(method)
  helper <- function(v) {Matrix::norm(v, type=method)}

  apply(A, 2, helper)
}
