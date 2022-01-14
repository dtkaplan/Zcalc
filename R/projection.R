#' Utilities for vector calculations
#'
#' `%dot%`, `%onto%`, and `%perp%` are infix operators.
#' The left-hand argument is a vector. For ``
#'
#' Convenience functions for basic operations relating to vector projection. These use
#' the Zcalc conventions that require vectors to be one column matrices.
#'
#' @param A a matrix
#' @param b a column vector
#' @param u a row vector, but a column vector is acceptable too
#' @param v like `u`
#'
#' @export
`%dot%` <- function(u, A) {
  if (inherits(A, "matrix")) {
    if (nrow(A) != length(u))
      stop("Matrix A must have the right number of rows to match vector u.")
  } else {
    if (length(A) != length(u))
      stop("A and u must have the same number of arguments.")
    A <- cbind(A)
  }

  u <- t(rbind(u)) # make sure <u> is a row vector.

  u %*% A
}
#' @export
`%onto%` <- function(b, A) {
  check_A_b(A, b)
  A %*% qr.solve(A, b)
}
#' @export
`%perp%` <- function(b, A) {
  check_A_b(A, b)
  b - (b %onto% A)
}
#' @export
normalize <- function(A) {
  check_A_b(A)
  helper <- function(v) { v / sqrt(sum(v^2)) }
  apply(A, 2, helper)
}
#' @export
as_magnitude <- function(A, method=c("2", "O", "I", "F", "M")) {
  check_A_b(A)
  method <- match.arg(method)
  helper <- function(v) {Matrix::norm(v, type=method)}

  apply(A, 2, helper)
}

#' helper function
check_A_b <- function(A, b) {
  if (!inherits(A, "matrix"))
    stop("Argument <A> must be a matrix.")
  if (!missing(v)) {
    if (!inherits(b, "matrix"))
      stop("Argument <b> must be a column vector.")
    if (nrow(b) != nrow(A))
      stop("Matrix A and vector b must have the same number of rows.")
    A %*% qr.solve(A, b)
  }

  return(TRUE)
}
