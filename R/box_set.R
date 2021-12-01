#' Evenly spaced samples across a one- or two-dim domain
#'
#' This function breaks up a domain in 1- or 2- dimensions into evenly
#' spaced samples. It returns a data frame of the position of the samples,
#' each of which can be considered to correspond to a Riemann bin
#' for the purposes of integration.
#'
#' @value By default, a data frame listing the location of the samples,
#' the `.output.` value of the given function at those samples, the spatial
#' extent of the samples (that is, dx for one-dimension or dx and dy for two dimensions. There
#' is also a `dA` giving the dx or dx*dy depending on the dimension). If `sum=TRUE`,
#' then returns the sum of `.output * dA`, that is, the estimate of the integral of
#' the function over the domain.
#'
#' @param tilde A tilde expression specifying the function to be evaluated on the domain.
#' @param domain Either a one- or two-dimensional domain in the same format
#' as for `slice_plot()` or `contour_plot()`, or a data frame with two columns
#' specifying the coordinates of a polygon defining the area.
#' @param n the number of divisions along each of the x- and y-directions. Can
#' be a vector of length 2 giving different numbers for the x and for the y directions.
#' @param sum If `TRUE` carry out the integral and return the numerical result.
#' @examples
#' box_set(x*y ~ x & y, domain(x=0:1, y=0:1), n = 4)
#' # approximation to the variance of a uniform [0,1] distribution
#' box_set((x-.5)^2 ~ x, domain(x=0:1), n=100, sum=TRUE)
#' # a polygon
#' poly <- tibble(x = c(1:9, 8:1), y = c(1, 2*(5:3), 2, -1, 17, 9, 8, 2:9))
#' boxes <- box_set(1 ~ x & y, poly)
#' gf_polygon(y ~ x, data = poly) %>%
#'   gf_rectangle((y - dy/3) + (y + dy/3) ~ (x - dx/3) + (x + dx/3),
#'   data = boxes)
#' # area inside polygon
#' box_set(1 ~ x & y, poly, n=100)
#' @export
box_set <- function(tilde = NULL, domain, n=10, sum=FALSE) {
  # can give either a regular domain or a polygon
  if (is.data.frame(domain)) {
    polygon <- domain
    domain <- list(range(polygon[[1]]), range(polygon[[2]]))
    names(domain) = names(polygon)
  } else {
    polygon <- NULL
    if (!is.list(domain)) stop("domain must be in list form. See, e.g., contour_plot().")
  }

  if (is.null(tilde)) {
    tilde_ex <- paste("1", "~", paste(names(domain), collapse="&"))
    tilde <- as.formula(tilde_ex)
  }
  if (! all(sort(names(tilde[[3]])) == sort(names(domain))))
    stop("Tilde expression must have input names", paste(names(domain), collapse="&"))

  FUN <- makeFun(tilde)

  if (!is.list(domain)) stop("Domain must be a named list, as in Integrate() or contour_plot().")
  nms <- names(domain)
  if (length(domain) == 1) {
    dx <- diff(range(domain[[1]])) / n
    res <- tibble(
      x = seq(min(domain[[1]]) + dx/2, max(domain[[1]]), by = dx),
      dx = dx,
      .output. = FUN(x),
      A = dx
    )

  } else if (length(domain) == 2) {
    if (length(n) == 1) n <- c(n, n)
    dx <- diff(range(domain[[1]])) / n[1]
    dy <- diff(range(domain[[2]])) / n[2]
    x <- seq(min(domain[[1]]) + dx/2, max(domain[[1]]), by = dx)
    y <- seq(min(domain[[2]]) + dy/2, max(domain[[2]]), by = dy)
    res <- expand.grid(list(x=x, y=y)) %>%
      mutate(dx = dx, dy=dy, dA = dx*dy,
             .output. = FUN(x, y))
  } else {
    stop("box_set() works only with domains with one or two variables.")
  }
  names(res)[1:(2*length(nms))] <- c(nms, paste0("d", nms))

  if (!is.null(polygon)) {
    if (!any(nms %in% names(polygon))) stop("Names of polygon data frame must match those of domain")
    inside <- sp::point.in.polygon(res[[1]], res[[2]],
                                   polygon[[nms[1]]],
                                   polygon[[nms[2]]])
    res <- res[inside == 1,]
  }

  if (sum) sum(res$.output. * res$dA)
  else res
}
