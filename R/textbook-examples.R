#' Example functions used in MOSAIC Calculus
#'
#' @export
SM_2_potential <- function(x, y) {
    abs(x)^2.7 + abs(y)^2.7 - 1 + # spring 1
    abs(x - 2)^1.5 + abs(y)^1.5 - 1 + #spring 2
    9.8 *y +  # gravity
    abs(x-3)^3 + abs(y)^3 - 1 # spring 3
}
#' @export
SM_3_potential <- function(x1,y1,x2,y2,x3,y3) {
  L01sq <- x1^2 + y1^2 - 0.5
  L12sq <- (x2-x1)^2 + (y2-y1)^2 - 0.5
  L23sq <- (x3-x2)^2 + (y3-y2)^2 - 0.5
  L34sq <- (x3 - 3.2)^2 + (y3 + 1.1)^2 - 0.5
  gravity <- (y1 + y2 + y3)

  0.8*(L01sq + L12sq + L23sq + L34sq)  + 2*gravity
}
