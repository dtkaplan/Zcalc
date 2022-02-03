#' Functions for the "Driving with integrals" project
#'
#' Generate speed, fuel economy, and heading data
#'
#' @param t Time from 0 to 60 minutes.
#' @rdname driving
#' @export
heading <- function(t) heading_raw(t) + 10*slope(t)*sin(t*slope(t)/20)
#' @rdname driving
#' @export
slope <- function(t) slope_raw(t) - cos(1.5*t)*slope_raw(t)
#' @rdname driving
#' @export
speed <-  function(t) {speedr1(t) + penalty(t) - slope(t) - 7}
#' @rdname driving
#' @export
mpg <- function(t) {
  1/(0.030 - 0.0005  * (50-(speed(t)+ 2*slope(t))))
}



rfun2 <- function(seed, min=0, max=1) {
  f <- mosaic::rfun(~ x, seed)
  r <- range(f(seq(-5,5,by=0.1)))
  function(x) {
    min + (max-min)*(f(((x-30)/7)) - r[1])/diff(r)
  }
}

dom <- mosaicCalc::domain(t=c(0,60))



slope_raw <- rfun2(6528, -2, 4) # degrees


dt_heading <- rfun2(354, min = -6, max=6)
heading_raw <- mosaicCalc::antiD(dt_heading(t) ~ t, lower.bound = 70)


# Another approach to traffic penalty
penalty_raw <- rfun2(94, -70, 30)
penalty <- function(t) {
  raw <- -penalty_raw((t-20)*2)
  ifelse(raw > 0, 0, raw )
}

speedr1 <- rfun2(932, 63, 75)



