# Generate speed, fuel economy, and heading data for the Rover project
library(mosaic)

rfun2 <- function(seed, min=0, max=1) {
  f <- rfun(~ x, seed)
  r <- range(f(seq(-5,5,by=0.1)))
  function(x) {
    min + (max-min)*(f(((x-30)/7)) - r[1])/diff(r)
  }
}

dom <- list(t=c(0,60))



slope_raw <- rfun2(6528, -2, 4) # degrees

#' @export
rover.incline <- function(t) slope_raw(t) - cos(1.5*t)*slope_raw(t)


dt_heading <- rfun2(354, min = -6, max=6)
heading_raw <- mosaicCalc::antiD(dt_heading(t) ~ t, lower.bound = 70)
#' @export
rover.heading <- function(t) heading_raw(t) + 10*rover.grade(t)*sin(t*rover.grade(t)/20)

# Another approach to traffic penalty
penalty_raw <- rfun2(94, -70, 30)
penalty <- function(t) {
  raw <- -penalty_raw((t-20)*2)
  ifelse(raw > 0, 0, raw )
}

speedr1 <- rfun2(932, 63, 75)

#' @export
rover.speed <-  function(t) {(pnorm(58-t))*pnorm(t-2) * (speedr1(t) + penalty(t) - rover.grade(t) - 7)/12}

#' @export
rover.current <- function(t) {
  1/(0.030 - 0.0005  * (50-(rover.speed(t)+ 2*rover.grade(t))))
}

