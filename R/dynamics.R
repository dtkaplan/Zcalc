#' Dynamical systems calculations and graphics
#'
#' - `streamlines()` draws raindrop-shaped paths at a randomized grid of points that follow trajectories
#' - `flow_field()` draws arrows showing the flow at a grid of points
#'
#' @param ... The first arguments should describe the dynamics. See details.
#' @param npts The number of points on an edge of the grid
#' @param dt Time step for integrating the streamlines.
#' @param nsteps How many steps to take for each streamline. Together with `dt`
#' this determines the length of the streamline.
#' @param color What color to use
#' @param alpha What alpha to use
#'
#' @param first Ignore this. It is a stub to handle piping a graphics
#' layer into the function call.
#'
#' @details The dynamical functions themselves will be formulas like `dx ~ a*x*y` and `dy ~ y/x`.
#' Initial conditions will be arguments of the form `x=3` and `y=4`.
#' If there are parameters in the dynamical functions, you should also
#' add the parameter values, for instance `a=2`.
#'
#' For `flow_field()` and `streamlines()` you do not need to specify
#' initial conditions. The grid will be set by the domain argument.
#'
#' NEED TO ADD THE REPLACEMENT FOR `integrateODE()`.
#'
#'
#' The graphics functions are all arranged to accept,
#' if given, a ggplot object piped in. The new graphics layer will be drawn on
#' top of that. If there is no ggplot object piped in, then the graphics
#' will be made as a first layer, which can optionally piped into other
#' ggformula functions or `+` into ggplot layers.
#'
#' @examples
#' streamlines(dx ~ x+y, dy~ x-y, domain=domain(x=0:6, y=0:3), npts=20, dt=0.01, alpha=.6, color="blue")
#' flow_field(dx ~ x+y, dy~ x-y, domain=domain(x=0:6, y=0:3), npts=20, scale=1)
#' @rdname dynamics
#' @export
streamlines <- function(first, ..., domain=NULL, npts=4, dt=0.01, nsteps=10, color="black", alpha=1) {
  if (is.null(domain)) stop("must specify domain=")
  dyn <- parse_dynamics(first, ..., req_initials=FALSE)
  dyn_fun <- dyn$dyn_fun
  dom <- domain
  if (length(dom) != 2) stop("domain must have two variables")
  xpts <- seq(min(dom[[1]]), max(dom[[1]]), length = npts)
  ypts <- seq(min(dom[[2]]), max(dom[[2]]), length = npts)
  dx <- diff(xpts[1:2])
  dy <- diff(ypts[1:2])
  grid <- as.matrix(expand.grid(list(xpts, ypts)))
  grid[,1] <-grid[,1] + runif(nrow(grid),min=-dx, max=dx)/2
  grid[,2] <-grid[,2] + runif(nrow(grid),min=-dy, max=dy)/2

  Flows <- list()
  for (point in 1:nrow(grid)) {
    x <- y <- numeric(nsteps)
    x[1] <- grid[point,1]
    y[1] <- grid[point,2]

    for (k in 2:length(x)) {
      step = dt*dyn_fun(c(x[k-1], y[k-1]))
      x[k] <- x[k-1] + step[1]
      y[k] <- y[k-1] + step[2]
    }

    Flows[[point]] <- tibble::tibble(x=x, y=y,
                                     group=point,
                                     alpha = 0.2+(1:nsteps)/(1.2*nsteps),
                                     size = alpha + 0.6
    )
  }
  Paths <- dplyr::bind_rows(Flows)
  # get the last point in the path
  Last <- Paths %>% group_by(group) %>%
    filter(row_number() == n())

  P <- dyn$gg
  if (length(P) < 2) P <- NULL # no gg object was piped in
  P %>% ggformula::gf_path(y ~ x, data = Paths,
                     lineend = "round",
                     group = ~ group, color=color, alpha=alpha, size= ~size, inherit=FALSE) %>%
    #ggformula::gf_point(y ~ x, data = Last, shape=23, fill="black", inherit=FALSE) %>%
    ggformula::gf_labs(x = names(domain)[1], y = names(domain)[2]) %>%
    gf_refine(scale_alpha_identity(),
              scale_size_identity())
}
#' @param scale Number indicating how long to draw arrows. By default,
#' the longest arrows take up a full box in the grid
#' @rdname dynamics
#' @export
flow_field <- function(first, ..., domain=NULL, npts=4, scale=0.8, color="black", alpha=1) {
  if (is.null(domain)) stop("must specify domain=")
  dyn <- parse_dynamics(first,..., req_initials=FALSE)
  dom <- domain
  if (length(dom) != 2) stop("domain must have two variables")
  xpts <- seq(min(dom[[1]]), max(dom[[1]]), length = npts)
  ypts <- seq(min(dom[[2]]), max(dom[[2]]), length = npts)
  grid <- as.matrix(expand.grid(list(xpts, ypts)))

  steps <- t(apply(grid, 1, FUN=dyn$dyn_fun))
  step_lengths <- sqrt(steps[,1]^2 + steps[,2]^2)
  dx <- diff(xpts[1:2])
  dy <- diff(ypts[1:2])
  # add two columns to grid
  grid <- cbind(grid, grid)
  scale <- scale / max(step_lengths)
  xstep <- scale*steps[,1]*dx
  ystep <- scale*steps[,2]*dy
  grid[,1] <- grid[,1] - xstep
  grid[,2] <- grid[,2] - ystep
  grid[,3] <- grid[,3] + xstep
  grid[,4] <- grid[,4] + ystep

  res <- as.data.frame(na.omit(grid))
  names(res) <- c("x", "y", "xend", "yend")

  P <- dyn$gg
  if (length(P) < 2) P <- NULL # no gg object was piped in
  P %>%
    gf_segment(y + yend ~ x + xend, data=res,
             color=color, alpha=alpha,
             arrow = arrow(ends="last", type="closed", length=unit(1, "mm")),
             inherit=FALSE)
}
#'
#' @export
parse_dynamics <- function(first, ..., req_initials=TRUE) {
  # Grab all arguments, unevalued
  # <first> is to make sure there is an argument to pipe into
  res <- list()
  res$gg <- if (inherits(first, "gg")) first
  else NA

  if (inherits(first, "formula")) {
    args <- c(first, enquos(...))
  } else {
    args <- enquos(...)
  }
  argnames <- names(args)
  if (length(args) == 0)
    stop("No arguments describing dynamics given.")


  f_names <- c()
  f_vars  <- c()
  dyn_args <- c()
  params <- c()
  others <- list()

  # see if the first one is a ggplot object
  first <- eval(rlang::get_expr(args[[1]]))


  # sort out which ones are dynamics formula style and which are
  # initial conditions
  for (k in 1:length(args)) {
    this <- eval(rlang::get_expr(args[[k]]))
    if (inherits(this, "formula")) {
      f_names <- union(f_names, all.vars(rlang::f_lhs(this)))
      f_vars  <- union(f_vars,  all.vars(rlang::f_rhs(this)))
      dyn_args <- c(dyn_args, k)
    } else if (inherits(this, "numeric")) {
      # it must be a parameter or an initial condition
      if (is.null(argnames[k])) {
        error("Unnamed non-formula argument for dynamics. Parameters and initial conditions must be named.")
      } else {
        params[argnames[k]] <- this
      }
    } else {
      if (is.null(argnames[k]))
        error("Unnamed non-parameter argument.")
      others[[argnames]] <- this
    }
  }

  # check names of formulas to be sure they appear in the list
  # of variables
  # strip out leading "d"
  f_names <- gsub("^d(.{1})", "\\1", f_names )
  matches <- f_names %in% f_vars
  if (!all(matches)) stop("LHS for each formula needs to be one of the variables on the RHS.")
  # any of the parameters that match f_names are really an initial condition
  matches <- f_names %in% names(params)
  if (req_initials && !all(matches)) stop("Initial condition must be specified for each dynamical variable.")
  initials <- params[names(params) %in% f_names]
  params <- params[!names(params) %in% f_names]

  ## Construct the dynamical function with a vector input and t ???
  split_vec <-
    paste0(paste0(f_names,
           "<- vec[",
           1:length(f_names),
           "]", collapse="; "), ";")

  param_assign <- ""
  if (length(params) > 0) {
    param_assign <- paste(names(params), "<-", params, collapse="; ")
    param_assign <- paste0(param_assign, "; ")
  }


  component_expressions <- lapply(args[dyn_args],
                  FUN=function(ex)
                    deparse(rlang::f_rhs(
                      rlang::get_expr(ex)))) %>% unlist()
  res$inside <- paste("c(",
                      paste(component_expressions, collapse=", "),
                      ")")
  res$params <- params
  res$initials <- initials

  res$body <- paste("{", param_assign, split_vec, res$inside, "}")

  res$dyn_fun <- function(vec){}
  body(res$dyn_fun) <- parse(text = res$body)


  res
}
