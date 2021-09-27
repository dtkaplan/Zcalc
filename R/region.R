#' Flexibly specify a domain for plotting
#'
#' Domains are used so often for plotting that it's nice
#' to have a more compact syntax. Within `region()` you
#' can replace c(left, right) with `left | right`, `left ^ right`,
#' and so on. (Typically, left and right are numerals, as in `0 | 10`.)
#' @export
region <- function(...) {
  args <- enexprs(...)
  res_names <- names(args)
  res <- as.list(rep("",length(args)))
  for (k in 1:length(args)) {
    ex <- args[[k]]
    command <- as.character(ex[[1]])
    if (nchar(names(args)[k]) > 0) {
      res_names[k] <- names(args)[k]
    } else {
      # It's unnamed, leave it for later
      res_names[k] <- paste0(".unknown_", k)
    }
    if (command == "[") {
      res_names[k] <- as.character(ex[[2]])
      res[[k]] <- c(ex[[3]][[2]], ex[[3]][[3]])
    } else if (command %in% c("%%","|", "||", "&" ,"<", "<=", ">", ">=", "-", "+", "*", "/","^", ":")) {
      res[[k]] <- c(eval(ex[[2]]), eval(ex[[3]]))
    } else {
      res[[k]] <- eval(ex)
    }

  }
  names(res) <- res_names
  res
}
