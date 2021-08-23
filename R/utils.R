msg <- function(..., startup = FALSE) {
  if (startup) {
    #if (!isTRUE(getOption("tidyverse.quiet"))) {
      packageStartupMessage(...)
    # }
  } else {
    message(...)
  }
}
