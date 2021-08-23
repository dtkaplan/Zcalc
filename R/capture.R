#' Displays the result of a command with the command itself
#'
#' In `{learnr}`, the results of running an exercise are posted below the code-entry
#' box. When there are multiple commands, this breaks the direct link between the command
#' itself and the printed results. `capture()` prints out both the command and the result
#' along with an optional message. `annot()` turns a character string into a line
#' that looks like a comment.
#'
#' @details You **must** use the built-in pipe (`|>`) into `capture()`. Use whatever
#' you like to the left of `|>`.
#' Meant for results that are very short vectors, either numeric or character string.
#'
#' @param cmd An R expression, typically piped into `capture()`
#' @param message a comment to print after the result of cmd.
#'
#'
#' @examples
#' cos(7) |> capture("Example 3")
#' annot("Show the plot")
#' @export
capture <- function(cmd, message = "", digits=4) {
  cmd2 <- substitute(cmd)
  if (is.call(cmd2) || is.name(cmd2)) {
    res <- force(cmd)
  } else {
    res <- cmd
  }
  if (is.numeric(res) || is.character(res)) {
    if (base::interactive()) {
      cat(crayon::inverse(paste(deparse(cmd2), crayon::red(" --> "))),
          one_liner(res), crayon::blue(message))
      invisible(res)
    } else {
      show <- glue::glue("<code>{deparse(cmd2)}</code>  -->  <strong>{paste(format(res, digits=digits), collapse='  ')}</strong>  <em># {message}</em><br><br>", )
      return(HTML(show))
    }
  } else {
    res
  }
}

#' @export
annot <- function(message) {
  cat(crayon::inverse(paste("#", message)), "\n")
  invisible(NULL)
}

one_liner <- function(x) {
  if (length(x) == 0) return("NULL result")
  if (length(x) < 5) {
    res <- capture.output(x)
    res <- gsub("\\[1\\] ", "", res)
    return(res)
  }
  if (is.numeric(x)) {

  }
}
