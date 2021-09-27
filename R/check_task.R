#' Check a mosaic programming task
#'
#' Utilities for the command trainer
#' @param target character string containing the CORRECT expression
#' @param new character string containing candidate for checking against target
#' #export
match_assignment <- function(target, new) {
  if (is.na(target)) return(was_correct("No assignment called for."))
  new <- gsub("^ *| *$", "", new)
  if (nchar(new) == 0) return(was_wrong("Not yet started."))
  if (grepl("=|<-[^ ]+", new)) return(was_wrong("Malformed assignment. Use <-"))
  if (!grepl("<- *$", new)) {
    return(was_wrong("Not yet a complete assignment phrase"))
  } else {
    name_part <- gsub(" *<- *", "", new)
    # Check the name
    name_pattern <- " *([a-zA-Z\\.][a-zA-Z0-9\\._]*) *<- *"
    if (!grepl(name_pattern, new)) {
      return(was_wrong(paste0("<", name_part, "> is an invalid name.")))
    } else {
      # it's a valid name!
      if (name_part == target) return(was_correct("Assignment phrase complete"))
      else return(was_wrong("Wrong name as assignment target."))
    }
  }
  stop("Shouldn't get here in checking assignment.")
}

#' #export
match_op<- function(target, new) {
  new <- gsub("^ *| *$", "", new) #kill blanks
  if (nchar(new) == 0) return(was_wrong("Not yet started"))
  # extract name
  found_name <- gsub("^\ *([a-zA-Z_\\.0-9]*)\ *", "\\1", new )
  if (found_name == target) {
    return(was_correct("Correct!"))
  } else {
    if (grepl(found_name, target, fixed = TRUE)) return(was_wrong("You're getting there."))
    else return(was_wrong("Looking bad."))
  }

  stop("Shouldn't get here in checking operator.")
}

text_entry <- function(id, width="90%") {
  glue::glue('<input id="{id}" type="text" class="form-control" style="display: inline; width:{width}" value=""/>')
}

label_row <- function(task) {
  res <- "<tr>"
  if (!is.na(task$assign)) res <- paste(res, "<td style='font-size: 15px;'>name assign</td> <td></td>")
  res <- paste(res, "<td style='font-size: 15px;'>operation</td><td></td>")
  res <- paste(res, "<td style='font-size: 15px;'>tilde</td><td></td>")
  paste(res, "</tr>")
}

feedback_row <- function(task) {
  res <- "<tr>"
  if (!is.na(task$assign))
    res <- paste(res, '<td><span id="fb_assign" class="shiny-text-output">pending</span></td><td></td>')
  res <- paste(res, '<td><span id="fb_op" class="shiny-text-output">pending</span></td><td></td>')
  res <- paste(res,'<td><span id="fb_tilde" class="shiny-text-output">pending</span></td><td></td>')
  if (!is.na(task$second))
    res <- paste(res,'<td><span id="fb_second" class="shiny-text-output">pending</span></td><td></td>')
  if (!is.na(task$third))
    res <- paste(res,'<td><span id="fb_third" class="shiny-text-output">pending</span></td><td></td>')
  if (!is.na(task$fourth))
    res <- paste(res,'<td><span id="fb_fourth" class="shiny-text-output">pending</span></td><td></td>')

  res
}



#' #export
match_tilde <- function(target, new) {
  if (grepl("[^~]*~ *$", new)) return(was_wrong("Add RHS to formula."))
  full <- try(str2lang(new), silent=TRUE)
  if (inherits(full, "try-error")) {
    leftnew <- gsub("~.*$", "", new)
    rightnew <- gsub("^.*~", "", new)
    if (leftnew == new && rightnew == new) {
      # treat new as a prospective LHS
      left <- try(str2lang(new), silent=TRUE)
      if (inherits(left, "try-error")) return(was_wrong("Not yet a complete LHS for tilde expression."))
      else return(was_wrong("Add the tilde (~) and the RHS of expression"))
    }
    left <- try(str2lang(leftnew), silent=TRUE)
    right <- try(str2lang(rightnew), silent=TRUE)
    if (inherits(left, "try-error")) return(was_wrong("Not yet a valid LHS for tilde expression."))
    if (inherits(right, "try-error")) return(was_wrong("Not yet a valid RHS for tilde expression."))
  }

  target <- as.formula(target)
  new <- try(as.formula(new), silent=TRUE)

  if (inherits(new, "try-error")) {
    return(was_wrong("Keep going ..."))
  } else {
    # At this point, it's a valid tilde expression, but is it the right one?
    tvars <- all.vars(target[[3]])
    nvars <- all.vars(new[[3]])

    if (length(tvars) != length(nvars)) return(was_wrong("Wrong number of names on RHS of tilde."))
    if (!all(tvars %in% nvars)) return(was_wrong("Wrong name on RHS of tilde."))
    if (!all(tvars == nvars)) return(was_wrong("Names in RHS in wrong order."))
    # At this point, the RHS is correct and the LHS is a valid expression
    tleft <- target[[2]]
    nleft <- new[[2]]
    msg <- compare_parse_tree(tleft, nleft)
    if (nchar(msg) == 0) return(was_correct("tilde expression is right!"))
    else return(was_wrong(paste("Error in tilde expression:", msg)))
  }
}

#' #export
match_arg <- function(target, new) {
  new <- gsub("^ *| *$", "", new)
  tlang <- try(str2lang(target), silent=TRUE)
  if (inherits(tlang, "try-error")) return(was_wrong("Mistake in task specification."))
  nlang <- try(str2lang(new), silent=TRUE)
  if (inherits(nlang, "try-error")) return(was_wrong("Not a valid/complete expression."))
  if (grepl("^data *=", target) && !grepl("^data *=", new))
    return(was_wrong("You should be specifying the data frame."))
  if (grepl("^domain\\(", target) && !grepl("^domain", new))
    return(was_wrong("You should be specifying the domain of the plot."))
  msg <- compare_parse_tree(tlang, nlang)
  if (nchar(msg) > 0) return(was_wrong(msg))

  was_correct("The argument is right.")
}

#' #export
compare_parse_tree <- function(target, new) { #these should be parsed expressions
  error_msg <- paste(deparse(target), "vs", deparse(new), "in yours")
  if (length(target) == 1 && length(new) == 1)
    return(ifelse(target==new, "", error_msg))


  if (length(target) != length(new)) return(error_msg)

  if (target[[1]] != new[[1]]) return(paste(deparse(target[[1]]), "vs", deparse(new[[1]]), "in yours"))

  left <- compare_parse_tree(target[[2]], new[[2]])
  if (nchar(left) != 0) return(error_msg) # return(left)

  if (length(target) == 3) {
    right <- compare_parse_tree(target[[3]], new[[3]])
    if (nchar(right) != 0) return(error_msg)
  }

  return("") # no problems
}

# no real need to export after debugging phase
#' #export
was_correct <- function(msg) make_check_result(TRUE, msg)
#' #export
was_wrong   <-function(msg) make_check_result(FALSE,msg)
#' #export
make_check_result <- function(valid, msg) {
  list(valid=valid, msg = msg)
}

#' Create frame for command entry
#'
#' #export
command_frame <- function(task) {
  assign <- if (!is.na(task$assign)) glue::glue("<td>{text_entry('assign', width='100px')}</td><td style='font-size: 44px;'></td>") else ""
  op     <- glue::glue("<td>{text_entry('op', width='100px')}</td><td style='font-size: 44px;'>(</td>" )
  comma  <- ifelse(!is.na(task$second), ",", "")
  tilde  <- glue::glue("<td>{text_entry('tilde', width='200px')} </td><td style='font-size: 44px;'>{comma} </td>" )
  comma  <- ifelse(!is.na(task$third), ",", "")
  second <- if (!is.na(task$second)) glue::glue("<td>{text_entry('second', width='200px')}</td><td>{comma} </td>") else ""
  comma  <- ifelse(!is.na(task$fourth), ",", "")
  third  <- if (!is.na(task$third)) glue::glue("<td>{text_entry('third', width='100px')}</td><td style='font-size: 44px;'>{comma} </td>") else ""
  fourth <- if (!is.na(task$fourth)) glue::glue("<td>{text_entry('fourth', width='200px')}</td>") else ""
  final  <- "<td style='font-size: 44px;'> )</td>"

  frame <-
    paste("<table><tr>\n",
       label_row(task),
       assign, "\n", op, "\n", tilde, "\n", second, "\n",
       third, "\n", fourth,"\n",
       final,
       feedback_row(task),
      "</tr></table>",
      collapse = "")

  frame
}
