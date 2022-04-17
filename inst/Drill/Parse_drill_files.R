library(shiny, quietly=TRUE)
library(dplyr, quietly=TRUE)
library(shinyjs, quietly=TRUE)
library(Zcalc, quietly=TRUE)
library(splines, quietly=TRUE)
library(digest, quietly=TRUE)

# This is one-time code to convert the drill questions into
# askMC() format and write them to a file
# format_all_topics() does the job, creating the file
# ~/Downloads/Drills.Rmd which can then be split out among the book chapters.

kill_empty_lines <- function(str) {
  str[nchar(str) > 0]
}


source_files <-
  readLines("https://raw.githubusercontent.com/dtkaplan/Zdrill/main/Roster.Rmd") %>%
  gsub("#.*$", "", .) %>% # get rid of comments
  kill_empty_lines() %>%
  paste0("https://raw.githubusercontent.com/dtkaplan/Zdrill/main/inst/", .)

# Add in the testing questions straight from the Zdrill sources
draft_questions <- "/Users/kaplan/KaplanFiles/USAFA/Zdrill/testing.Rmd"
dq_size <- file.info(draft_questions)$size
if (is.na(dq_size) || dq_size < 10) draft_questions <- NULL

# test_file <- system.file("Zdrill/www/text.Rmd", package="CalcZapps")
source_files <- c(
  source_files,
  draft_questions
)



Qbank <- try(readDrillfiles(source_files))
if (inherits(Qbank, "try-error")) cat("Problem reading files")
Qbank_topics <- unique(Qbank$Q$topic)

enquote <- function(str) {paste0('r"(', str, ')"')}
format_one_question <- function(index, Q=Qbank$Q, C=Qbank$C) {
  this <- as.list(Q[index,])
  # pull out the corresponding choices
  choices <- C %>% filter(question==this$unique)
  # mark the choice text with "+" if it's correct
  these_choices <- choices %>%
    mutate(choice_text = ifelse(correct,
                                paste0("+",choice_text, "+"),
                                choice_text)) %>%
    mutate(argstring = paste0(enquote(choice_text), " = ", enquote(feedback)))

  choice_text <- paste(these_choices$argstring, collapse=",\n  ")
  randomize <- !any(these_choices$mark == 'a')
  command <- glue::glue("Znotes::askMC(\n  prompt = {enquote(this$prompt)},\n  {choice_text},\n  random_answer_order={randomize}\n)")

  command

}

format_all_questions <- function(this_topic, Q=Qbank$Q, C=Qbank$C) {
  Znotes:::MC_counter$reset()
  curlyleft <- "{"; curlyright <- "}"
  Questions <- Q %>% filter(topic==this_topic)
  Res <- character(nrow(Questions))
  for(k in 1:nrow(Questions)) {
    text <- format_one_question(k, Q=Questions, C)
    ID <- glue::glue("drill-{this_topic}-{k}")
    Res[k] <- glue::glue("\n\n```{curlyleft}r {ID}, echo=FALSE, results='markup'{curlyright}\n{text}\n```\n\n")
  }

  Res
}


format_all_topics <- function(Q=Qbank$Q, C=Qbank$C) {
  topics <- unique(Q$topic)
  topics <- gsub(" ", "-", topics)
  browser()
  Res <- character(length(topics))
  for (k in 1:length(topics)) {
    questions <- format_all_questions(topics[k], Q, C)
    Res[k] <- glue::glue(
      "## Drill {topics[k]}\n\n{paste(questions, collapse='\n\n')}\n\n"
    )
  }

  writeLines(Res, "~/Downloads/Drills.Rmd")
}

# RUN: format_all_topics()
