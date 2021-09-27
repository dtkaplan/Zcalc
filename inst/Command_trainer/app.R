library(shiny)
library(Znotes)
library(Zcalc)

TASKS <- Zcalc::TASKS




# Define UI for application that draws a histogram
ui <- fluidPage(
  # changing padding on text input widgets
  tags$head(
    tags$style(HTML(
      ".form-control { height:auto; padding:3px 1px;}"
    ))
  ),
  titlePanel("MOSAIC Command Trainer"),

  verticalLayout(
    selectInput("task_id", "Choose task:", sort(TASKS$ID)),
    hr(),
    div(uiOutput("show_task")),
    hr(),
    uiOutput("user_input"),
    hr(),
    htmlOutput("command_text"),
    hr(),
    htmlOutput("hash"),
    hr(),
    htmlOutput("text_results"),
    plotOutput("plot_results"),
    textInput("grader", "Instructor option:")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  plot_result_holder <- reactiveVal(NULL)
  text_result_holder <- reactiveVal(NULL)

  current_task <- reactive({
    ind <- which(TASKS$ID == input$task_id)
    TASKS[ind, ]
  })

  output$user_input <- renderUI({
    HTML(
        Zcalc:::command_frame(current_task())
        )
  })



  output$show_task <- renderUI({
     withMathJax(HTML(glue::glue("<div style=\"color: green; font-size: 1.3em;\">{md2html(current_task()$description)}</div>")))
  })

  observe({
    if(answer_complete()) {
      cmd <- gsub("^ *[a-zA-Z0-9_\\.]* *<- *", "", current_command())
      result <- try(eval(parse(text=cmd)), silent=TRUE)
      if (inherits(result, "try-error")) {
        plot_result_holder(NULL)
        text_result_holder("")
        return()
      }
      if (inherits(result, "ggplot")) {
        plot_result_holder(result)
        text_result_holder(" ")
      } else {
        plot_result_holder(NULL)
        text_result_holder(paste(capture.output(result), collapse="\n"))
      }
    } else {
      plot_result_holder(NULL)
      text_result_holder("")
    }
  })

  # State of correctness of current entries
  # If the entry isn't listed, regard it as TRUE
  assign_state <- reactive({
    if (is.na(current_task()$assign)) return(list(valid=TRUE))
    if (is.null(input$assign)) return(list(valid=FALSE))
    Zcalc:::match_assignment(current_task()$assign, input$assign)
  })
  op_state <- reactive({
    if (is.na(current_task()$op)) return(list(valid=TRUE))
    if (is.null(input$op)) return(list(valid=FALSE))
    Zcalc:::match_op(current_task()$op, input$op)
  })
  tilde_state <- reactive({
    if (is.na(current_task()$tilde)) return(list(valid=TRUE))
    if (is.null(input$tilde)) return(list(valid=FALSE))
    Zcalc:::match_tilde(current_task()$tilde, input$tilde)
  })
  second_state <- reactive({
    if (is.na(current_task()$second)) return(list(valid=TRUE))
    if (is.null(input$second)) return(list(valid=FALSE))
    Zcalc:::match_arg(current_task()$second, input$second)
  })
  third_state <- reactive({
    if (is.na(current_task()$third)) return(list(valid=TRUE))
    if (is.null(input$third)) return(list(valid=FALSE))
    Zcalc:::match_arg(current_task()$third, input$third)
  })
  fourth_state <- reactive({
    if (is.na(current_task()$fourth)) return(list(valid=TRUE))
    if (is.null(input$fourth)) return(list(valid=FALSE))
    Zcalc:::match_arg(current_task()$fourth, input$fourth)
  })

  answer_complete <- reactive({
    assign_state()$valid & op_state()$valid & tilde_state()$valid &
      second_state()$valid & third_state()$valid & fourth_state()$valid
  })

  output$fb_assign <- renderText({
    if (nchar(gsub("^ *| *$", "", input$assign)) == 0) "Enter name"
    else {
      feedback <- assign_state()
      if (feedback$valid) random_success()
      else feedback$msg
    }
  })


  output$fb_op <- renderText({
    if (nchar(gsub("^ *| *$", "", input$op)) == 0) "pending"
    else {
      feedback <- op_state()
      if (feedback$valid) random_success()
      else feedback$msg
    }
  })

  output$fb_tilde <- renderText({
    if (nchar(gsub("^ *| *$", "", input$tilde)) == 0) "waiting ..."
    else {
      feedback <- tilde_state()
      if (feedback$valid) random_success()
      else feedback$msg
    }
  })

  output$fb_second <- renderText({
    if (nchar(gsub("^ *| *$", "", input$second)) == 0) "waiting ..."
    else {
      feedback <- second_state()
      if (feedback$valid) random_success()
      else feedback$msg
    }
  })

  output$fb_third <- renderText({
    if (nchar(gsub("^ *| *$", "", input$third)) == 0) "waiting ..."
    else {
      feedback <- third_state()
      if (feedback$valid) random_success()
      else feedback$msg
    }
  })

  output$fb_fourth <- renderText({
    if (nchar(gsub("^ *| *$", "", input$fourth)) == 0) "waiting ..."
    else {
      feedback <- fourth_state()
      if (feedback$valid) random_success()
      else feedback$msg
    }
  })

  current_command <- reactive({
    res <- ""
    if (!is.na(current_task()$assign)) res <- paste0(res, input$assign)
    if (!is.na(current_task()$op)) res <- paste0(res, input$op)
    res <- paste0(res, "(")
    if (!is.na(current_task()$tilde)) res <- paste0(res, input$tilde)
    if (!is.na(current_task()$second)) res <- paste0(res, ", ", input$second)
    if (!is.na(current_task()$third)) res <- paste0(res, ", ", input$third)
    if (!is.na(current_task()$fourth)) res <- paste0(res, ", ", input$fourth)
    res <- paste0(res, ")")
  })

  output$command_text <- renderText({
    res <- ifelse(answer_complete(), "", "Constructed so far: ")
    res <- paste0(res, "<code>", current_command(), "</code>")

    HTML(res)
  })

  output$hash <- renderText({
    if (answer_complete()) {
      return(HTML(paste(
        "<strong>Success!</strong> Submit this hash code: <span style='color: magenta;'>",
        current_task()$hash, "</span>")))
    } else {
      ""
    }
  })

  output$plot_results <- renderPlot({
    res <- plot_result_holder()
    if (inherits(res, "try-error")) NULL
    else res
  })

  output$text_results <- renderText({
    HTML(paste0("<pre>", text_result_holder(), "</pre>"))
  })

  observe({
    if (input$grader == "update") {
      google_name <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSDNmAILXPEijK2T6c_fgWqcG7Q4P0dl9aPqbrRWqxei2afej1ioW8dapII19A-Q-0WkuVLqchXXi-z/pub?output=csv"
      TASKS <<- readr::read_csv(google_name)
      cat("Updating tasks.\n")
      updateSelectInput(session, "task_id", choices = TASKS$ID)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
