library(shiny)
library(shinythemes)
library(lubridate)
library(shinyjs)
library(shinyWidgets)

ui <- fluidPage(theme = shinytheme("sandstone"),
                useShinyjs(),
                tags$head(
                  tags$link(rel = "shortcut icon", type = "image/x-icon", href = "https://raw.githubusercontent.com/tsu2000/math_quiz/main/calculate.png"),
                  tags$title("R Math Quiz")
                ),
                titlePanel(
                  HTML("<span style='font-size: 36px; font-weight: bold; color: #042C8F;'>custom aRithmetic quiz</span>")
                ),
                sidebarLayout(
                  sidebarPanel(
                    numericInput("num_questions", "Choose number of questions to appear in the quiz:", value = 5, min = 1, max = 100000, step = 1),
                    sliderTextInput(
                      inputId = "value",
                      label = "Select how long the quiz should last:",
                      choices = c("30 sec", "1 min", "2 min", "5 min", "10 min"),
                      selected = "1 min",
                      grid = TRUE,
                      force_edges = TRUE,
                      width = "90%"
                    ),
                    markdown(tags$span("Select operations to appear in the quiz:", style = "font-size: 12px;")),
                    div(style = "padding: 0px 0px;
                                 margin-top: -15px",
                        fluidRow(
                          column(width = 2, checkboxInput(inputId = "add", label = HTML("<b>+</b>"), value = TRUE)),
                          column(width = 2, checkboxInput(inputId = "subtract", label = HTML("<b>–</b>"), value = TRUE)),
                          column(width = 2, checkboxInput(inputId = "multiply", label = HTML("<b>*</b>"))),
                          column(width = 6, checkboxInput(inputId = "divide", label = HTML("<b>%/%</b> &nbsp; (integer division)")))
                        )
                    ),
                    radioButtons("difficulty", "Select difficulty of the quiz:",
                                 c("Easy (1 - 2 digits by 1 digit)" = "easy",
                                   "Medium (3 digits by 1 - 2 digits) " = "medium",
                                   "Hard (4 digits by 1 - 2 digits)" = "hard",
                                   "Expert (4 digits by 2 - 3 digits)" = "expert")
                    ),
                    actionButton("start_button", "Start Quiz"),
                    actionButton("end_button", "End Quiz", disabled = TRUE)
                  ),
                  mainPanel(
                    div(id = "quiz_status",
                        uiOutput('timeleft'),
                        uiOutput("quiz_panel")
                    )
                  )
                ),
                tags$head(
                  tags$style(HTML('
                    #quiz_status.quiz-not-started #timeleft {
                      display: none;
                    }
                    
                    #quiz_status.quiz-started #timeleft {
                      display: inline-block;
                      background-color: lightblue;
                      border: 2px solid red;
                      border-radius: 5px;
                      padding: 5px;
                      margin-bottom: 10px;
                      padding-right: 10px;
                      font-size: 20px;
                    }
                  '))
                )
)

server <- function(input, output, session) {
  questions <- reactiveVal()
  current_question <- reactiveVal(0)
  correct_answers <- reactiveVal(0)
  quiz_started <- reactiveVal(FALSE)
  quiz_completed <- reactiveVal(FALSE)
  
  timer <- reactiveVal(0)
  active <- reactiveVal(FALSE)
  
  observeEvent(input$num_questions, {
    # Get the entered number of questions
    num_questions <- input$num_questions
    
    # Check if the input is a positive integer
    if (!is.numeric(num_questions) || num_questions %% 1 != 0 || num_questions <= 0) {
      shinyjs::disable("start_button")
    } else {
      shinyjs::enable("start_button")
    }
  })
  
  observeEvent(input$start_button, {
    
    # Generate questions
    questions(generateQuestions(input$num_questions, input$difficulty, list(
      add = input$add,
      subtract = input$subtract,
      multiply = input$multiply,
      divide = input$divide
    )))
    
    correct_answers(0)
    current_question(1)
    
    value <- input$value
    numeric_value <- switch(value, "30 sec" = 30, "1 min" = 60, "2 min" = 120, "5 min" = 300, "10 min" = 600)
    
    quiz_started(TRUE)
    active(TRUE)
    timer(numeric_value)
    quiz_completed(FALSE)
    
    shinyjs::enable("end_button")
    shinyjs::removeClass("quiz_status", "quiz-not-started")
    shinyjs::addClass("quiz_status", "quiz-started")
  })
  
  observeEvent(c(input$add, input$subtract, input$multiply, input$divide), {
    if (!input$add && !input$subtract && !input$multiply && !input$divide) {
      shinyjs::disable("start_button")
    } else {
      shinyjs::enable("start_button")
    }
  })
  
  observe({
    if (quiz_started() && !quiz_completed()) {
      shinyjs::enable("end_button")
    } else {
      shinyjs::disable("end_button")
    }
  })
  
  output$timeleft <- renderUI({
    if (!quiz_started() || current_question() == 0 || is.null(questions()) || quiz_completed())
      return(NULL)
    
    minutes <- floor(timer() / 60)
    seconds <- timer() %% 60
    
    HTML(paste("Time left: ", HTML("&emsp;"),"<b>", sprintf(" %02d:%02d", minutes, seconds), "</b>", sep = ""))
  })
  
  observe({
    invalidateLater(1000, session)
    isolate({
      if (active()) {
        timer(timer() - 1)
        if (timer() < 1) {
          active(FALSE)
          if (!quiz_completed()) {
            showModal(modalDialog(
              title = "Quiz Completed",
              paste("You answered", correct_answers(), "out of", input$num_questions, "questions correctly."),
              footer = modalButton("Close")
            ))
            quiz_completed(TRUE)
            current_question(0)
            quiz_started(FALSE)
            shinyjs::addClass("quiz_status", "quiz-not-started")
            shinyjs::removeClass("quiz_status", "quiz-started")
          }
        }
      }
    })
  })
  
  output$quiz_panel <- renderUI({
    if (!quiz_started() || current_question() == 0 || is.null(questions()))
      return(NULL)
    
    question <- questions()[[current_question()]]
    
    tagList(
      h3(paste("Question", current_question(), "of", input$num_questions)),
      br(),
      h4(question$expression),
      br(),
      numericInput("answer_input", "Your Answer:", value = NULL),
      actionButton("next_button", "Next"),
      tags$script(HTML('
        setTimeout(function() {
          document.getElementById("answer_input").focus();
        }, 0);

        $("#answer_input").on("keydown", function(event) {
          if (event.which == 13) {
            event.preventDefault();
            var answerValue = parseInt($("#answer_input").val(), 10);
            Shiny.setInputValue("next_button", answerValue, {priority: "event"});
          }
        });
      '))
    )
  })
  
  observeEvent(input$next_button, {
    question <- questions()[[current_question()]]
    answer <- input$answer_input
    
    if (!is.na(answer)) {
      if (as.integer(answer) == question$answer)
        correct_answers(correct_answers() + 1)
    }
    
    if (current_question() < input$num_questions) {
      current_question(current_question() + 1)
    } else {
      if (!quiz_completed()) {
        showModal(modalDialog(
          title = "Quiz Completed",
          paste("You answered", correct_answers(), "out of", input$num_questions, "questions correctly."),
          footer = modalButton("Close")
        ))
        quiz_completed(TRUE)
      }
      
      current_question(0)
      quiz_started(FALSE)
      active(FALSE)
      
      shinyjs::addClass("quiz_status", "quiz-not-started")
      shinyjs::removeClass("quiz_status", "quiz-started")
    }
  })
  
  observeEvent(input$end_button, {
    if (quiz_started()) {
      showModal(modalDialog(
        title = "Quiz Ended",
        paste("You ended the quiz prematurely."),
        footer = modalButton("Close")
      ))
      
      current_question(0)
      quiz_started(FALSE)
      active(FALSE)
      
      shinyjs::disable("end_button")
      shinyjs::addClass("quiz_status", "quiz-not-started")
      shinyjs::removeClass("quiz_status", "quiz-started")
    }
  })
}

# Function to generate questions based on the user's inputs
generateQuestions <- function(num_questions, difficulty, operations) {
  
  questions <- vector("list", num_questions)
  sample_values <- vector("list", 2)
  
  if (difficulty == "easy") {
    sample_values[[1]] <- 1:99
    sample_values[[2]] <- 1:9
  } else if (difficulty == "medium") {
    sample_values[[1]] <- 100:999
    sample_values[[2]] <- 1:99
  } else if (difficulty == "hard") {
    sample_values[[1]] <- 1000:9999
    sample_values[[2]] <- 1:99
  } else {
    sample_values[[1]] <- 1000:9999
    sample_values[[2]] <- 10:999
  }
  
  operators <- c()
  
  if (operations$add) {
    operators <- c(operators, "+")
  }
  if (operations$subtract) {
    operators <- c(operators, "-")
  }
  if (operations$multiply) {
    operators <- c(operators, "*")
  }
  if (operations$divide) {
    operators <- c(operators, "%/%")
  }
  
  # print(operators)
  
  for (i in 1:num_questions) {
    
    x <- sample(sample_values[[1]], 1)
    y <- sample(sample_values[[2]], 1)
    
    operator <- sample(operators, 1)
    
    if (operator == "+") {
      answer <- x + y
    } else if (operator == "-") {
      answer <- x - y
    } else if (operator == "*") {
      answer <- x * y
    } else {
      answer <- x %/% y
    }
    
    # print(answer)
    expression <- paste(x, operator, y, "=", sep = " ")
    
    questions[[i]] <- list(expression = expression, answer = answer)
  }
  
  questions
}

# Run Shiny App
shinyApp(ui, server)
