#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)


server <- function(input, output){
  
  # Get new data set upon clicking on the activeButton
  # by getting a new random seed
  # id <- eventReactive(input$newdata, {
  #   sample(1:10^6,1)
  # })
  
  cl <- reactive({
    if (is.na(input$id)){
      return(NA)
    }else{
      # Plant the random number seed
      set.seed(input$id)
      cl <- sample(88:96, 1)
      return(cl)
    }
  })
  
  catData <- reactive({
    
    if (is.na(input$id)){
      userdata <- data.frame(Individual=NA, Outcome=NA)
      return(userdata)
    }else{
      # Plant the random number seed
      set.seed(input$id) 
      cl <- sample(88:96, 1)
      n <- sample(30:40, 1)
      
      A_one <- 1:n
      group_one <- rep(c('Red'), times = 47)
      group_two <- rep(c('Blue'), times = 53)
      population <- c(group_one, group_two)
      A_two <- sample(population, n)
      
      
      
      userdata <- data.frame(A_one, A_two)
      col_headings <- c('Individual', 'Outcome')
      colnames(userdata) <- col_headings
      return(userdata)
    }
  }) # end reactive
  
  mu <- reactive({
    
    # Plant the random number seed
    if (is.na(input$id)){
      return(NULL)
    }else{
      set.seed(input$id) 
      mu <- round(runif(1, 70, 80),2)
    }})
  
  numData <- reactive({
    
    # Plant the random number seed
    if (is.na(input$id)){
      userdata <- data.frame(Individual=NA, Scores=NA, Notes=NA)
      return(userdata)
    }else{
      set.seed(input$id) 
      mu <- round(runif(1, 70, 80),2)
      n <- sample(25:30, 1)
      
      A_one <- 1:n
      A_two <- sample(rnorm(1000, 75, 5), n)
      A_three <- A_one %% 2
      
      userdata <- data.frame(A_one, A_two, A_three)
      col_headings <- c('Individual', 'Scores', 'Notes')
      colnames(userdata) <- col_headings
      return(userdata)
    }
    
  }) # end reactive
  
  
  # Output: Textblock 1 ----
  output$textblocknum <- renderText({
    paste("Data set id:",
          "<font color=\"#FF0000\"><b>", 
          input$id,
          "</b></font>",
          "<ul>",
          "<li>Each row represents the score of one student.</li>",
          "<li>Test if the true average score is ", mu(), "</li>",
          "</ul>")
  })
  
  output$textblockcat <- renderText({
    paste("Data set id:",
          "<font color=\"#FF0000\"><b>", 
          input$id,
          "</b></font>",
          "<ul>",
          "<li>Each row represents one ball drawn from a jar.</li>",
          "<li>Find the ", cl(), "% confidence interval.</li>",
          "</ul>")
  })
  
  
  # Output: Sample data table ----
  output$catTable <- DT::renderDataTable({
    datatable(catData(), rownames = FALSE, options = list(
      pageLength = 50,
      dom = "t",
      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = 0:1))))%>%
      formatStyle("Outcome", backgroundColor=styleEqual(c("Red","Blue"),c('orange','gray')))
  },striped = TRUE,colnames = TRUE)
  
  # Output: Sample data table ----
  output$numTable <- DT::renderDataTable({
    datatable(numData(), rownames = FALSE, options = list(
      pageLength = 50,
      dom = "t",
      columnDefs = list(list(className ='dt-center', targets = 0:1), 
                        list(visible=FALSE, targets=2))))%>%
      formatStyle("Notes", backgroundColor=styleEqual(c(1,0),c('orange','gray')), target = "row")%>%
      formatRound(columns=c('Scores'), digits=2)
  },striped = TRUE,colnames = TRUE)
  
} #end server
