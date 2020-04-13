#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)


ui <- fluidPage(
  
  # App title 
  titlePanel(title="STA126 Test 4 Data Sets (Spring 2020)"),
  
  # Sidebar layout 
  sidebarLayout(
    
    # Sidebar objects
    sidebarPanel(
      
      numericInput(inputId = "id",
                   "Enter the last four of your MU ID", 
                   value=NULL),  
      # Object
      #actionButton(inputId = "newdata", "Get a Data Set"),
      
      # Sidebar width can not exceed 12, default 4.
      width = 4
    ), # end of sidebar panel
    
    # Main panel----
    mainPanel(
      
      tabsetPanel(
        tabPanel("Question 1", 
                 fluidRow(column(12, htmlOutput("textblocknum"))),
                 fluidRow(column(12, DT::dataTableOutput("numTable"))
                 )),
        tabPanel("Question 2",
                 fluidRow(column(12, htmlOutput("textblockcat"))),
                 fluidRow(column(12, DT::dataTableOutput("catTable"))
                 )
        ))
      # Display textblock 1 ----
      #verbatimTextOutput("textblock1"),
      
      
      # Display sample data table ----
      # tableOutput("table1")
      
      
      
    ) #end of mainPanel
    
  ) #end of sidebarlayout
  
) #end of fluidpage
