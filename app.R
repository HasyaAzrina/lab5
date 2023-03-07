library(shiny)
library(datasets)
insurance <- read.csv("insurance.csv", stringsAsFactors = FALSE)
dat <- read.csv('iris.csv', stringsAsFactors = FALSE)
groceries <- read.csv("groceries.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("Tab Panel Example"),
  tabsetPanel(
    tabPanel("Summary", h3("Data Summary"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "dataset",
                             label = "Choose a dataset:",
                             choices = c("insurance", "iris", "groceries")),
                 numericInput(inputId = "obs",
                              label = "Number of observations to view:",
                              value = 10)
               ),
               mainPanel(
                 verbatimTextOutput("summary"),
                 tableOutput("view")
               ))),
      
    tabPanel("Import", h3("Uploading Data"),
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1", "Choose CSV File",
                           multiple = TRUE,
                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                 tags$hr(),
                 checkboxInput("header", "Header", TRUE),
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                              selected = ","),
                 radioButtons("quote", "Quote",
                              choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                              selected = '"'),
                 tags$hr(),
                 radioButtons("disp", "Display",
                              choices = c(Head = "head", All = "all"),
                              selected = "head")
               ),
               mainPanel(
                 tableOutput("contents")
               )
             )),
    tabPanel("Download File", h3("Downloading Data"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset", "Choose a dataset:",
                             choices = c("rock", "pressure", "cars")),
                 downloadButton("downloadData", "Download")
               ),
               mainPanel(
                 tableOutput("table")
               )
             )
    ),
    tabPanel("Timer", h3(textOutput("currentTime")))
  )
)

server <- function(input, output,session) {
  # Server code goes here
  datasetInput <- reactive({
    switch(input$dataset,
           "insurance" = insurance,
           "iris" = dat,
           "groceries" = groceries)
  })
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  output$contents <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  }) 
  dataset2Input <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  output$table <- renderTable({
    dataset2Input()
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset2, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataset2Input(), file, row.names = FALSE)
    }
  )

  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  })
}

shinyApp(ui = ui, server = server)
