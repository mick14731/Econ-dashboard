library(shiny)  
library(dygraphs)

#source("CPI.R")
source("cpi maker.R")

ui <- shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("CPI without",
      sidebarLayout(
        sidebarPanel(
          selectizeInput("c","Sectors to exclude:",
                         choices = colnames(CPI_INDEX.xts),
                         options = list(multiple=T, placeholder = "Meat"))
        ),
        mainPanel(fluidRow(width = 8,dygraphOutput("constructed")),
                  fluidRow(verbatimTextOutput("text"))
        )

      )
    )
  )  
  
))





server <- shinyServer(function(input, output,session) {
  
  # updateSelectizeInput(session,'c', choices = colnames(CPI_INDEX.xts), server = TRUE)
  # 
  # graph_data <- reactive(
  #   ind_sub(base = CPI_INDEX.xts$`All-items`*CPI_Weights.xts$`All-items`/RV$`All-items`,
  #           choices = input$c,
  #           indexs = CPI_INDEX.xts,
  #           refValues = RV,
  #           Weights = CPI_Weights.xts)
  #  
  # )
  #
  graph_data <- ind_sub(base = CPI_INDEX.xts$`All-items`*CPI_Weights.xts$`All-items`/RV$`All-items`,
               choices = c("Meat"),
               indexs = CPI_INDEX.xts,
               refValues = RV,
               Weights = CPI_Weights.xts)
  output$constructed <- renderDygraph(
    dygraph(graph_data)
     )
  output$test <- renderText(input$c)
})



# Run the application 
shinyApp(ui = ui, server = server)