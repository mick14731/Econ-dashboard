library(shiny)  
source("functions.R")
source("cpi maker.R")
source("Lab_force.R")


ui <- shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("CPI without",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectizeInput("choice_Ex","Sectors to exclude:",
                         choices = colnames(CPI_INDEX.xts),
                         selected = colnames(CPI_INDEX.xts)[5],
                         multiple=TRUE,
                         options = list(placeholder = "Componant",maxItems = 10)),
          selectizeInput("choice_add","Sectors to combine:",
                         choices = colnames(CPI_INDEX.xts),
                         selected = colnames(CPI_INDEX.xts)[5],
                         multiple=TRUE,
                         options = list(placeholder = "Componant",maxItems = 10))),
        
        mainPanel(
          width = 9,
          fluidRow(column(width = 9,dygraphOutput("excluded"))),
          fluidRow(column(width = 9, verbatimTextOutput("exTest"))),
          fluidRow(column(width = 9,dygraphOutput("constructed"))),
          fluidRow(column(width = 9, verbatimTextOutput("addTest"))))

      )
    ),
    tabPanel("Labour market",
             sidebarLayout(
               sidebarPanel(
                 witdh=3,
                 selectizeInput("labStat","Measure to view:",
                                choices = lab_chars,
                                selected = "Unemployment rate",
                                multiple=FALSE,
                                options = list(placeholder = "Labour measure")),
                 sliderInput("year",h3("Select year:"),
                             min = 1976, max = 2018, step = 1, value = 2000,
                             animate = FALSE)),
               
               mainPanel(leafletOutput("labour"))
             
    ))
  )  
  
))

server <- shinyServer(function(input, output,session) {

#####  
  graph_data_ex <- reactive(
    ind_sub(base = CPI_INDEX.xts$`All-items`*CPI_Weights.xts$`All-items`/RV$`All-items`,
            choices = input$choice_Ex,
            indexs = CPI_INDEX.xts,
            refValues = RV,
            Weights = CPI_Weights.xts)
    
  )
#####  
  graph_data_add <- reactive(
    ind_add(choices = input$choice_add,
            indexs = CPI_INDEX.xts,
            refValues = RV,
            Weights = CPI_Weights.xts)
  )
#####
  output$constructed <- renderDygraph(
    dygraph(graph_data_add())%>%
      dyRangeSelector()
  )
#####  
  output$excluded <- renderDygraph(
    dygraph(graph_data_ex())%>%
      dyRangeSelector()
  )
#####  
  output$exText <- renderText( c("CPI inflation excluding:",paste(input$choice_Ex, collapse = ", " )))
  output$addText <- renderText( c("CPI inflation of:",paste(input$choice_add, collapse = ", " )))
#####
  
  lng.center <- -99
  lat.center <- 55
  zoom.def <- 3
  

  
  get_data <- reactive({
    lab_data[which(lab_data$year == input$year & lab_data$`Labour force characteristics`==input$labStat),]
  })


  pal <- reactive({
    colorNumeric("viridis", domain = legend_values())
  })
  
  legend_values <- reactive(
    switch(input$labStat,ranges[[input$labStat]])
  )
  
  output$labour <- renderLeaflet({

    leaflet(data = data.p) %>%
      addProviderTiles("OpenStreetMap.Mapnik", options = providerTileOptions(opacity = 1), group = "Open Street Map") %>%
      setView(lng = lng.center, lat = lat.center, zoom = zoom.def) %>%
      addPolygons(group = 'base',
                  fillColor = 'transparent',
                  color = 'black',
                  weight = 1.5)  %>%
      addLegend(pal = pal(), 
                values = legend_values(),
                opacity = 0.7, 
                title = NULL,
                position = "topright")
    
  })
  
  observe({
    l_data <- get_data()  
    leafletProxy('labour', data = l_data) %>%
      clearGroup('polygons') %>%
      addPolygons(group = 'polygons',
                  fillColor = ~pal()(VALUE),
                  fillOpacity = 0.9,
                  color = 'black',
                  weight = 1.5)
  })


#####


  
  
})

#####

# Run the application 
shinyApp(ui = ui, server = server)