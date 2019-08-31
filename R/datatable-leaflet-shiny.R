require(tidyverse)
require(shiny)
require(shinydashboard)
require(datasets)
require(DT)
require(leaflet)

my_server <- function(session, input, output) {
  
  output$my_datatable <- renderDT({
    
    quakes %>% 
      datatable()
    
  })
  
  observeEvent(input$my_datatable_rows_selected, {
    
    selected_lats <- eventReactive(input$my_datatable_rows_selected, {
      as.list(quakes$lat[c(unique(input$my_datatable_rows_selected))])
    })
    
    selected_longs <- eventReactive(input$my_datatable_rows_selected, {
      as.list(quakes$long[c(unique(input$my_datatable_rows_selected))])
    })
    
  })
  
  
}