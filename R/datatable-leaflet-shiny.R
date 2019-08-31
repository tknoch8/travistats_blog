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
  
  output$selected_rows <- 
    eventReactive()
  
  
}