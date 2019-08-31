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
    
    # this is the data that will be passed to the leaflet in the addCircleMarkers argument
    coord_df <- reactive({
      tibble(lat = unlist(selected_lats()),
             lng = unlist(selected_longs()))
    })
    
  })
  
  
  output$my_leaflet <- renderLeaflet({
    
    coord_df() %>% 
      leaflet() %>% 
    
  })
  
  
}


my_sidebar <- dashboardSidebar()

my_header <- dashboardHeader(title = "Fiji Earthquakes")

my_body <- dashboardBody(
  
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,
      DTOutput(
        "my_datatable"
      )
    )
  ),
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,
      leafletOutput(
        "my_leaflet"
      )
    )
  )
  
)





