# code for showing how to use downloadHandler to generate markdowns from a shiny
require(shiny)
require(shinythemes)
require(plotly)
require(datasets)
require(rmarkdown)
require(GGally)
require(janitor)
# require(corrgram)
require(MASS)
require(tidyverse)


ui <- fluidPage(
  theme = shinytheme("slate"),
  tags$style(type = "text/css",".recalculating {opacity: 1.0;}"),
  # includeCSS(path = here::here("www", "styles.css")),
  titlePanel("Report Generator"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      downloadButton(
        "report_gen",
        "Create Report"
      ),
      br(),
      selectInput(
        "choose_data",
        "Select data to use",
        choices = c("mtcars", "chickens", "flowers")
      )
    ),
    mainPanel(
      width = 10,
      column(
        width = 12,
        fluidRow(
          tableOutput(
            "dat_table"
          ),
          br(),
          plotOutput(
            "pairs_plot"
          )
        ),
        fluidRow(
          column(
            width = 12,
            verbatimTextOutput(
              "formula_text"
            ),
            verbatimTextOutput(
              "reg_out"
            )
          )
        )
    )
  )
)

server <- function(input, output) {
  
  reg_dat <- reactive({
    
    ###---
    # convert dependent vars to a seltInput that depends on input$choose data
    # so user can choose dependent variable
    ###---
    
    switch(
      input$choose_data,
      "mtcars" = datasets::mtcars %>% dplyr::select(mpg, dplyr::everything()),
      "chickens" = as_tibble(datasets::ChickWeight) %>% 
        dplyr::select(-Chick) %>% 
        dplyr::select(weight, everything()),
      "flowers" = datasets::iris3 %>% 
        as.data.frame() %>% 
        janitor::clean_names() %>% 
        dplyr::select(sepal_l_setosa, everything())
    )
    
  })
  
  output$pairs_plot <- renderPlot({
    
    req(reg_dat())
    
    reg_dat() %>%
      ggpairs() +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            panel.background = element_rect(fill = "#f2f2f2", color = "#f2f2f2"))
    
  })
  
  vals <- reactiveValues()
  
  output$reg_out <- renderPrint({
    
    req(reg_dat())
    
    x_vars <- names(reg_dat()[2:length(reg_dat())]) %>% 
      paste(collapse = " + ")
    
    my_formula <<- as.formula(paste0(names(reg_dat()[1]), " ~ ", x_vars))
    
    message("\n...reg_vars: ", x_vars, "\n")
    
    reg_sum <- reg_dat() %>% 
      lm(my_formula, data = .) %>% 
      summary()
    
    vals$call <- my_formula
    
    reg_sum
    
  })
  
  # output$resid_plots <- plotOutput({
  #   
  #   stud_res <- studres(
  #     reg_dat() %>% 
  #       lm(my_formula, data = .)
  #   )
  #   
  #   
  #   
  # })
  
  output$formula_text <- renderText({
    
    isolate(vals$call)
    
  })
  
  output$dat_table <- renderTable({
    
    req(reg_dat())
    head(reg_dat())
    
  })
  
  # output$report_handler <- downloadHandler(
  #   
  #   filename = "sample_report.pdf",
  #   content = function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "png_to_pdf.Rmd")
  #     message("\n... tempReport path: ", tempReport, "\n")
  #     
  #     file.copy("png_to_pdf.Rmd", tempReport, overwrite = TRUE)
  #     
  #     # set up parameters to pass to to Rmd template
  #     # can also pass reactiveValues or reactive objects
  #     pass_params <- list(
  #       plots_for_export = plots_for_export
  #     )
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(
  #       tempReport,
  #       output_file = file,
  #       params = pass_params,
  #       envir = new.env(parent = globalenv())
  #     )
  #     
  #   }
  #   
  # )
  
}

shinyApp(ui, server)





