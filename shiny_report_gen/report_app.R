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
      fluidRow(
        column(
          width = 6,
          tableOutput(
            "dat_table"
          )
        )
        # column(
        #   width = 6,
        #   plotOutput(
        #     "resid_plots"
        #   )
        # )
      ),
      fluidRow(
        plotOutput(
          "pairs_plot"
        )
      ),
      br(),
      fluidRow(
        verbatimTextOutput(
          "reg_out"
        )
      )
    )
  )
)

server <- function(input, output) {
  
  to_export <- reactiveValues()
  
  reg_dat <- reactive({
    
    ###---
    # convert dependent vars to a seltInput that depends on input$choose data
    # so user can choose dependent variable
    ###---
    
    dat <- switch(
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
    
    to_export$reg_dat <- dat
    
    dat
    
  })
  
  output$pairs_plot <- renderPlot({
    
    req(reg_dat())
    
    p <- reg_dat() %>%
      ggpairs() +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            panel.background = element_rect(fill = "#f2f2f2", color = "#f2f2f2"))
    
    to_export$pairs_plot <- p
    
    p
    
  })
  
  output$reg_out <- renderPrint({
    
    req(reg_dat())
    
    reg_names <- colnames(reg_dat())
    
    len <- length(reg_names)
    message("\n... length of req_dat(): ", length(reg_dat()), "\n")
    
    x_vars <- reg_names[2:len] %>% 
      paste(collapse = " + ")
    
    dep_var <- reg_names[1]
    
    message("\n... pre-formula: ", paste0(dep_var, " ~ ", x_vars), "\n")
    
    my_formula <- paste0(dep_var, " ~ ", x_vars)
    
    message("\n...reg_vars: ", x_vars, "\n")
    message("\n...formula: ", as.character(my_formula))
    
    reg_sum <- reg_dat() %>% 
      lm(noquote(my_formula), data = .) %>% 
      summary()
    
    reg_sum$call <- sym(my_formula)
    
    to_export$reg_sum <- reg_sum
    
    reg_sum
    
  })
  
  # output$resid_plots <- plotOutput({
  #   
  #   req(reg_dat())
  #   
  #   mod <- reactive({
  #     reg_dat() %>% 
  #       lm(noquote(my_formula), data = .)
  #   })
  #   
  #   stud_res <- reactive({ studres(mod()) })
  #   
  #   plot(stud_res(), mod()$fitted.values)
  #   
  # })
  
  # output$formula_text <- renderText({
  #   
  #   isolate(vals$call)
  #   
  # })
  
  output$dat_table <- renderTable({
    
    req(reg_dat())
    t <- head(reg_dat())
    
    to_export$dat_table <- t
    
    t
    
  })
  
  output$report_gen <- downloadHandler(

    filename = "sample_report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_template.Rmd")
      message("\n... tempReport path: ", tempReport, "\n")

      file.copy("report_template.Rmd", tempReport, overwrite = TRUE)

      # set up parameters to pass to to Rmd template
      # can also pass reactiveValues or reactive objects
      pass_params <- list(
        imported = to_export
      )

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = pass_params,
        envir = new.env(parent = globalenv())
      )

    }

  )
  
}

shinyApp(ui, server)





