# code for showing how to use downloadHandler to generate markdowns from a shiny
require(shiny)
require(shinythemes)
require(plotly)
require(datasets)
require(rmarkdown)
require(GGally)
require(janitor)
require(DT)
require(MASS)
require(tidyverse)


ui <- fluidPage(
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
          width = 12,
          br(),
          div(
            style = 'overflow-x: scroll; height: auto;',
            dataTableOutput(
              "dat_table",
              width = "100%"
            )
          )
        )
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
        ),
        br(),
        plotOutput(
          "resid_plots",
          width = "100%"
        )
      )
    )
  )
)

server <- function(input, output) {
  
  my_vals <- reactiveValues()
  
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
    
    my_vals$reg_dat <- dat
    
    dat
    
  })
  
  output$pairs_plot <- renderPlot({
    
    req(reg_dat())
    
    p <- reg_dat() %>%
      ggpairs() +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            panel.background = element_rect(fill = "#f2f2f2", color = "#f2f2f2"))
    
    my_vals$pairs_plot <- p
    
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
    
    my_vals$dep_var <- dep_var
    
    message("\n... pre-formula: ", paste0(dep_var, " ~ ", x_vars), "\n")
    
    my_formula <- paste0(dep_var, " ~ ", x_vars)
    
    my_vals$my_formula <- my_formula
    
    message("\n...reg_vars: ", x_vars, "\n")
    message("\n...formula: ", as.character(my_formula))
    
    reg_sum <- reg_dat() %>% 
      lm(noquote(my_formula), data = .) %>% 
      summary()
    
    reg_sum$call <- sym(my_formula)
    
    my_vals$reg_sum <- reg_sum
    
    reg_sum
    
  })
  
  # mod_vals <- reactiveValues()
  
  observeEvent(input$choose_data, {
    
    req(reg_dat())
    req(my_vals)
    
    mod <- reg_dat() %>% 
      lm(noquote(my_vals$my_formula), data = .)
    
    message("\n... calculationg residuals\n")
    
    stud_res <- MASS::studres(mod)
    my_vals$stud_res <- stud_res
    
    fit_vals <- mod$fitted.values
    my_vals$fit_vals <- fit_vals
    
    rp <- ggplot(NULL, aes(stud_res, fit_vals)) +
      geom_point() +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            panel.background = element_rect(fill = "#f2f2f2", color = "#f2f2f2"))
    
    my_vals$resid_plot <- rp
    
  })
  
  output$resid_plots <- renderPlot({
    
    req(my_vals)  
    
    my_vals$resid_plot

  })
  
  output$dat_table <- renderDataTable({
    
    req(reg_dat())
    
    t <- datatable(
      reg_dat(),
      width = "100%",
      rownames = FALSE,
      #filter = "top",
      extensions = c('Scroller', 'FixedColumns', 'Buttons'),  # ???
      options = list(
        buttons = c('csv', 'excel'),
        dom = "Bftlp",
        autoWidth = FALSE,
        deferRender = TRUE,  # ???
        scroller = TRUE,
        scrollY = 500,
        scrollX = TRUE
        #pageLength = 15,
        #fixedColumns = TRUE
        # columnDefs = list(
        #   list(
        #     width = "120",
        #     targets = "_all"
        #   )
        # )
      )
    )
      
    # only need to print some of the data in the report
    my_vals$dat_table <- head(reg_dat(), 15)
    
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
      
      # copy the report template into the temp directory
      file.copy(here::here("shiny_report_gen", "report_template.Rmd"), tempReport, overwrite = TRUE)

      # set up parameters to pass to to Rmd template
      # can also pass reactiveValues or reactive objects
      pass_params <- list(
        imported = my_vals
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
