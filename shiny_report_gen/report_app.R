# code for showing how to use downloadHandler to generate markdowns from a shiny
require(shiny)
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
      uiOutput(
        "choose_ggpairs_cols",
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
    
    # dataset returned to reg_dat "switches" based on the value of input$choose_data
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
    
    # to pass to markdown incase we need the full dataset
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
  
  output$choose_ggpairs_cols <- renderUI({
    
    # can't access values/onjects from server in ui, so this selectInput needs to be made
    # as a renderUI
    
    # capture variables available from the selected dataset
    my_choices <- reg_dat() %>% 
      names()
    
    selectInput(
      "choose_ggpairs_cols",
      "Cols for ggpairs in report",
      choices = my_choices,
      selected = my_choices[1:4],
      multiple = TRUE
    )
    
  })
  
  # capture the user's chosen columns and add to reactiveValues object
  observeEvent(input$choose_ggpairs_cols, {
    
    # update the vector of column names based on the selected 
    # columns from input$choose_ggpairs_cols so the correct names
    # are available to be passed to the markdown
    my_vals$user_cols <- input$choose_ggpairs_cols
    
  })
  
  output$reg_out <- renderPrint({
    
    req(reg_dat())
    
    # get names of dataset
    reg_names <- colnames(reg_dat())
    
    # get length of names
    len <- length(reg_names)
    message("\n... length of req_dat(): ", length(reg_dat()), "\n")
    
    # based on how I ordered the variables of each dataset earlier,
    # get all the predictor variables' names and paste them together separated
    # by a "+"
    x_vars <- reg_names[2:len] %>% 
      paste(collapse = " + ")
    
    # assign dep_var to reactiveValues object for use in markdown in-line text
    dep_var <- reg_names[1]
    my_vals$dep_var <- dep_var
    
    message("\n... pre-formula: ", paste0(dep_var, " ~ ", x_vars), "\n")
    
    # create formula for the model
    my_formula <- paste0(dep_var, " ~ ", x_vars)
    
    # store for availability
    my_vals$my_formula <- my_formula
    
    message("\n...reg_vars: ", x_vars, "\n")
    message("\n...formula: ", as.character(my_formula))
    
    reg_mod <- reg_dat() %>% 
      lm(noquote(my_formula), data = .)
    
    # store for availability
    my_vals$reg_mod <- reg_mod
    
    # get summary of model
    reg_sum <- reg_mod %>% 
      summary()
    
    # store for availability
    my_vals$reg_sum <- reg_sum
    
    # store as sym for availability, storing as character string throws error for some reason
    reg_sum$call <- sym(my_formula)
    
    # return to renderPrint
    reg_sum
    
  })
  
  # observe the value of input$choose_data, including it's default value
  observeEvent(input$choose_data, {
    
    # make sure reg_dat() and my_vals, or reactiveValues object,
    # are observed/noticed
    req(reg_dat())
    req(my_vals)
    
    message("\n... calculating residuals\n")
    
    # create and store studentized ("leave-one-out") for availability
    stud_res <- MASS::studres(my_vals$reg_mod)
    my_vals$stud_res <- stud_res
    
    message("\n... getting fitted values\n")
    
    # create and store fitted values for availability
    fit_vals <- my_vals$reg_mod$fitted.values
    my_vals$fit_vals <- fit_vals
    
    message("\n... creating rp\n")
    
    # create studentized residuals vs fitted values plot
    rp <- ggplot(NULL, aes(stud_res, fit_vals)) +
      geom_point() +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            panel.background = element_rect(fill = "#f2f2f2", color = "#f2f2f2"))
    
    message("\n... storing rp\n")
    # store for availability
    my_vals$resid_plot <- rp
    
  })
  
  output$resid_plots <- renderPlot({
    
    req(my_vals$resid_plot)  
    
    # return plot to renderPlot
    my_vals$resid_plot

  })
  
  output$dat_table <- renderDataTable({
    
    req(reg_dat())
    
    # make datatable
    t <- datatable(
      reg_dat(),
      width = "100%",
      rownames = FALSE,
      filter = "top",
      extensions = c('Scroller', 'FixedColumns', 'Buttons'),  # ???
      options = list(
        buttons = c('csv', 'excel'),
        dom = "Bftlp",
        autoWidth = TRUE,
        deferRender = TRUE,  # ???
        scroller = TRUE,
        scrollY = 500,
        scrollX = TRUE
      )
    )
      
    # only need to print some of the data in the report
    my_vals$dat_table <- head(reg_dat(), 15)
    
    # return to renderDataTable
    t
    
  })
  
  output$report_gen <- downloadHandler(

    filename = "sample_report.pdf",
    content = function(file) {
      # copy markdown report file to a temporary directory before knitting it with the
      # selected dataset. This is useful if we don't have write permissions for the current 
      # working directory
      temp_report <- file.path(tempdir(), "report_template.Rmd")
      message("\n... temp_report path: ", temp_report, "\n")
      
      # copy the report template into the temp directory
      file.copy(here::here("shiny_report_gen", "report_template.Rmd"), temp_report, overwrite = TRUE)

      # create a named list of parameters to pass to to Rmd template.
      # can also pass reactiveValues or reactive objects
      pass_params <- list(
        imported = my_vals
      )

      # knit the document, passing in the `pass_params` list, and evaluate it in a
      # child of the global environment (this isolates the code in the document
      # from the code in the app).
      rmarkdown::render(
        temp_report,
        output_file = file,
        params = pass_params,
        envir = new.env(parent = globalenv())
      )
      
    }

  )
  
}

shinyApp(ui, server)
