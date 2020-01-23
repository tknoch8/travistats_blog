# code for showing how to use downloadHandler to renerate markdowns from a shiny

server <- function(input, output) {

  output$report_handler <- downloadHandler(
    
    filename = "sample_report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "png_to_pdf.Rmd")
      message("\n... tempReport path: ", tempReport, "\n")
      
      file.copy("png_to_pdf.Rmd", tempReport, overwrite = TRUE)
      
      # set up parameters to pass to to Rmd template
      # can also pass reactiveValues or reactive objects
      pass_params <- list(
        plots_for_export = plots_for_export
        # my_checkbox_2_value           = input$my_checkbox_2,
        # poly_slider_main_value        = input$poly_slider_main,
        # my_province_select_2_value    = input$my_province_select_2,
        # click_layer_boxes_long_value  = input$click_layer_boxes_long,
        # my_province_select_1_value    = input$my_province_select_1,
        # click_layer_boxes_long_value  = input$click_layer_boxes_long
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