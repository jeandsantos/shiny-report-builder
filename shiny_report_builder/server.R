#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rmarkdown)
library(bookdown)

report_path <- tempfile(fileext = ".Rmd")
file.copy("report.Rmd", report_path, overwrite = TRUE)

render_report <- function(input, output, params) {
    rmarkdown::render(input,
                      output_file = output,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    
    df <- eventReactive(eventExpr = input$run_analysis, {
        
        inFile <- input$file1
        
        if (is.null(inFile)) return(NULL)
        
        data <- read.csv(inFile$datapath, header = TRUE)
        data
    })
    

    output$contents <- renderTable({ df() })
    
    # stats <- data.frame(mean = mean(df[1,]))
    
    summary_stats <- reactive({
        
        data.frame(
        mean = mean(as.numeric(df()[1,]), na.rm = TRUE),
        median = median(as.numeric(df()[1,]), na.rm = TRUE),
        min = min(as.numeric(df()[1,]), na.rm = TRUE),
        max = max(as.numeric(df()[1,]), na.rm = TRUE)
        )
    
    })
    
    
    output$input_DT_data_table <- DT::renderDataTable(
        DT::datatable(summary_stats(),
        # colnames = c("Mean", "Min", "Max"),
        extensions = c("Buttons","FixedColumns", "Scroller", "KeyTable"), 
        options = list(
            pageLength = 3,
            deferRender = F,
            dom = "t",
            # buttons = c("copy", "csv"),
            autoWidth = TRUE,
            keys = TRUE)
        )
    )
    
    output$plot_line <- renderPlot(
        plot(x = 1:length(df()[1,]),
             y = df()[1,])
    )
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = "export_data.csv",
        content = function(file) {
            write.csv(df(), file, row.names = FALSE)
        }
    )

    
    output$report <- downloadHandler(
            filename = "report.html",
            content = function(file) {
                params <- list(df = df())
                callr::r(
                    render_report,
                    list(input = report_path, output = file, params = params)
                )
            }
        )
    
})

























