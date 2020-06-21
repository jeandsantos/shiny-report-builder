library(shiny)
library(rmarkdown)
library(bookdown)
library(tidyverse)
library(pracma)
library(knitr)

report_path <- tempfile(fileext = ".Rmd")
file.copy("report.Rmd", report_path, overwrite = TRUE)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # import data
    df <- eventReactive(eventExpr = input$run_analysis, {
        
        inFile <- input$file1
        
        if (is.null(inFile)) return(NULL)
        
        data <- read.csv(inFile$datapath, header = input$contains_header)
        data
    })
    
    # Print input table
    # output$contents <- renderTable({ df() })

    summary_stats <- reactive({
        
        data.frame(
            mean = mean(as.numeric(df()[1,]), na.rm = TRUE),
            median = median(as.numeric(df()[1,]), na.rm = TRUE),
            min = min(as.numeric(df()[1,]), na.rm = TRUE),
            max = max(as.numeric(df()[1,]), na.rm = TRUE)
        ) %>% round(4)
    
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
             y = df()[1,], type="l", 
             xlab = "x",
             ylab = "y",
             main = "Distribution of values")
    )
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = "export_data.csv",
        content = function(file) {
            write.csv(summary_stats(), file, row.names = FALSE)
        }
    )
    
    report_timestamp <- format(Sys.time(), "%Y-%m-%d %H%M")
    
    output$downloadReport <- downloadHandler(
        filename = function() {paste0('report ',report_timestamp, '.', switch(input$format, pdf = 'pdf', html = 'html', docx = 'docx'))},
        content = function(file) {
            src <- normalizePath('report.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            
            out <- rmarkdown::render(input = 'report.Rmd',
                                     params = list(df = summary_stats(), date = report_timestamp),
                                     output_format = switch(input$format,
                                                            pdf = pdf_document(), 
                                                            html = html_document(), 
                                                            docx = word_document()
                                                            )
                                     )
            file.rename(out, file)
        }
    )
    
})

























