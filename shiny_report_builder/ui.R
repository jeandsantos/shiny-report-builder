#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# report_path <- tempfile(fileext = ".Rmd")
# file.copy("report.Rmd", report_path, overwrite = TRUE)
# 
# render_report <- function(input, output, params) {
#     rmarkdown::render(input,
#                       output_file = output,
#                       params = params,
#                       envir = new.env(parent = globalenv())
#     )
# }


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # App title ----
    titlePanel("Uploading Files"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            
            actionButton("run_analysis", "Run Analysis", width = "170px", icon = icon("bullseye")), br(),br(),
            
            # Button
            downloadButton("downloadData", "Export Data"), br(),
            downloadButton("report", "Generate report")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            tableOutput("contents"),
            DT::dataTableOutput(outputId = "input_DT_data_table"),
            # tableOutput("stats_table")
            
            shiny::plotOutput("plot_line")
            
        )
        
    )
)
)