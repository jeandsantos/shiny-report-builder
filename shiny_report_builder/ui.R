library(shiny)

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
            
            # Input: Checkbox if file has header ----
            checkboxInput(inputId = "contains_header",
                          label = "Files container header",
                          value = TRUE),
            
            # Input: Select separator ----
            radioButtons(inputId = "sep", 
                         label = "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons(inputId = "quote", 
                         label = "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Input: Report format ----
            radioButtons('format', 'Document format', c('pdf', 'docx', 'html'),
                         inline = FALSE), 
            
            # # Horizontal line ----
            # tags$hr(),
            
            actionButton(inputId = "run_analysis", label = "Run Analysis", width = "170px", icon = icon("bullseye")), br(),br(),
            
            # Button
            downloadButton("downloadData", "Export Data"), br(), br(),
            downloadButton('downloadReport', "Generate Report"),
            # textInput(inputId = "report_author", label = "Report Author", value = "")
            
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            DT::dataTableOutput(outputId = "input_DT_data_table"),
            shiny::plotOutput(outputId = "plot_line")
            
        )
        
    )
)
)
