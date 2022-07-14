#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Regression data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
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

                    actionButton("go", "Model Data")   
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    #Making the stuff that the button does
    modeled_data <- eventReactive(input$go, {
    model = lm(formula= y~x, 
        data=dataInput())
        print(model)
        return(model)
  })
      
    output$distPlot <- renderPlot({
        #plot(dataInput()$x,dataInput()$y)
        ggplot() + geom_point(aes(x = dataInput()$x, y = dataInput()$y))
       
                              })
    
    output$lmPlot <- renderPlot({
        model_summary <- summary(dataInput())
     intercept_value <- 0.46
     label1 = paste( "b=", intercept_value)
        
        slope_value <- 2.24
        label2 = paste( "slope=", slope_value)
        
       R_squared <- 0.9268
       label3 = paste("R squared =", R_squared)
        
        
        #plot(dataInput()$x,dataInput()$y)
        ggplot() + 
        geom_point(aes(x = dataInput()$x, y = dataInput()$y)) +
        geom_line(aes(x = dataInput()$x, y = predict(modeled_data(), newdata = dataInput())),color = 'red') +
        annotate("text", x=20, y=5, label=label1) +
        annotate("text", x=20, y=6, label=label2) +
        annotate("text", x=20, y=4, label=label3)
        
        
                   # + ggtitle ('modeled output with regression line')
                 #  + xlab('x values')
                 # + ylab('y value')
    })
    

    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}
                                 
#run the file
shinyApp(ui = ui, server = server)
