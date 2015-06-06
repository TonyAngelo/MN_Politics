library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
        
        plotOutput("distPlot"),
        
        selectInput(inputId = "width",
                    label = "hPVI width of bars:",
                    choices = c(1,5,10,25,50),
                    selected = 10
        ),
        
        selectInput(
            'chamber', 'Chamber', choices = c("Senate", "House")
        ),
        
        checkboxInput(inputId = "districts",
                      label = "Show individual districts",
                      value = TRUE
        )
    )
)