library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        # app title
        titlePanel("Minnesota 2015 hPVI Distribution"),
        
        mainPanel(
            
            helpText("Bars are bins, the width of which is defined by the 'hPVI width of bars' select box. 
                    For example, with the default setting of 10, the bar to left of the zero line represents 
                     the number of districts with hPVI<0 and hPVI>-10, negative numbers are districts that 
                     favor Republicans, positive numbers districts that favor Democrats"),
        
            # histogram
            plotOutput("distPlot"),
            
            # bin width selector
            selectInput(inputId = "width",
                        label = "hPVI width of bars:",
                        choices = c(1,5,10,25,50),
                        selected = 10
            ),
            # chamber seletor
            selectInput(
                'chamber', 'Chamber', choices = c("Senate", "House")
            ),
            # show/hide rug
            checkboxInput(inputId = "districts",
                          label = "Show individual districts",
                          value = TRUE
            )
        )
    )
)