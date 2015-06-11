library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
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