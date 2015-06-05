library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
        selectInput(
            'chamber', 'Chamber', choices = c("Senate", "House"),
            selectize = FALSE
        ),
        checkboxInput(inputId = "districts",
                      label = "Show individual districts",
                      value = TRUE
        ),
        selectInput(inputId = "width",
                    label = "hPVI width of bins:",
                    choices = c(1,5,10,25,50),
                    selected = 10),
    
        plotOutput("distPlot")
    )
)