library(shiny)
library(DT)

shinyUI(
    fluidPage(
        # app title
        titlePanel("DW-Nominate Explorer"),
        
        # Create a new row for the table.
        fluidRow(
            dataTableOutput(outputId="data")
        ) 
    )
)