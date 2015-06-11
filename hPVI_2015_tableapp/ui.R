library(shiny)
library(DT)

# Define the overall UI
shinyUI(
    fluidPage(
        # app title
        titlePanel("Minnesota 2015 hPVI Table"),
        
        # Create a row for selectInputs
        fluidRow(
            
            column(4, 
                   selectInput("chamber", 
                               "Chamber:", 
                               c("All", "House", "Senate"))
            ),
            column(4, 
                   selectInput("party", 
                               "Party:", 
                               c("All", "DFL", "R"))
            )
           
        ),
        
        # Create a new row for the table.
        fluidRow(
            dataTableOutput(outputId="table")
        )    
    )  
)