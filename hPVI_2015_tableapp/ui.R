library(shiny)
library(DT)

# Define the overall UI
shinyUI(
    fluidPage(
        
        
        titlePanel("Minnesota 2015 hPVI Table"),
        
        # Create a new Row in the UI for selectInputs
        fluidRow(
            column(2, 
                   selectInput("chamber", 
                               "Chamber:", 
                               c("All", "House", "Senate"))
                                 #unique(as.character(data$chamber))))
            ),
            column(2, 
                   selectInput("party", 
                               "Party:", 
                               c("All", "DFL", "R"))
                                 #unique(as.character(data$party))))
            )     
        ),
        # Create a new row for the table.
        fluidRow(
            dataTableOutput(outputId="table")
        )    
    )  
)