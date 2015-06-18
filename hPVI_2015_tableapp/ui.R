library(shiny)
library(DT)

# Define the overall UI
shinyUI(
    fluidPage(
        # app title
        titlePanel("Minnesota 2015 hPVI Table"),
        
        mainPanel(
            
            helpText("hPVI is the pretty looking version of rPVI.
                     If you sort by rPVI you will see the most DFL or Republican districts at the top of the list.
                     ABS is the absolute value of rPVI. If you sort by ABS, you will see the most and least 
                     competitive districts at the top of the list."),
            
            # Create a row for selectInputs
            fluidRow(
                
                column(6, 
                       selectInput("chamber", 
                                   "Chamber:", 
                                   c("All", "House", "Senate"))
                ),
                column(6, 
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
)