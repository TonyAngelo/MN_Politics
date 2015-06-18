require(shiny)
require(DT)

shinyUI(
    fluidPage(
        # app title
        titlePanel("DW-Nominate Explorer"),
        
        # Create a row for selectInputs
        fluidRow(
            column(4, 
                   uiOutput('congressSel')  
            ),
            column(4, 
                   uiOutput('partySel')
            ),
            column(4, 
                   uiOutput('stateSel')
            )
        ),
        
        # Create a new row for the table.
        fluidRow(
            dataTableOutput("data")
        ) 
    )
)