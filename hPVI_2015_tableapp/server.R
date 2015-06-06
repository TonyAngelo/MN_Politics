# required packages
library(shiny)
library(DT)

# Define a server for the Shiny app
shinyServer(function(input, output) {
    
    # Filter data based on selections
    output$table <- renderDataTable({
        data <- read.csv("./data/mn_hpvi_2015.csv")[,2:10]
        data$hpvi <- as.character(data$hpvi)
        # round the raw numbers
        data[,5] <- round(data[,5],2)
        
        data <- data[,c(1,9,2,3,4,5)]
        
        data$abs <- abs(data$rpvi)
        
        if (input$chamber != "All"){
            data <- data[data$chamber == input$chamber,]
        }
        if (input$party != "All"){
            data <- data[data$party == input$party,]
        }
        
        data
    }, options = list(
        lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
        pageLength = 25,
        orderClasses = TRUE,
        
        order = list(list(5, 'desc'))
        ), rownames = FALSE
    )
    
})