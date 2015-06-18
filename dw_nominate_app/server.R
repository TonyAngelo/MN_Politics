# required packages
library(shiny)
library(DT)

shinyServer(function(input, output) {
    
    # load the data with a progress bar because it takes a minute
    withProgress(message = 'Loading App Data', value = 0, {
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/2, detail = "...")
        
        ## load the data
        state_names <- read.csv("./data/state_codes.csv", stringsAsFactors=FALSE)[,2]
        party_names <- read.csv("./data/party_codes.csv", stringsAsFactors=FALSE)[,2]
        data_full <- read.csv("./data/dwnominate.csv", stringsAsFactors=FALSE)[,2:ncol(data_full)]
        
        # Increment the progress bar, and update the detail text.
        incProgress(2/2, detail = "finished")
    })
    
    # render selectBox for selecting congress
    output$congressSel = renderUI({
        congress_choices <- c("ALL",as.character(1:113))
        names(congress_choices) <- congress_choices
        selectInput(inputId='congress', label='Congress', choices=congress_choices, 
                    selected="ALL")
    })
    
    # render selectBox for selecting party
    output$partySel = renderUI({
        party_choices <- c("ALL",as.character(party_names))
        names(party_choices) <- party_choices
        selectInput(inputId='party', label='Party', choices=party_choices, 
                    selected="ALL")
    })
    
    # render selectBox for selecting state
    output$stateSel = renderUI({
        state_choices <- c("ALL",as.character(sort(state_names)))
        names(state_choices) <- state_choices
        selectInput(inputId='state', label='State', choices=state_choices, 
                    selected="ALL")
    })
    
    # render table of data
    output$data <- renderDataTable({ 
        # create temp data set
        myData <- data_full
        # if a congress other than "ALL" has been selected
        if(!is.null(input$congress) && input$congress!="ALL"){
            # filter the data based on the selection
            myData <- myData[myData$Congress==input$congress,]
        }
        # if a party other than "ALL" has been selected
        if(!is.null(input$party) && input$party!="ALL"){
            # filter the data based on the selection
            myData <- myData[myData$Party==input$party,]
        }
        # if a state other than "ALL" has been selected
        if(!is.null(input$state) && input$state!="ALL"){
            # filter the data based on the selection
            myData <- myData[myData$StateName==input$state,]
        }
        # data to be displayed in table
        myData 
    }, options = list(
            # default items per page
            pageLength = 25,
            orderClasses = TRUE,
            order = list(0, 'desc')
            ), rownames = FALSE
    )
})