# required packages
library(shiny)
library(DT)
library(XML)
library(httr)

if(!file.exists("data")){
    dir.create("data")
}

getParsedHTML <- function(url){
    html <- GET(url)
    content <- content(html,as="text")
    htmlParse(content, asText=TRUE)
}

getDWNominateLinkIndex <- function(x, html){
    grep(x, sapply(xpathSApply(html, "//a", xmlValue),function(x) sub("\r\n","",x), USE.NAMES=FALSE))[1]
}

checkDWNominateFileCurrent <- function(html,string){
    # extract the url from the html
    url <- xpathSApply(html, "//a", xmlAttrs)[getDWNominateLinkIndex(string, html)]
    # create the file path that should exist if that file is current
    file_path <- paste("./data/", sub("ftp://voteview.com/junkord/", "", url), sep="")
    #check to see if that path exists
    if(!file.exists(file_path)){
        download.file(url,file_path)
    }
    return(file_path)
}

shinyServer(function(input, output) {
    
    # update ui with app loading info
    #output$loading <- renderText({"App is loading DW-Nominate Data..."})
    
    withProgress(message = 'Loading Data', value = 0, {
        
        progIncs <- 5
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/progIncs, detail = "Loading State Data")
    
        # get the state names and codes into vectors
        if(!file.exists("./data/state_codes.csv")){
            state_code_url <- "http://voteview.com/state_codes_icpsr.htm"
            state_code_html <- getParsedHTML(state_code_url)
            # pull out the names/districts of the links and the links themselves
            state_info <- xpathSApply(state_code_html, "//b", xmlValue)
            state_info <- strsplit(state_info,"\r\n")[[2]]
            state_names <- sapply(state_info,function(x) sub("[ ]*$", "", sub("[0-9]{2} [A-Z]{2} ", "", x)),
                                  USE.NAMES=FALSE)[1:length(state_info)-1]
            state_codes <- sapply(state_info,function(x) sub("[A-Z ]+$", "", x),
                                  USE.NAMES=FALSE)[1:length(state_info)-1]
            state_full <- cbind(as.character(state_names),as.integer(state_codes))
            state_full <- rbind(state_full,c("ALL",99))
            
            # write the data to a local file
            write.csv(state_full,file="./data/state_codes.csv")
        } else {
            state_names <- read.csv("./data/state_codes.csv", stringsAsFactors=FALSE)[,2]
            state_codes <- read.csv("./data/state_codes.csv", stringsAsFactors=FALSE)[,3]
        }
        
        
        #### get the dw-naminate data
        
        # Increment the progress bar, and update the detail text.
        incProgress(2/progIncs, detail = "Checking Data")
        
        # create a column label vector
        column_names <- c("Congress", "ICPSR", "StateCode", "District", "StateName", "Party", "Name", 
            "FirstDimension", "Second Dimension", "First Dimension SE", "SecondDimensionSE", "FirstSecondCorr", 
            "LogLikelihood", "Votes", "ClassificationErrors", "GeometricMeanProbability")
        
        # check to see if there is a new version of DW-Nominate available
        dw_nominate_url <- "http://voteview.com/dwnomin.htm"
        dw_nominate_html <- getParsedHTML(dw_nominate_url)
        
        # strings to search for
        house_search_string <- "Legislator Estimates 1st to"
        senate_search_string <- "Senator Estimates 1st to"
        
        # Increment the progress bar, and update the detail text.
        incProgress(3/progIncs, detail = "Loading House Data")
        
        # check to see if the files are current, and if not, download new files
        house_file_path <- checkDWNominateFileCurrent(dw_nominate_html,house_search_string)
        
        # read the file into memory
        data_house <- read.fwf(house_file_path, col.names=column_names, stringsAsFactors=FALSE,
                               widths=c(4, 6, 3, 2, 8, 5, 14, 5, 7, 5, 7, 7, 14, 5, 5, 7))
        
        # Increment the progress bar, and update the detail text.
        incProgress(4/progIncs, detail = "Loading Senate Data")
        
        # check to see if the files are current, and if not, download new files
        senate_file_path <- checkDWNominateFileCurrent(dw_nominate_html,senate_search_string)
        
        # read the file into memory
        data_senate <- read.fwf(senate_file_path, col.names=column_names, stringsAsFactors=FALSE,
            widths=c(4, 6, 3, 2, 8, 5, 14, 5, 7, 5, 7, 7, 14, 5, 5, 7))
        
        # Increment the progress bar, and update the detail text.
        incProgress(5/progIncs, detail = "Combining Data")
        
        data_full <- rbind(data_senate,data_house)
        data_full$StateName <- sapply(data_full$StateCode, function(x) state_names[state_codes %in% x])
        data_full <- data_full[,c(1,5,4,7,6,8,9)]
        
    })
    
    # update ui with app loading info
    output$data <- renderDataTable({
        data_full
    }, options = list(
        pageLength = 25,
        orderClasses = TRUE
        ), rownames = FALSE)
})