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

# get the state names and codes into vectors
# check to see if we have the state_codes file, if not create one
if(!file.exists("./data/state_codes.csv")){
    # state names/codes url
    state_code_url <- "http://voteview.com/state_codes_icpsr.htm"
    # load page
    state_code_html <- getParsedHTML(state_code_url)
    # extract the relevant data
    state_info <- xpathSApply(state_code_html, "//b", xmlValue)
    # split the text chunk on "enter"
    state_info <- strsplit(state_info,"\r\n")[[2]]
    # get the state names from the split text chuncks
    state_names <- sapply(state_info,function(x) sub("[ ]*$", "", sub("[0-9]{2} [A-Z]{2} ", "", x)),
                          USE.NAMES=FALSE)[1:length(state_info)-1]
    # get the state codes from the split text chuncks
    state_codes <- sapply(state_info,function(x) sub("[A-Z ]+$", "", x),
                          USE.NAMES=FALSE)[1:length(state_info)-1]
    
    # combine the names and codes for writing to a file
    state_full <- cbind(as.character(state_names),as.integer(state_codes))
    # write the data to a local file
    write.csv(state_full,file="./data/state_codes.csv")
} else {
    # read the names and codes from the file
    state_names <- read.csv("./data/state_codes.csv", stringsAsFactors=FALSE)[,2]
    state_codes <- read.csv("./data/state_codes.csv", stringsAsFactors=FALSE)[,3]
}

# get the party names and codes
# check to see if we have the party_codes file, if not create one
if(!file.exists("./data/party_codes.csv")){
    # party names/codes url
    party_code_url <- "http://voteview.com/party3.htm"
    # load page
    party_code_html <- getParsedHTML(party_code_url)
    # extract the relevant data
    party_info <- xpathSApply(party_code_html, "//b", xmlValue)[[1]]
    # split the text chunk on "enter"
    party_info <- strsplit(party_info,"\r\n")[[1]]
    # get the party names from the split text chuncks
    party_names <- sapply(party_info,function(x) sub("[ ]*$", "", sub("[ ]*[0-9]+[ ]{2}", "", x)),
                          USE.NAMES=FALSE)[1:length(party_info)]
    # get the party codes from the split text chuncks
    party_codes <- sapply(party_info,function(x) sub("[ ]*", "", sub("[ ]{2}[A-Za-z '-//.]+$", "", x)),
                          USE.NAMES=FALSE)[1:length(party_info)]
#     party_names[1] <- "ALL"
#     party_codes[1] <- "9998"
    
    # combine the names and codes for writing to a file
    party_full <- cbind(as.character(party_names),as.integer(party_codes))
    # write the data to a local file
    write.csv(party_full,file="./data/party_codes.csv")
} else {
    # read the names and codes from the file
    party_names <- read.csv("./data/party_codes.csv", stringsAsFactors=FALSE)[,2]
    party_codes <- read.csv("./data/party_codes.csv", stringsAsFactors=FALSE)[,3]
}

# create the column label vector
column_names <- c("Congress", "ICPSR", "StateCode", "District", "StateName", "Party", "Name", 
                  "FirstDimension", "Second Dimension", "First Dimension SE", "SecondDimensionSE", "FirstSecondCorr", 
                  "LogLikelihood", "Votes", "ClassificationErrors", "GeometricMeanProbability")
#### step 1: done ####

#### step 2: get data ####
## first check to see if there are new versions of the data set online

# check to see if there is a new version of DW-Nominate available
dw_nominate_url <- "http://voteview.com/dwnomin.htm"
dw_nominate_html <- getParsedHTML(dw_nominate_url)
# strings to search for
house_search_string <- "Legislator Estimates 1st to"
senate_search_string <- "Senator Estimates 1st to"
# check to see if the files are current, and if not, download new files
house_file_path <- checkDWNominateFileCurrent(dw_nominate_html,house_search_string)
# check to see if the files are current, and if not, download new files
senate_file_path <- checkDWNominateFileCurrent(dw_nominate_html,senate_search_string)

## with the freshness resolved, load the data

# read the file into memory
data_senate <- read.fwf(senate_file_path, col.names=column_names, stringsAsFactors=FALSE,
                        widths=c(4, 6, 3, 2, 8, 5, 13, 6, 7, 5, 7, 7, 14, 5, 5, 7))

# read the file into memory
data_house <- read.fwf(house_file_path, col.names=column_names, stringsAsFactors=FALSE,
                       widths=c(4, 6, 3, 2, 8, 5, 13, 6, 7, 5, 7, 7, 14, 5, 5, 7))
#### step 2: done ####

#### step 3: clean data ####
# combine the senate and house data
data_full <- rbind(data_senate,data_house)
# add the full state names        
data_full$StateName <- sapply(data_full$StateCode, function(x) state_names[state_codes %in% x], USE.NAMES = FALSE)
# add the party names
data_full$Party <- sapply(data_full$Party, function(x) party_names[party_codes %in% x], USE.NAMES = FALSE)
# reorder/drop columns
data_full <- data_full[,c(1,5,4,7,6,8,9)]
#### step 3: done ####

#### step 4: write to file ####
write.csv(data_full,file="./data/dwnominate.csv")