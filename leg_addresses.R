## script for getting the home addresses of legislators

# load libraries
library(XML)
library(httr)

# url of page with house member page links
senate_url <- "http://www.senate.leg.state.mn.us/members/"
# get the page with the links
senate_html <- GET(senate_url)
# parse the links page
senate_content <- content(senate_html,as="text")
parsedSenateHtml <- htmlParse(senate_content, asText=TRUE)
# pull out the names/districts of the links and the links themselves
senate_info <- xpathSApply(parsedSenateHtml, "//table", xmlValue)[4:70]

#house_links <- xpathSApply(parsedSenateHtml, "//option", xmlAttrs)[2:135]

# create the data frame for storing the address info
house_address <- data.frame(
    city=character(),
    #zip=character(),
    #street=character(),
    distanceToCapitol=integer(),
    stringsAsFactors=FALSE)

# include the distance functions
source("get_distances.R")

# vector to keep track of pages with no bio info
no_info <- list()

# loop over the member page links
for(i in 1:134){
    # get the member page html
    mem_html <- GET(house_links[[i]])
    # parse the member page html
    mem_content <- content(mem_html,as="text")
    parsedMemHtml <- htmlParse(mem_content, asText=TRUE)
    # get the paragraphs on the page
    mem_ps <- xpathSApply(parsedMemHtml, "//p", xmlValue)
    # find the paragraph with the "Home:" heading
    bio_p <- mem_ps[grep("\r\nHome:\r\n", mem_ps)]
    # split the paragraph and grab the first part
    address <- strsplit(bio_p,";")[[1]][1]
    # make sure it's the address
    if(length(grep("Occupation: ", address))>0){ # doesn't contain address
        if(i==51 | i==58 | i==60 | i==80 | i==19 | i==21 | i==57 | i==84 | i==101 | i==124 | i==128){
            address <- gsub(" [0-9-]*Occupation:.*$","",gsub("^[\r\n]*[ ]+[\r\n]*[A-Za-z:]{5}[\r\n]*[ ]+","",address))
            house_address[i,1] <- address
        } else if(i==13 | i==22 | i==25 | i==107){
            address <- gsub(", MN$","",gsub("^.*Business: ","",address))
            house_address[i,1] <- address
        } else {
            no_info <- c(no_info,c(i,address))
        }
    } else {
        
        # clean up the address
        address <- gsub("^[\r\n]*[ ]+[\r\n]*[A-Za-z:]{5}[\r\n]*[ ]+","",address)
        address <- gsub("  "," ",gsub("[*]*[-]*[,]*","",address))
        # get the component parts of the address and put in the address data.frame
        if(i==113){
            house_address[i,1] <- gsub(" Education:.*$", "", address)# get city
        } else if(length(grep("P.O. Box", address))>0){
            house_address[i,1] <- gsub(" P.O. Box.*$", "", address)# get city
        } else {
            house_address[i,1] <- gsub(" [0-9]+.*$", "", address)# get city
        }
    }
    house_address[i,2] <- GetDistanceToCapitol(house_address[i,1])
}

# clean up the names
district <- sapply(house_names, function(x) gsub(" .*$", "", x))
name <- sapply(house_names, function(x) gsub("\\)$", "", gsub("^.* \\(", "", x)))

# create a single data.frame with all the info
member_info <- cbind(house_address,name,district)

# write the member info as a csv file for later use
write.csv(member_info,file="./data/mn_house_member_info_2014.csv")
