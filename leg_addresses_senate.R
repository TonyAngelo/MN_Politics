## script for getting the home addresses of legislators

# load libraries
library(XML)
library(httr)

# include the distance functions
source("get_distances.R")

# url of page with house member page links
senate_url <- "http://www.senate.leg.state.mn.us/members/"
# get the page with the links
senate_html <- GET(senate_url)
# parse the links page
senate_content <- content(senate_html,as="text")
parsedSenateHtml <- htmlParse(senate_content, asText=TRUE)
# pull out the p tag with the relevant info
senate_info <- getNodeSet(parsedSenateHtml, "//p")[1:67]
# get the link to the member page from the bio p
senate_link_tag <- sapply(senate_info, FUN=function(x) xmlChildren(x)[[2]])
senate_links <- sapply(senate_link_tag, FUN=function(x) xmlAttrs(x))
senate_names <- sapply(senate_link_tag, FUN=function(x) xmlValue(x))
# the base senate url
senate_base_url <- "http://www.senate.leg.state.mn.us"

# create the data frame for storing the address info
sen_member_info <- data.frame(
    bio=character(),
    city=character(),
    distanceToCapitol=integer(),
    stringsAsFactors=FALSE)

# loop over the member page links
for(i in 1:67){
    # get the member page html
    mem_html <- GET(paste(senate_base_url,senate_links[[i]],sep=""))
    # parse the member page html
    mem_content <- content(mem_html,as="text")
    parsedMemHtml <- htmlParse(mem_content, asText=TRUE)
    # get the bio div
    mem_bio <- xpathSApply(parsedMemHtml, "//div[@id='bio']", xmlValue)
    # clean up the bio info
    mem_bio <- gsub("[\n]?[\r]?[\t]?","",mem_bio)
    
    if(length(mem_bio)>0){
        sen_member_info[i,1] <- mem_bio
        
        # get the city
        sen_member_info[i,2] <- strsplit(mem_bio,"[[:space:]]")[[1]][2]
        sen_member_info[i,3] <- GetDistanceToCapitol(sen_member_info[i,2])
        
        # assign the info to the data.frame
        #sen_member_info[i,1] <- mem_bio
        #sen_member_info[i,2] <- city
        #sen_member_info[i,3] <- GetDistanceToCapitol(sen_member_info[i,2])
    }
}

# clean up the names
sen_district <- sapply(senate_names, function(x) gsub(", [A-Z]+\\)$", "", gsub("^.* \\(", "", x)))
sen_name <- sapply(senate_names, function(x) gsub(" \\([0-9]+, [A-Z]+\\)$", "", x))
sen_party <- sapply(senate_names, function(x) gsub("\\)$", "", gsub("^.* \\([0-9]+, ", "", x)))

# create a single data.frame with all the info
sen_member_info <- cbind(sen_member_info,sen_name,sen_party,sen_district)

# write the member info as a csv file for later use
write.csv(sen_member_info,file="./data/mn_senate_member_info_2014.csv")
