## script for getting the home addresses of legislators

# load libraries
library(XML)
library(httr)

# url of page with house member page links
house_url <- "http://www.house.leg.state.mn.us/members/hmem.asp"
# get the page with the links
house_html <- GET(house_url)
# parse the links page
house_content <- content(house_html,as="text")
parsedHtml <- htmlParse(house_content, asText=TRUE)
# pull out the names/districts of the links and the links themselves
house_names <- xpathSApply(parsedHtml, "//option", xmlValue)[2:135]
house_links <- xpathSApply(parsedHtml, "//option", xmlAttrs)[2:135]

# create the data frame for storing the address info
house_address <- data.frame(
    city=character(),
    zip=character(),
    street=character(), 
    stringsAsFactors=FALSE)

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
    # clean up the address
    address <- gsub("^[\r\n]*[ ]+[\r\n]*[A-Za-z:]{5}[\r\n]*[ ]+","",address)
    address <- gsub("  "," ",gsub("[*]*[-]*[,]*","",address))
    # get the component parts of the address and put in the address data.frame
    house_address[i,1] <- gsub(" [0-9]+.*$", "", address)# get city
    house_address[i,2] <- gsub("^.* ", "", address) # get zip
    house_address[i,3] <- gsub(" [0-9]{5}$", "", gsub("^[A-Za-z]*\\. ?[A-Za-z]+ ", "", address)) # get street
}

# clean up the names
dist <- sapply(house_names, function(x) gsub(" .*$", "", x))
name <- sapply(house_names, function(x) gsub("\\)$", "", gsub("^.* \\(", "", x)))

# create a single data.frame with all the info
member_info <- cbind(house_address,name,dist)
