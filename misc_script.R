## Script for storing random peices of code
# creates a vector of MN House seat names (01A, 01B, etc)
fnMN_House_Labels <- function(){
    return(paste(
        # creates vector of house seat numbers (01, 01, 02, 02, etc)
        sprintf("%02d",rep(1:67,each=2)), 
        # creates vector of house seat letters (A, B, A, B)
        rep(c("A","B"),length=67), sep=""
    ))
}
# years of the results urls
mn_election_results_years=c(
    "2014",   
    "2010",
    "2008",
    "2006",
    "2004",
    "2002",
    "2000"
)


# function that takes a url and returns an xml document object
getParsedHTML <- function(url){
    html <- GET(url)
    content <- content(html,as="text")
    htmlParse(content, asText=TRUE)
}

## get the current house member names/party/district
# get the html from the page with house member page links
parsedHouseHtml <- getParsedHTML("http://www.house.leg.state.mn.us/members/hmem.asp")
# pull out the names/districts/parties of the members
house_names <- xpathSApply(parsedHouseHtml, "//b", xmlValue)[2:268][c(TRUE,FALSE)]
# get the individual values
district <- sapply(house_names, function(x) gsub("[A-Za-z '\\.\\)]+ \\(", "", gsub(", [A-Z]+\\)$", "", x)), USE.NAMES = FALSE)
party <- sapply(house_names, function(x) gsub(".*, ", "", gsub("\\)$", "", x)), USE.NAMES = FALSE)
name <- sapply(house_names, function(x) gsub(" \\([0-9AB]{3}, [A-Z]+\\)", "", x), USE.NAMES = FALSE)
# an extra clean up step to account for nicknames
name <- sapply(name, function(x) gsub(" \\([A-Za-z\\.]+\\)", "", x), USE.NAMES = FALSE)

# get rid of variables not longer needed
rm("district", "name", "party", "house_names", "legdata", "parsedHouseHtml")

## get the senate member names/party/district
# get the html from the page with senate member page links
parsedSenateHtml <- getParsedHTML("http://www.senate.leg.state.mn.us/members/")
# pull out the p tag with the relevant info
senate_names <- getNodeSet(parsedSenateHtml, "//p")[1:67]
# get the member name/party/district element
senate_names <- sapply(senate_names, FUN=function(x) xmlChildren(x)$a)
senate_names <- sapply(senate_names, FUN=function(x) xmlValue(x))
# get the individual values
district <- sapply(senate_names, function(x) gsub(", [A-Z]+\\)$", "", gsub("^.* \\(", "", x)), USE.NAMES = FALSE)
name <- sapply(senate_names, function(x) gsub(" \\([0-9]+, [A-Z]+\\)$", "", x), USE.NAMES = FALSE)
party <- sapply(senate_names, function(x) gsub("\\)$", "", gsub("^.* \\([0-9]+, ", "", x)), USE.NAMES = FALSE)

## load the data
# load the 2014 data
nResultsYearIndex <- 1
rdata <- read.csv(paste("./data/election_results/precinct_",mn_election_results_years[nResultsYearIndex],".csv", sep=""), header=TRUE, stringsAsFactors=FALSE)

## 2014 data cleaning

# column names, default import data type [desired data type], sample of data
# 1=num, 2-4=char, 5=factor, 6=char, 7-18=factor, 19-68=numeric
# $ VTDID     : num [char]  2.7e+08 2.7e+08 2.7e+08 2.7e+08 2.7e+08 ...
# $ PCTNAME   : chr  "Aitkin" "Aitkin Twp" "Ball Bluff Twp" "Balsam Twp" ...
# $ PCTCODE   : chr  "0005" "0010" "0015" "0020" ...
# $ MCDNAME   : chr  "Aitkin" "Aitkin Twp" "Ball Bluff Twp" "Balsam Twp" ...
# $ COUNTYNAME: chr [factor]  "Aitkin" "Aitkin" "Aitkin" "Aitkin" ...
# $ COUNTYCODE: chr  "1" "1" "1" "1" ...
# $ CONGDIST  : chr [factor] "8" "8" "8" "8" ...
# $ MNSENDIST : chr [factor] "10" "10" "10" "10" ...
# $ MNLEGDIST : chr [factor] "10B" "10B" "10B" "10B" ...
# $ CTYCOMDIST: chr [factor] "1" "1" "5" "5" ...
# $ JUDDIST   : chr [factor] "09" "09" "09" "09" ...
# $ SWCDIST   : chr [factor] "3001" "3001" "3001" "3001" ...
# $ WARD      : chr [factor] NA NA NA NA ...
# $ HOSPDIST  : chr [factor] NA NA NA NA ...
# $ PARKDIST  : chr [factor] NA NA NA NA ...
# $ TABSYSTEM : chr [factor] "Precinct Tabulator" "Precinct Tabulator" "Precinct Tabulator" "Central Count" ...
# $ TABMODEL  : chr [factor] "ES&S Model 100" "ES&S Model 100" "ES&S Model 100" "ES&S Model 100" ...
# $ MAILBALLOT: chr [factor] "NO" "NO" "NO" "YES" ...
# $ REG7AM    : num  1074 553 173 25 43 ...
# $ EDR       : num  44 25 12 0 1 6 0 2 24 4 ...
# $ SIGNATURES: num  624 378 126 0 0 63 0 17 495 143 ...
# $ AB_MB     : num  154 60 16 20 37 5 18 6 107 24 ...
# $ FEDONLYAB : num  0 0 0 0 0 0 0 0 0 0 ...
# $ TOTVOTING : num  778 438 142 20 37 68 18 23 602 167 ...
# $ USSENIP   : num  26 9 5 0 2 1 1 0 14 4 ...
# $ USSENR    : num  324 238 48 9 5 19 7 10 289 75 ...
# $ USSENDFL  : num  416 183 89 10 30 45 10 13 292 88 ...
# $ USSENLIB  : num  7 5 0 0 0 3 0 0 5 0 ...
# $ USSENWI   : num  0 0 0 0 0 0 0 0 0 0 ...
# $ USSENTOTAL: num  773 435 142 19 37 68 18 23 600 167 ...
# $ USREPIP   : num  0 0 0 0 0 0 0 0 0 0 ...
# $ USREPR    : num  357 257 54 6 11 24 4 13 310 78 ...
# $ USREPDFL  : num  371 161 76 10 24 41 11 10 273 85 ...
# $ USREPWI   : num  3 0 0 0 0 0 0 0 0 0 ...
# $ USREPTOTAL: num  765 433 142 17 37 68 17 23 597 167 ...
# $ MNLEGIP   : num  0 0 0 0 0 0 0 0 0 0 ...
# $ MNLEGR    : num  353 258 50 7 10 22 7 11 321 88 ...
# $ MNLEGDFL  : num  412 170 89 13 26 46 8 12 278 77 ...
# $ MNLEGWI   : num  2 1 0 0 0 0 2 0 0 0 ...
# $ MNLEGTOTAL: num  767 429 139 20 36 68 17 23 599 165 ...
# $ MNGOVIP   : num  21 14 2 0 2 1 0 0 19 7 ...
# $ MNGOVR    : num  336 253 53 8 5 19 7 10 304 87 ...
# $ MNGOVDFL  : num  384 162 82 12 27 46 9 12 269 68 ...
# $ MNGOVLIB  : num  3 0 0 0 0 2 2 0 5 0 ...
# $ MNGOVGLC  : num  21 5 4 0 3 0 0 1 3 4 ...
# $ MNGOVWI   : num  0 0 0 0 0 0 0 0 0 0 ...
# $ MNGOVTOTAL: num  765 434 141 20 37 68 18 23 600 166 ...
# $ MNSOSIP   : num  41 23 4 1 4 1 1 0 24 7 ...
# $ MNSOSR    : num  356 251 54 6 6 22 7 11 314 80 ...
# $ MNSOSDFL  : num  341 140 79 9 26 41 7 11 248 74 ...
# $ MNSOSLIB  : num  13 7 2 0 1 4 2 0 6 2 ...
# $ MNSOSWI   : num  0 0 0 0 0 0 0 0 0 0 ...
# $ MNSOSTOTAL: num  751 421 139 16 37 68 17 22 592 163 ...
# $ MNAUDIP   : num  29 19 4 0 3 1 1 0 19 4 ...
# $ MNAUDR    : num  277 202 52 6 5 17 7 11 239 64 ...
# $ MNAUDDFL  : num  415 189 79 9 25 46 8 11 325 90 ...
# $ MNAUDLIB  : num  12 4 1 0 0 2 2 0 3 2 ...
# $ MNAUDGLC  : num  18 10 5 0 3 1 0 0 6 5 ...
# $ MNAUDWI   : num  0 0 0 0 0 0 0 0 0 0 ...
# $ MNAUDTOTAL: num  751 424 141 15 36 67 18 22 592 165 ...
# $ MNAGIP    : num  16 12 2 0 3 0 0 1 7 1 ...
# $ MNAGR     : num  262 208 46 6 4 16 6 9 230 66 ...
# $ MNAGDFL   : num  426 184 87 12 26 47 8 12 338 91 ...
# $ MNAGGP    : num  5 2 0 0 0 0 0 0 2 1 ...
# $ MNAGLIB   : num  4 7 0 0 0 4 2 0 6 2 ...
# $ MNAGLMN   : num  35 9 6 0 3 1 2 0 8 5 ...
# $ MNAGWI    : num  0 0 0 0 0 0 0 0 0 0 ...
# $ MNAGTOTAL : num  748 422 141 18 36 68 18 22 591 166 .

## total number of precincts with no voters
#sum(rdata$TOTVOTING==0)

# remove precincts with no votes
cdata <- rdata[rdata$TOTVOTING>0,]

# set columns 6-18 to factor
cdata[6:18] <- tbl(lapply(cdata[6:18], as.factor))


fnGetDFLPrecentByOffice <- function(data, office="USSEN"){
    return(data[,grep(paste(office,"DFL", sep=""), colnames(data))]/
               (data[,grep(paste(office,"DFL", sep=""), colnames(data))]+data[,grep(paste(office,"R", sep=""), colnames(data))]))
}

# calculate dfl percentages and add those columns to data frame
# calculate DFL Senate % 
cdata[,"USSENDFLPER"] <- fnGetDFLPrecentByOffice(cdata,"USSEN")

# calculate DFL Gov % 
cdata[,"MNGOVDFLPER"] <- fnGetDFLPrecentByOffice(cdata,"MNGOV")

# calclulate DFL Leg % 
cdata[,"MNLEGDFLPER"] <- fnGetDFLPrecentByOffice(cdata,"MNLEG")


## get the names of the current reps
library(XML)
url <- "http://www.gis.leg.mn/php/house.php"
html <- htmlTreeParse(url, useInternalNodes=T)
leg_names <- readHTMLTable(html, header=TRUE)[[1]]

## get district demo info
leg_demo=data.frame()
for(i in leg_names$District){
    url <- paste("http://www.gis.leg.mn/php/profiles/house.php?district=", i, sep="")
    html <- htmlTreeParse(url, useInternalNodes=T)
    t <- readHTMLTable(html, header=TRUE)[[1]]
    leg_demo <- rbind(leg_demo,cbind(Label=rep(i,length(t)),t))
}

# coerce the data to the correct type
leg_demo[,3] <- as.integer(gsub(",", "", leg_demo[,3]))
levels(leg_demo[,4])[1] <- "100.0"
leg_demo[,4] <- as.numeric(levels(leg_demo[,4]))[leg_demo[,4]]


## random functions
fnGetVotesForOfficeByLD <- function(data, district="01A", office="USSENDFL"){
    return(sum(data[data$MNLEGDIST==district,grep(office, colnames(data))]))
}


## summerize data for mapping

library(dplyr)
ddata <- tbl_df(cdata)
county_data <- summarise(group_by(ddata, COUNTYNAME), 
                         govper=sum(MNGOVDFL)/(sum(MNGOVDFL)+sum(MNGOVR)), 
                         senper=sum(USSENDFL)/(sum(USSENDFL)+sum(USSENR)),
                         sosper=sum(MNSOSDFL)/(sum(MNSOSDFL)+sum(MNSOSR)),
                         audper=sum(MNAUDDFL)/(sum(MNAUDDFL)+sum(MNAUDR)),
                         agper=sum(MNAGDFL)/(sum(MNAGDFL)+sum(MNAGR)))

cong_data <- summarise(group_by(ddata, CONGDIST), 
                         govper=sum(MNGOVDFL)/(sum(MNGOVDFL)+sum(MNGOVR)), 
                         senper=sum(USSENDFL)/(sum(USSENDFL)+sum(USSENR)),
                         sosper=sum(MNSOSDFL)/(sum(MNSOSDFL)+sum(MNSOSR)),
                         audper=sum(MNAUDDFL)/(sum(MNAUDDFL)+sum(MNAUDR)),
                         agper=sum(MNAGDFL)/(sum(MNAGDFL)+sum(MNAGR)))

sen_data <- summarise(group_by(ddata, MNSENDIST), 
                       govper=sum(MNGOVDFL)/(sum(MNGOVDFL)+sum(MNGOVR)), 
                       senper=sum(USSENDFL)/(sum(USSENDFL)+sum(USSENR)),
                       sosper=sum(MNSOSDFL)/(sum(MNSOSDFL)+sum(MNSOSR)),
                       audper=sum(MNAUDDFL)/(sum(MNAUDDFL)+sum(MNAUDR)),
                       agper=sum(MNAGDFL)/(sum(MNAGDFL)+sum(MNAGR)))

leg_data <- summarise(group_by(ddata, MNLEGDIST), 
                       govper=sum(MNGOVDFL)/(sum(MNGOVDFL)+sum(MNGOVR)), 
                       senper=sum(USSENDFL)/(sum(USSENDFL)+sum(USSENR)),
                       sosper=sum(MNSOSDFL)/(sum(MNSOSDFL)+sum(MNSOSR)),
                       audper=sum(MNAUDDFL)/(sum(MNAUDDFL)+sum(MNAUDR)),
                       agper=sum(MNAGDFL)/(sum(MNAGDFL)+sum(MNAGR)))


source("map_functions.R")

## use map functions
county_title <- "2014 Minnesota Gubernatorial Results by County"
county_subtitle <- "The map on the left is the county map of Minnesota. The map on the right is a cartogram, where the size of the county reflects its population size rather than its geographic size."
ShadedDualPlot("county","county2010",county_data$govper,county_title,county_subtitle)

sen_title <- "2014 Minnesota Gubernatorial Results by Senate District"
sen_subtitle <- "The map on the left is the state senate map of Minnesota. The map on the right is a cartogram, where the size of each senate district reflects its population size rather than its geographic size."
ShadedDualPlot("senate","S2012",sen_data$govper,sen_title,sen_subtitle)

leg_title <- "2014 Minnesota Gubernatorial Results by House District"
leg_subtitle <- "The map on the left is the state house map of Minnesota. The map on the right is a cartogram, where the size of each house district reflects its population size rather than its geographic size."

ShadedDualPlot("house","L2012-1",leg_data$govper,leg_title,leg_subtitle)