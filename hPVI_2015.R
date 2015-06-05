## script for calculating the 2015 hPVI

# load the libraries
require(dplyr)
require(XML)
require(httr)

#####################################
######### calculate hPVIs ###########
#####################################

# load the 2014 data
rdata2014 <- read.csv("./data/precinct_2014.csv", header=TRUE, stringsAsFactors=FALSE)
rdata2012 <- read.csv("./data/precinct_2012.csv", header=TRUE, stringsAsFactors=FALSE)

# 2012 data contains a totals row at the bottom
rdata2012 <- rdata2012[1:nrow(rdata2012)-1,]

# functions for adding a leading zero to districts without one
formatLegDist <- function(x){
    if(nchar(x)<3){
        paste("0",x,sep="")
    } else {
        x
    }
}

formatSenDist <- function(x){
    if(nchar(x)<2){
        paste("0",x,sep="")
    } else {
        x
    }
}

# format the sen and leg dist column, add a leading zero to single digit districts
rdata2014$MNLEGDIST <- sapply(rdata2014$MNLEGDIST, formatLegDist)
rdata2012$MNLEGDIST <- sapply(rdata2012$MNLEGDIST, formatLegDist)
rdata2014$MNSENDIST <- sapply(rdata2014$MNSENDIST, formatSenDist)
rdata2012$MNSENDIST <- sapply(rdata2012$MNSENDIST, formatSenDist)

# convert the countycode-mailballot columns to factors
rdata2014[6:18] <- lapply(rdata2014[6:18], as.factor)
rdata2012[5:17] <- lapply(rdata2012[5:17], as.factor)

# convert to a dplyr tbl
cdata2014 <- tbl_df(rdata2014)
cdata2012 <- tbl_df(rdata2012)

# get the gov % by sen district
sendata2014 <- summarise(group_by(cdata2014, MNSENDIST), 
                        gov=sum(MNGOVDFL)/(sum(MNGOVDFL)+sum(MNGOVR)))

# get the pres % by sen district
sendata2012 <- summarise(group_by(cdata2012, MNSENDIST),  
                        pres=sum(USPRSDFL)/(sum(USPRSDFL)+sum(USPRSR)))

# get the gov % by house district
legdata2014 <- summarise(group_by(cdata2014, MNLEGDIST), 
                      gov=sum(MNGOVDFL)/(sum(MNGOVDFL)+sum(MNGOVR)))

# get the pres % by house district
legdata2012 <- summarise(group_by(cdata2012, MNLEGDIST),  
                         pres=sum(USPRSDFL)/(sum(USPRSDFL)+sum(USPRSR)))

# merge the two years of data
sendata <- merge(sendata2014, sendata2012, by.x = "MNSENDIST", by.y = "MNSENDIST", all=TRUE)
legdata <- merge(legdata2014, legdata2012, by.x = "MNLEGDIST", by.y = "MNLEGDIST", all=TRUE)

# get rid of variables used to create data
rm("rdata2014","rdata2012", "cdata2014","cdata2012", "sendata2014", "sendata2012", "legdata2014", "legdata2012")

# rename the district column
names(sendata)[1] <- "district"
names(legdata)[1] <- "district"

# reorder the data
sendata <- arrange(sendata, district)
legdata <- arrange(legdata, district)

# get the ave between the pres and gov
sendata <- mutate(sendata,ave=(pres+gov)/2)
legdata <- mutate(legdata,ave=(pres+gov)/2)

# calculate the raw pvi number
sendata <- mutate(sendata,rpvi=(ave-.5)*100)
legdata <- mutate(legdata,rpvi=(ave-.5)*100)

# format the raw pvi into the standard pvi format
sendata <- mutate(sendata,hpvi=ifelse(round(rpvi,digits=0)>0, 
                                      paste("D+", as.character(round(rpvi,digits=0)) , sep=""), 
                                      ifelse(round(rpvi,digits=0)<0, 
                                             paste("R+", as.character(abs(round(rpvi,digits=0))), sep=""), 
                                             "EVEN")))

legdata <- mutate(legdata,hpvi=ifelse(round(rpvi,digits=0)>0, 
                                      paste("D+", as.character(round(rpvi,digits=0)) , sep=""), 
                                      ifelse(round(rpvi,digits=0)<0, 
                                             paste("R+", as.character(abs(round(rpvi,digits=0))), sep=""), 
                                             "EVEN")))

#######################################
## get member names, party, district ##
#######################################

# function that takes a url and returns an xml document object
getParsedHTML <- function(url){
    html <- GET(url)
    content <- content(html,as="text")
    htmlParse(content, asText=TRUE)
}

## get the house member names/party/district
# url of page with house member page links
house_url <- "http://www.house.leg.state.mn.us/members/hmem.asp"
# get the html from the page
parsedHouseHtml <- getParsedHTML(house_url)
# pull out the names/districts/parties of the members
house_names <- xpathSApply(parsedHouseHtml, "//b", xmlValue)[2:268][c(TRUE,FALSE)]
# get the individual values
district <- sapply(house_names, function(x) gsub("[A-Za-z '\\.\\)]+ \\(", "", gsub(", [A-Z]+\\)$", "", x)), USE.NAMES = FALSE)
party <- sapply(house_names, function(x) gsub(".*, ", "", gsub("\\)$", "", x)), USE.NAMES = FALSE)
name <- sapply(house_names, function(x) gsub(" \\([0-9AB]{3}, [A-Z]+\\)", "", x), USE.NAMES = FALSE)
# an extra clean up step to account for nicknames
name <- sapply(name, function(x) gsub("\\([A-Za-z]+\\)", "", x), USE.NAMES = FALSE)
# create a dplyr tbl
house_member_info <- tbl_df(as.data.frame(cbind(name,party,district)))

# merge the house member info tbl with the pvi tbl
houseHPVI <- tbl_df(merge(legdata, house_member_info, by.x = "district", by.y = "district", all=TRUE))

# get rid of variables not longer needed
rm("house_member_info", "district", "name", "party", "house_names", "legdata", "house_url", "parsedHouseHtml")

## get the senate member names/party/district
# url of page with senate member page links
senate_url <- "http://www.senate.leg.state.mn.us/members/"
# get the html from the page
parsedSenateHtml <- getParsedHTML(senate_url)
# pull out the p tag with the relevant info
senate_names <- getNodeSet(parsedSenateHtml, "//p")[1:67]
# get the member name/party/district element
senate_names <- sapply(senate_names, FUN=function(x) xmlChildren(x)$a)
senate_names <- sapply(senate_names, FUN=function(x) xmlValue(x))
# get the individual values
district <- sapply(senate_names, function(x) gsub(", [A-Z]+\\)$", "", gsub("^.* \\(", "", x)), USE.NAMES = FALSE)
name <- sapply(senate_names, function(x) gsub(" \\([0-9]+, [A-Z]+\\)$", "", x), USE.NAMES = FALSE)
party <- sapply(senate_names, function(x) gsub("\\)$", "", gsub("^.* \\([0-9]+, ", "", x)), USE.NAMES = FALSE)
# bind the data into a single matrix
senate_member_info <- tbl_df(as.data.frame(cbind(name,party,district)))

# merge the senate member info tbl with the pvi tbl
senateHPVI <- tbl_df(merge(sendata, senate_member_info, by.x = "district", by.y = "district", all=TRUE))

# get rid of variables not longer needed
rm("senate_member_info", "district", "name", "party", "senate_names", "sendata", "senate_url", "parsedSenateHtml")

# reorder columns of data.frames
senateHPVI <- senateHPVI[,c(1,7,8,6,5,4,2,3)]
houseHPVI <- houseHPVI[,c(1,7,8,6,5,4,2,3)]

#####################################
### output data, tables and plots ###
#####################################

# create output dir if none exists
if(!file.exists("./output")){
    dir.create("output")
}

# write to csv
write.csv(senateHPVI,file="./output/mn_senate_hpvi_2015.csv")
write.csv(houseHPVI,file="./output/mn_house_hpvi_2015.csv")

# write full html table output to file
write(print(xtable(senateHPVI, caption="Minnesota Senate Districts hPVI"), 
            caption.placement='top', type="html", include.rownames=FALSE), "./output/senate_hpvi_table_2015.html")
write(print(xtable(houseHPVI, caption="Minnesota House Districts hPVI"), 
            caption.placement='top', type="html", include.rownames=FALSE), "./output/house_hpvi_table_2015.html")

# write ten most conservative and liberal districts html table output to file
write(print(xtable(arrange(senateHPVI,rpvi)[1:10,1:4], caption="10 Most Republican Minnesota Senate Districts by hPVI"), 
            caption.placement='top', type="html", include.rownames=FALSE), "./output/senate_hpvi_most_rep_table_2015.html")
write(print(xtable(arrange(houseHPVI,rpvi)[1:10,1:4], caption="10 Most Republican Minnesota House Districts by hPVI"), 
            caption.placement='top', type="html", include.rownames=FALSE), "./output/house_hpvi_most_rep_table_2015.html")
write(print(xtable(arrange(senateHPVI,desc(rpvi))[1:10,1:4], caption="10 Most DFL Minnesota Senate Districts by hPVI"), 
            caption.placement='top', type="html", include.rownames=FALSE), "./output/senate_hpvi_most_dfl_table_2015.html")
write(print(xtable(arrange(houseHPVI,desc(rpvi))[1:10,1:4], caption="10 Most DFL Minnesota House Districts by hPVI"), 
            caption.placement='top', type="html", include.rownames=FALSE), "./output/house_hpvi_most_dfl_table_2015.html")

# write the twenty most competitive districts html table output to file
write(print(xtable(arrange(senateHPVI,abs(rpvi))[1:20,1:4], caption="20 Most Competitive Minnesota Senate Districts by hPVI"), 
            caption.placement='top', type="html", include.rownames=FALSE), "./output/senate_hpvi_most_comp_table_2015.html")
write(print(xtable(arrange(houseHPVI,abs(rpvi))[1:20,1:4], caption="20 Most Competitive Minnesota House Districts by hPVI"), 
            caption.placement='top', type="html", include.rownames=FALSE), "./output/house_hpvi_most_comp_table_2015.html")


# histogram plots

# add a factor variable for coloring hist bars
houseHPVI$hpvi_color <- as.factor((houseHPVI[,5]>0)*1)
senateHPVI$hpvi_color <- as.factor((senateHPVI[,5]>0)*1)

# create the pngs
png(file="./output/house_hpvi_hist_2015.png",width=500,height=500,units="px")
ggplot(data=houseHPVI, aes(rpvi, fill=hpvi_color)) + 
    geom_histogram(breaks=c(-50,-40,-30,-20,-10,0,10,20,30,40,50)) + 
    scale_fill_manual(values = c("red", "blue")) + 
    ylab("Number of Districts") + 
    xlab("hPVI") + 
    ggtitle("Minnesota House District hPVI Distribution") +
    theme(legend.position = "none",
          plot.title = element_text(size = rel(2)))
dev.off(which = dev.cur())

png(file="./output/senate_hpvi_hist_2015.png",width=500,height=500,units="px")
ggplot(data=senateHPVI, aes(rpvi, fill=hpvi_color)) + 
    geom_histogram(breaks=c(-50,-40,-30,-20,-10,0,10,20,30,40,50)) + 
    scale_fill_manual(values = c("red", "blue")) + 
    ylab("Number of Districts") + 
    xlab("hPVI") + 
    ggtitle("Minnesota Senate District hPVI Distribution") +
    theme(legend.position = "none",
          plot.title = element_text(size = rel(2)))
dev.off(which = dev.cur())

# district bar graph
ggplot(data=senateHPVI, aes(x=district, y=rpvi, fill=party)) + geom_bar(colour="black", stat="identity")

