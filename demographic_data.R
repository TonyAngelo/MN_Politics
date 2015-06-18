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