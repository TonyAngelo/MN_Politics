## script for calculating the 2015 hPVI

# load the libraries
library(dplyr)

# load the 2014 data
rdata2014 <- read.csv("./data/precinct_2014.csv", header=TRUE, stringsAsFactors=FALSE)
rdata2012 <- read.csv("./data/precinct_2012.csv", header=TRUE, stringsAsFactors=FALSE)

# 2012 data contains a totals row at the bottom
rdata2012 <- rdata2012[1:nrow(rdata2012)-1,]

# function for adding a leading zero to leg districts without one
formatLegDist <- function(x){
    if(nchar(x)<3){
        paste("0",x,sep="")
    } else {
        x
    }
}

# format the leg dist column, add a leading zero to single digit districts
rdata2014$MNLEGDIST <- sapply(rdata2014$MNLEGDIST, formatLegDist)
rdata2012$MNLEGDIST <- sapply(rdata2012$MNLEGDIST, formatLegDist)

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

# merge the senate and house data
sendata <- merge(sendata2014, sendata2012, by.x = "MNSENDIST", by.y = "MNSENDIST", all=TRUE)
legdata <- merge(legdata2014, legdata2012, by.x = "MNLEGDIST", by.y = "MNLEGDIST", all=TRUE)

# get rid of variables used to create data
#rm("rdata2014","rdata2012","cdata2014","cdata2012", "sendata2014", "sendata2012", "legdata2014", "legdata2012")

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

######### data is ready to go, write to file ###########
write.csv(sendata,file="./data/mn_senate_hpvi_2014.csv")
write.csv(legdata,file="./data/mn_house_hpvi_2014.csv")
########################################################


hist(legdata$rpvi, breaks=c(-50,-40,-30,-20,-10,0,10,20,30,40,50))
hist(sendata$rpvi, breaks=c(-50,-40,-30,-20,-10,0,10,20,30,40,50))




                                      




