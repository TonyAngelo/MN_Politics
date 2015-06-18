# load libraries
library(dplyr)

# election results years
mn_election_results_years=c(
    "2014",   
    "2010",
    "2008",
    "2006",
    "2004",
    "2002",
    "2000"
)

# function for getting the percentage of the two-party vote for each 2014 race
get2014PercentagesByBoundry <- function(data){
    data <- summarise(data, 
              ussenper=sum(USSENDFL)/(sum(USSENDFL)+sum(USSENR)),
              uslegper=sum(USREPDFL)/(sum(USREPDFL)+sum(USREPR)),
              govper=sum(MNGOVDFL)/(sum(MNGOVDFL)+sum(MNGOVR)), 
              sosper=sum(MNSOSDFL)/(sum(MNSOSDFL)+sum(MNSOSR)),
              audper=sum(MNAUDDFL)/(sum(MNAUDDFL)+sum(MNAUDR)),
              agper=sum(MNAGDFL)/(sum(MNAGDFL)+sum(MNAGR)),
              mnlegper=sum(MNLEGDFL)/(sum(MNLEGDFL)+sum(MNLEGR)))
    names(data)[1] <- "district"
    data
}

## load the data for 2014
nResultsYearIndex <- 1
rdata <- read.csv(paste("./data/election_results/precinct_",mn_election_results_years[nResultsYearIndex],".csv", sep=""), header=TRUE, stringsAsFactors=FALSE)

# remove precincts with no votes
rdata <- rdata[rdata$TOTVOTING>0,]

# format the county, sen and leg dist column, add a leading zero to single digit districts
rdata$MNLEGDIST <- sapply(rdata$MNLEGDIST, function(x){if(nchar(x)<3){paste("0",x,sep="")} else {x}})
rdata$MNSENDIST <- sapply(rdata$MNSENDIST, function(x){if(nchar(x)<2){paste("0",x,sep="")} else {x}})
rdata$COUNTYCODE <- sapply(rdata$COUNTYCODE, function(x){if(nchar(x)<2){paste("0",x,sep="")} else {x}})

# set columns 6-18 to factor
factor_names <- names(rdata)[6:18]
rdata[,factor_names] <- lapply( rdata[,factor_names] , factor)

# get the data sets
county_data <- get2014PercentagesByBoundry(group_by(rdata, COUNTYCODE))
cong_data <- get2014PercentagesByBoundry(group_by(rdata, CONGDIST))
sen_data <- get2014PercentagesByBoundry(group_by(rdata, MNSENDIST))
leg_data <- get2014PercentagesByBoundry(group_by(rdata, MNLEGDIST))

## load the mapping functions
source("map_functions.R")

## create dual county map
county_title <- "2014 Minnesota Gubernatorial Results by County"
county_subtitle <- "The map on the left is the county map of Minnesota. The map on the right is a cartogram, where the size reflects its population size rather than its geographic size."
mn.county.df <- prepareDataForMap(readOGR("./data/mn_shape_files/county", layer = "county2010"), county_data, county=TRUE)
cart.county.df <- prepareDataForMap(readOGR("./data/mn_shape_files/county", layer = "county2010_cart"), county_data, county=TRUE)
makeDualPlot(getShadedCountyMap(mn.county.df),getShadedCountyMap(cart.county.df),county_title,county_subtitle)

## create dual congressional district map
cong_title <- "2014 Minnesota Gubernatorial Results by Congressional District"
cong_subtitle <- "The map on the left is the congressional district map of Minnesota. The map on the right is a cartogram, where the size reflects its population size rather than its geographic size."
mn.cong.df <- prepareDataForMap(readOGR("./data/mn_shape_files/congress", layer = "C2012"), cong_data)
cart.cong.df <- prepareDataForMap(readOGR("./data/mn_shape_files/congress", layer = "C2012_cart"), cong_data)
makeDualPlot(getShadedDistrictMap(mn.cong.df),getShadedDistrictMap(cart.cong.df),cong_title,cong_subtitle)

## create dual seate district map
sen_title <- "2014 Minnesota Gubernatorial Results by Senate District"
sen_subtitle <- "The map on the left is the state senate map of Minnesota. The map on the right is a cartogram, where the size reflects its population size rather than its geographic size."
mn.sen.df <- prepareDataForMap(readOGR("./data/mn_shape_files/senate", layer = "S2012"), sen_data)
cart.sen.df <- prepareDataForMap(readOGR("./data/mn_shape_files/senate", layer = "S2012_cart"), sen_data)
makeDualPlot(getShadedDistrictMap(mn.sen.df),getShadedDistrictMap(cart.sen.df),sen_title,sen_subtitle)

## create dual house district map
leg_title <- "2014 Minnesota Gubernatorial Results by House District"
leg_subtitle <- "The map on the left is the state house map of Minnesota. The map on the right is a cartogram, where the size reflects its population size rather than its geographic size."
mn.leg.df <- prepareDataForMap(readOGR("./data/mn_shape_files/house", layer = "L2012-1"), leg_data)
cart.leg.df <- prepareDataForMap(readOGR("./data/mn_shape_files/house", layer = "L2012-1_cart"), leg_data)
makeDualPlot(getShadedDistrictMap(mn.leg.df),getShadedDistrictMap(cart.leg.df),leg_title,leg_subtitle)
