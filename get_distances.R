##########################################
## get distance from city to MN capitol ##
## GetDistanceToCapitol(city)           ##
##########################################

# load libraries
library(XML)
library(httr)

# receives a city name and/or state abbr and creates a gmap encoded string
createGMapAddress <- function(city="Minneapolis", state="MN"){
    encoded <- paste(city, state)
    gsub(" ","+",encoded)
}

if(!file.exists("./data/mn_capitol_distance_cache.csv")){
    # for caching distances
    cached <- data.frame(city=character(),distance=integer(), stringsAsFactors=FALSE)
} else {
    cached <- read.csv("./data/mn_capitol_distance_cache.csv", header=TRUE)
}

# from a city, gets the driving distance (in meters) from that address to the Minnesota Capitol
GetDistanceToCapitol <- function(city){
    # if the city is not in the cache
    if(nrow(cached[cached$city==city,][2])==0){
    
        # city of the capitol building
        capitol_address <- "St+Paul+MN"
        
        # url peices for assembling the api query url
        API_Key <- "&key=AIzaSyDOUkQEV9ZIRgOExItbzERjNYSslppcuX0"
        gmaps_url <- "http://maps.googleapis.com/maps/api/directions/json?"
        oparam="origin="
        dparam="&destination="
        
        # assemble the url
        url <- paste(gmaps_url,oparam,createGMapAddress(city),dparam,capitol_address,sep="")
        
        # get the data from the url
        payload_json <- GET(url)
        
        # get the content from the payload
        data_json <- content(payload_json)
        
        # if the json response is complete
        if(data_json$status == "OK"){
            
            # assign the distance to a var
            distance <- data_json$routes[[1]]$legs[[1]]$distance$value # meters in integer
            
            # check to see if the city is in the cache, if not put in cache
            if(nrow(cached[cached$city==city,][2])==0){
                
                # add the distance to the cache
                cached <- rbind(cached,data.frame(city=city,distance=distance))
            }
            
            # return the distance
            return(distance) 
            
        } else {
            
            # if the json status is not "ok" return NA
            return(NA)
        }
    # city is in the cache
    } else {
        
        # get distance from cache
        return(cached[cached$city==city,][2])
    }
}