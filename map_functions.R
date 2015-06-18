# lload libraries
library(rgdal)
library(rgeos)
library(ggplot2)
library(gridExtra)
library(mapproj)

# function for adding the standard dual map plotting theme elements
getStandardTheme <- function(){
    theme(
        #plot.title = element_text(size=24, face="bold"),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
}

# returns a shaded district map element
getShadedDistrictMap <- function(x, shade=1){
    ggplot(x, aes(long,lat,group=DISTRICT, fill=govper)) + 
        geom_polygon() +
        geom_path(color="white") +
        coord_equal() +
        getShadeTheme(shade) +
        getStandardTheme()
}

# returns a shaded county map element
getShadedCountyMap <- function(x, shade=1){
    ggplot(x, aes(long,lat,group=COUNTY, fill=govper)) + 
        geom_polygon() +
        geom_path(color="white") +
        coord_equal() +
        getShadeTheme(shade) +
        getStandardTheme()
}

getShadeTheme <- function(x=1){
    if(x==1){
        # red to white to blue
        scale_fill_gradientn(colours = c("red","white","blue"), limits=c(0,1), space="rgb")
    } else if(x==2){
        # red to blue through purple
        scale_fill_continuous(low="red", high="blue", limits=c(0,1), space="rgb")
    } else if(x==3){
        # white to blue (dfl density)
        scale_fill_continuous(low="white", high="blue", limits=c(0,1), space="rgb")
    } else if(x==4){
        # red to white (rep density)
        scale_fill_continuous(low="red", high="white", limits=c(0,1), space="rgb")
    }
}

# returns a shaded district map element with district names
getNumericDistrictMap <- function(x, textSize=6, bounds=c(0,1)){
    cnames <- aggregate(cbind(long, lat, govper) ~ DISTRICT, data=x, 
                        FUN=function(y)mean(range(y)))
    
    ggplot(x, aes(long,lat)) +  
        geom_polygon(data=x, aes(group=DISTRICT, fill=govper), colour='black') +
        geom_text(data=cnames, aes(long, lat, label = DISTRICT), size=textSize) +
        coord_equal() +
        scale_fill_continuous(low="red", high="blue", limits=bounds, space="rgb") +
        getStandardTheme()
}

# returns a shaded county map element with county names
getNumericCountyMap <- function(x, textSize=6, bounds=c(0,1)){
    cnames <- aggregate(cbind(long, lat, NAME) ~ COUNTY, data=x, 
                        FUN=function(y)mean(range(y)))
    
    ggplot(x, aes(long,lat)) +  
        geom_polygon(data=x, aes(group=COUNTY, fill=govper), colour='black') +
        geom_text(data=cnames, aes(long, lat, label = NAME), size=textSize) +
        coord_equal() +
        scale_fill_continuous(low="red", high="blue", limits=bounds, space="rgb") +
        getStandardTheme()
}

# combines map and data for shaded mapping
prepareDataForMap <- function(map, data, county=FALSE){
    df <- fortify(map)
    df <- merge(df,map@data,by.x="id",by.y="row.names")
    if(county==TRUE){
        df$COUNTY <- as.numeric(df$COUNTY)
        data$district <- as.numeric(data$district)
        merge(df,data, by.x="COUNTY", by.y="district", all=TRUE)
    } else {
        merge(df,data, by.x="DISTRICT", by.y="district", all=TRUE)
    }
}

# creates a dual plot element and/or png
makeDualPlot <- function(x, y, title="Map and Cartogram", subtitle="", makePNG=FALSE){
    if(makePNG){
        png(file="mn_maps.png",width=1000,height=500,units="px")
    }
    grid.arrange(x, y, ncol=2, 
                 main=textGrob(title, gp=gpar(cex=2.5), just="top"),
                 sub=textGrob(subtitle, gp=gpar(cex=1), vjust=-1))
    if(makePNG){
        dev.off(which = dev.cur())
    }
}


### misc stuff

## plot maps
#library(maps)
#library(maptools)

#rb.col <- colorRamp(c("#FF0000","#0000FF"))
#getHEX <- function(x){
#    rgb(rb.col(x)[1],rb.col(x)[2],rb.col(x)[3], maxColorValue=255)
#}

#d$color <- sapply(d$dflper,getHEX)
#mn <- map("county", "minnesota", col=d$color, fill=TRUE)