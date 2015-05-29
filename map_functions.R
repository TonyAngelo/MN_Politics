library(plyr); library(dplyr)
library(rgdal)
library(rgeos)
library(ggplot2)
library(gridExtra)
library(mapproj)

getShadedMap <- function(x, bounds=c(0,1)){
    ggplot(x, aes(long,lat,group=group, fill=margin)) + 
        geom_polygon() +
        geom_path(color="white") +
        coord_equal() +
        scale_fill_continuous(low="red", high="blue", limits=bounds, space="rgb") +
        #ggtitle("Cartogram of Minnesota Counties") + 
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


getNumericMap <- function(x, textSize=6){
    cnames <- aggregate(cbind(long, lat) ~ DISTRICT, data=x, 
                        FUN=function(y)mean(range(y)))
    
    ggplot(x, aes(long,lat)) +  
        geom_polygon(aes(group=group), colour='black', fill=NA) +
        geom_text(data=cnames, aes(long, lat, label = DISTRICT), size=textSize) +
        coord_equal() +
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


prepareDataForShadedMap <- function(x, data){
    x <- fortify(x)
    x$margin <- sapply(x$group,FUN=function(y) data[y])
    return(x)
}

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

## put it all together
ShadedDualPlot <- function(type="county", layer="county2010", shadeData, title="Title", subtitle="Subtitle"){
    plot <- readOGR(paste("./data/mn_shape_files/",type,sep=""), layer = layer)
    cart <- readOGR(paste("./data/mn_shape_files/",type,sep=""), layer = paste(layer,"_cart",sep=""))
    makeDualPlot(getShadedMap(prepareDataForShadedMap(plot,shadeData)), 
                 getShadedMap(prepareDataForShadedMap(cart,shadeData)), 
                 title, subtitle)
}

mn.sen <- readOGR("./data/mn_shape_files/senate", layer = "S2012")
mn.sen@data$id <- rownames(mn.sen@data)
#mn.sen@data   <- join(mn.sen@data, data, by="DISTRICT")
mn.sen.df     <- fortify(mn.sen)
mn.sen.df     <- join(mn.sen.df,mn.sen@data, by="id")
mn.sen.plot <- getNumericMap(mn.sen.df)

cart.sen <- readOGR("./data/mn_shape_files/senate", layer = "S2012_cart")
cart.sen@data$id <- rownames(cart.sen@data)
#cart.sen@data   <- join(cart.sen@data, data, by="DISTRICT")
cart.sen.df     <- fortify(cart.sen)
cart.sen.df     <- join(cart.sen.df,cart.sen@data, by="id")
cart.sen.plot <- getNumericMap(cart.sen.df)


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