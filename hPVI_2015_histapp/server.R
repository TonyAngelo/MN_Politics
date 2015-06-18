# required packages
library(shiny)
library(ggplot2)
library(gridExtra)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    # create the output plot
    output$distPlot <- renderPlot({
        # get the data for the selected chamber
        x <- read.csv(paste("./data/mn_",tolower(input$chamber),"_hpvi_2015.csv",sep=""))[,2:9]
        # add a color factor for coloring the graph
        x$hpvi_color <- as.factor((x$rpvi>0)*1)
        # get the bin breaks
        bins <- seq(from = -50, to = 50, by = as.numeric(input$width))
        
        # draw the histogram
        h_plot <- ggplot(data=x, aes(rpvi, fill=hpvi_color)) + 
            geom_histogram(breaks=bins) + 
            scale_fill_manual(values = c("red", "blue")) + 
            ylab("Number of Districts") + 
            xlab("hPVI") + 
#             ggtitle(paste("2015 Minnesota ",toupper(substring(input$chamber, 1,1)),substring(input$chamber, 2)," hPVI Distribution", sep="")) +
            theme(legend.position = "none",
                  plot.title = element_text(size = rel(2)))
        
        # if the show districts check box is selected add the rug
        if(input$districts) {
            h_plot + geom_rug()
        } else {
            h_plot
        }
    })
})