library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Expression that generates a histogram. The expression is
    # wrapped in a call to renderPlot to indicate that:
    #
    #  1) It is "reactive" and therefore should be automatically
    #     re-executed when inputs change
    #  2) Its output type is a plot
    
    output$distPlot <- renderPlot({
        x    <- read.csv(paste("./data/mn_",tolower(input$chamber),"_hpvi_2015.csv",sep=""))
        bins <- seq(from = -50, to = 50, by = as.numeric(input$width))
        
        x$hpvi_color <- as.factor((x$rpvi>0)*1)
        
        # draw the histogram with the specified number of bins
        #hist(x$rpvi, breaks = bins, col = 'darkgray', border = 'white')
        
        h_plot <- ggplot(data=x, aes(rpvi, fill=hpvi_color)) + 
            geom_histogram(breaks=bins) + 
            scale_fill_manual(values = c("red", "blue")) + 
            ylab("Number of Districts") + 
            xlab("hPVI") + 
            #ggtitle("Minnesota House District hPVI Distribution") +
            theme(legend.position = "none",
                  plot.title = element_text(size = rel(2)))
        
        if(input$districts) {
            #rug(x$rpvi)
            h_plot + geom_rug()
        } else {
            h_plot
        }
        
        
    })
    
})