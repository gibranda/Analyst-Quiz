library(shiny)
library(gridExtra)
library(scales)
library(dplyr)

shinyServer(function(input, output, session) {
    
    ## Interactive Map ###########################################
    ## 
    # Get some highest posterior regions to indicate on the map
    output$guess <- renderPrint({ guess })
    output$zoom <- renderPrint({ input$zoom })
    
    
        # Create the map
    output$map <- renderPlot({
        ContourLines <- as.data.frame(HPDregionplot(mcmc(data.matrix(mhall$trace)), prob=input$coverage/100))
        
        #Berlin <- get_stamenmap(bbox = c(left = 13.36, bottom = 52.45, right = 13.52, top = 52.55), maptype = "toner", zoom = 13)
        Berlin <-  get_googlemap(center = c(lon = 13.45705, lat=52.51122), 
                                 zoom = as.numeric(input$zoom), 
                                 maptype = input$mapType, 
                                 size = c(780, 640))
        BerlinMap <- ggmap(Berlin)
        #BerlinMap <- qmap("berlin", zoom = 13)
        if(input$zoom < 14){
            BerlinMap + 
                geom_polygon(data = ContourLines, aes(x = x, y = y), color = "red", size = 2, fill = "red", alpha = 0.2) +
                stat_density2d(
                    aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                    size = 1, bins = 9, data = scatter_all,
                    geom = "density2d", color = "black"
                ) +
                geom_point(aes(x = guess$lon, y = guess$lat), size = 4, color = "red")+
                theme(legend.position="none")
        } else {
            BerlinMap + 
                geom_point(aes(x = guess$lon, y = guess$lat), size = 4, color = "red")+
                theme(legend.position="none")
        }
       
            
    })
    output$hist <- renderPlot({
    par(mfcol = c(1,2))
       plot(density(mhall$trace[,1]), main = "latitude"); abline(v=13.471004300000004, lty=2, col ="grey")
       plot(density(mhall$trace[,2]), main = "longitude"); abline(v=52.5066796, lty=2, col ="grey")
    })
}
)