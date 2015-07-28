library(shiny)
library(dplyr)


shinyServer(function(input, output, session) {
    
    ## Interactive Map ###########################################
    ## 
    analystPos <- paste(sep = " ",
                        "Top Analyst: Expected Position <br/>",
                        "longitude=", round(guess[1],4),"<br/>",
                        "latitude=", round(guess[2],4)
    )
    
    # Reactive expression for the probability coverage that underlies the
    # polygon drawn on map
    coverage <- reactive({
        prob.cov <- input$range / 100
        ContourLines <- HPDregionplot(mcmc(data.matrix(mhall$trace), thin = 10), prob=prob.cov)
        leafCont <- as.data.frame(ContourLines)[, 2:3]
        cov <- data.matrix(leafCont)
        cov
    })
    
    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        leaflet() %>%
            addTiles() %>%
            setView(guess$lon, guess$lat, zoom = 12) %>%
            addMarkers(data=guess, popup=analystPos)
    })
    
    observe({
        proxy <- leafletProxy("map", data = guess)
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% 
            clearShapes() %>% 
            addPolygons(data = coverage())
    })

}
)