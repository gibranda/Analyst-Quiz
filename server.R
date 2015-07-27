library(shiny)
library(gridExtra)
library(scales)
library(dplyr)

shinyServer(function(input, output, session) {
    
    ## Interactive Map ###########################################
    ## 
    # Get some highest posterior regions to indicate on the map
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
    
    
    
    # In a more elaborate application I'd separate the observer and the observed events
    # and the rendering of the map. Now the map is re-rendered every time the input
    # changes, which is not handy. However, using leafletProxy() throws an error and
    # the response time without proxy is still fair enough.
    observe({
        output$map <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                addMarkers(data=guess, popup=analystPos) %>% 
                addPolygons(data = coverage())
        })
    })
    
    output$hist <- renderPlot({
        par(mfcol = c(1,2))
        plot(density(mhall$trace[,1]), main = "latitude"); abline(v=13.471004300000004, lty=2, col ="grey")
        plot(density(mhall$trace[,2]), main = "longitude"); abline(v=52.5066796, lty=2, col ="grey")
    })
}
)