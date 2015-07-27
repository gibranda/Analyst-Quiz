library(shiny)
library(leaflet)
library(coda)
library(MASS)
library(emdbook)

source('helpers.R')


shinyUI(fluidPage(
    titlePanel("Top analyst treasure map"),
    sidebarLayout(
        sidebarPanel(
            p("The map on the right indicates the likely position of the analyst.
The expected position (marker) together with a probability region around it. You may select the probability coverage of this region using the slider below. You will find the analyst with the given probability. The higher the
likelihood to find the analyst, the larger the search area."),
            sliderInput("range", 
                        label = h6("Probability coverage (in %):"),
                        min = 1, max = 99, value = 20),
            em("The likelihood surface of the position of the analyst was simulated using an adaptive 
              Metropolis-Hastings sampler (50,000 iterations with discarded as 25,000 warm-up sample)."),
            p("Fork the ", a("code for this app", href = "https://github.com/pviefers/Analyst-Quiz"), " and the methodology behind at GitHub.")
        ),
        
        mainPanel(
            h3("Analyst position."),
            leafletOutput("map")
        )
    )
    )
)