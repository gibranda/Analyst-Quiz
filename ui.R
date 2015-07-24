## ---- Setup-and-estimation ----
list.of.packages <- c("ggmap",
                      "coda",
                      "MASS",
                      "emdbook",
                      "ggplot2",
                      "MHadaptive",
                      "geosphere"
)

# Auto-install if not available
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(ggmap)
library(coda)
library(MASS)
library(emdbook)
library(MHadaptive)
library(geosphere)

source('helpers.R')


shinyUI(fluidPage(
    titlePanel("Top analyst treasure map"),
    sidebarLayout(
        sidebarPanel(
            p("The map on the right indicates the likely position of the analyst.
              You may zoom in further (see health warning below) and also adjust the 
              appearance of the map and the probability coverage. 
              The density of the analyst was simulated using an adaptive 
              Metropolis-Hastings sampler."),
            sliderInput("coverage", 
                        label = h6("Probability coverage (in %):"),
                        min = 1, max = 99, value = 20),
            p("Here you can change the appearance of the underlying map."),
            selectInput("mapType", label = h6("Select maptype:"), 
                        choices = list("Roadmap" = "roadmap", 
                                       "Terrain" = "terrain", 
                                       "Satellite" = "satellite",
                                       "Hybrid" = "hybrid"), 
                        selected = "roadmap"),
            p("Health warning: probability regions are not invariant to zooming in. 
              ggplot discards points that fall outside the plot region and adjust the kernel density estimates. 
              Contours largely invariant at levels 13 or lower. For levels 14 or higher contours are thus omitted."),
            selectInput("zoom", 
                       label = h6("Zoom (larger = zoom in, smaller = zoom out):"), 
                       choices = list("10" = 10,
                                      "11" = 11,
                                      "12" = 12,
                                      "13" = 13,
                                      "14" = 14,
                                      "15" = 15,
                                      "16" = 16,
                                      "17" = 17), 
                       selected = 13) 
            ),
        
        mainPanel(
            h5("Posterior mean analyst position (red dot on map)."),
            verbatimTextOutput("guess"),
            plotOutput("map"),
            p("Marginal posteriors of latitude and longitude of the analyst's position."),
            plotOutput("hist"),
            p("Dashed vertical lines indicate corrdinate of nearest Zalando office (see also orange triangle in map).")
        )
    )
    )
)