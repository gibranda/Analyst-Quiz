## ---- Setup-and-estimation ----
list.of.packages <- c("coda",
                      "MASS",
                      "emdbook",
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
Please select the probability with which you wish to find the analyst. Note, the higher the
likelihood to find the analyst, the higher the search area."),
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