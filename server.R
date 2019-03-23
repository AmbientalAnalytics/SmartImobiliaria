## Cargando librerias
library(shiny)
library(leaflet)
library(tidyverse)

# Shiny server
server <- function(input,output){
  
  output$mymap <- renderLeaflet({
    
    m <- leaflet(data = df) %>%
      addTiles() %>%
      setView(lng = -55.919765, lat = -27.380976, zoom = 13)
    m
  })
}