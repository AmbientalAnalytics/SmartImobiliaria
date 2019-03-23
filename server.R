## Cargando librerias
library(shiny)
library(leaflet)
library(tidyverse)

#devtools::install_github("hrbrmstr/nominatim")
library(nominatim)

# Shiny server
server <- function(input,output){
  
  output$mymap <- renderLeaflet({
    
    # Hay que agregar un try/catch caso direccion estes NULL
    dir <- osm_geocode(tolower(input$Direccion),#"pedernera, 2037 - posadas, ar",
                       limit=1,
                       key = Sys.getenv("consumer_key"),
                       country_codes = 'ar')
    # if null
    if (is.null(dir)){
      dir <- NULL
      dir$lon = -55.89492
      dir$lat = -27.35812
      dir$place_id = "Erro"
    }
    
    m <- leaflet() %>% #cambiar a un mapa base mas limpio
      addTiles() %>%
      addMarkers(
        lat = dir$lat, lng = dir$lon, popup = paste(dir$place_id)) %>% 
      setView(lng = dir$lon, lat = dir$lat, zoom = 20)
    m
  })
}