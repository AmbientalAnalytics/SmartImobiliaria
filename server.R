## Cargando librerias
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)

devtools::install_github("hrbrmstr/nominatim")
library(nominatim)


# Shiny server
server <- function(input,output){
  #Building index
  # distance from school
  escuela <- read_sf("./Datos/DatosEspaciais.gpkg", "Escuelas")
  competencia <- read_sf("./Datos/DatosEspaciais.gpkg", "Restaurantes")
  # Hay que agregar un try/catch caso direccion estes NULL
  boton_reactivo <- eventReactive(input$ActualizarIndicadores, {
    dir <- osm_geocode(tolower(input$Direccion),
                     #"pedernera, 2037 - posadas, ar",
                     limit=1,
                     key = Sys.getenv("consumer_key"),
                     country_codes = 'ar')
  dir.sf <- st_as_sf(x = dir, coords = c("lon", "lat"), crs = 4326)
  
  # competencia <- osm_search(paste0(input$Rubro,"+posadas"), limit=5,
  #                           key = Sys.getenv("consumer_key"),
  #                           country_codes = 'ar')
  
  # if null
  if (is.null(dir)){
    dir <- NULL
    dir$lon = -55.89492
    dir$lat = -27.35812
    dir$place_id = "Erro"
  }

  m <- leaflet() %>% #cambiar a un mapa base mas limpio
    addTiles() %>%
    addProviderTiles(providers$Stamen.Toner) %>% 
    setView(dir$lon, dir$lat, zoom = 16) %>%
    # direccion buscada
    addAwesomeMarkers( 
      lng = dir$lon, lat = dir$lat, popup = paste(dir$display_name),
      icon = awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = "green")) %>% 
    # distribuicion competencia
    addAwesomeMarkers( 
      lng = st_coordinates(competencia)[,"X"],
      lat = st_coordinates(competencia)[,"Y"], 
      popup = paste(competencia$name),
      icon = awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = "red"))#%>% 
  #addLayersControl(
  #  baseGroups = c("Stamen.Toner"),
  #  overlayGroups = c("Hot SPrings"),
  #  options = layersControlOptions(collapsed = T)
  #  )
  })
    # Hacer que se haga la busqueda solo cuando se presione el boton
    output$mymap <- renderLeaflet({
      #Hay que poner con parentesis sino tira error
      boton_reactivo()
    })
}

# distancia para escuela mas proxima:
# st_distance(dir.sf, escuela)[which.min(st_distance(dir.sf, escuela))]