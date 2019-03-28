## Cargando librerias
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
#devtools::install_github("hrbrmstr/nominatim")
library(nominatim)
library(stringr)

# Shiny server
server <- function(input,output){
  
  boton_reactivo <- eventReactive(input$ActualizarIndicadores, {
    if(input$Rubro == "Restaurante"){
      competencia <- read_sf("./Datos/DatosEspaciais.gpkg", "Restaurantes")
    } else {
      competencia <- read_sf("./Datos/DatosEspaciais.gpkg", "Bar")
    }
    
    # direccion buscada
    dir <- osm_geocode(
      #tolower(input$Direccion),
      "pedernera, 2037 - posadas, ar",
                     limit=1,
                     key = Sys.getenv("consumer_key"),
                     country_codes = 'ar')
    
    dir.sf <- st_as_sf(x = dir, coords = c("lon", "lat"), crs = 4326)

    
    #Building index
    # distance from school
    escuela <- read_sf("./Datos/DatosEspaciais.gpkg", "Escuelas")
    cole <- read_sf("./Datos/DatosEspaciais.gpkg", "ParadaColectivos")
    # distancia para escuela mas proxima:
    dist.escuelas <- st_distance(dir.sf, escuela)[which.min(st_distance(dir.sf, escuela))]
    dist.escuelas <- round(as.numeric(str_split(dist.escuelas, " ")), 2)
    
    dist.paradacole <- st_distance(dir.sf, escuela)[which.min(st_distance(dir.sf, cole))]
    dist.paradacole <- round(as.numeric(str_split(dist.paradacole, " ")), 2)
    tbl <- data.frame(c("Parada Colectivos", "Escuela"), c(dist.paradacole, dist.escuelas))
    colnames(tbl) <- c("Elemento", "Distancia")
    indices <- ggplot(tbl) + geom_col(aes(x = Elemento, y = Distancia, fill = Elemento)) + coord_flip()
    output$PlotIndices <- renderPlotly({indices})
    
    
    # # Market share
    # Seguir por acá: https://stackoverflow.com/questions/45719790/create-voronoi-polygon-with-simple-feature-in-r
    # bbox_polygon <- function(x) {
    #   bb <- sf::st_bbox(x)
    #   p <- matrix(
    #     c(bb["xmin"], bb["ymin"], 
    #       bb["xmin"], bb["ymax"],
    #       bb["xmax"], bb["ymax"], 
    #       bb["xmax"], bb["ymin"], 
    #       bb["xmin"], bb["ymin"]),
    #     ncol = 2, byrow = T
    #   )
    #   
    #   sf::st_polygon(list(p))
    # }
    # box <- st_sfc(bbox_polygon(competencia))
    # 
    # voronoi <- st_voronoi(
    #   st_transform(
    #       st_union(
    #       st_geometry(dir.sf), st_geometry(competencia)), 5343), #envelope, 
    #   dTolerance = 1, bOnlyEdges = FALSE)
    # plot(st_geometry(voronoi))

    
    # Mapa
    m <- leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Stamen.Toner) %>% 
      setView(dir$lon, dir$lat, zoom = 16) %>%
      
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
          markerColor = "red")) %>% 
      addLayersControl(
        baseGroups = c("Stamen.Toner"),
        overlayGroups = c("Hot SPrings"),
        options = layersControlOptions(collapsed = T)
        )
    })
  
  output$mymap <- renderLeaflet({
    boton_reactivo()
    })
  }
