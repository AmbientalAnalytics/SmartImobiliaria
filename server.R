## Cargando librerias
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(leaflet)
library(tidyverse)
library(sf)
library(rgdal)
library(plotly)
#devtools::install_github("hrbrmstr/nominatim")
library(nominatim)
library(stringr)

# Shiny server
server <- function(input,output){
  
  boton_reactivo <- eventReactive(input$buscar, {
    if(input$Rubro == "Restaurante"){
      competencia <- read_sf("./Datos/DatosEspaciais.gpkg", "Restaurantes")
    } else {
      competencia <- read_sf("./Datos/DatosEspaciais.gpkg", "Bar")
    }
    
    # direccion buscada
    dir <- osm_geocode(
      tolower(input$direccion),
      #"mitre, 2349 - posadas, ar",
      limit=1,
      key = Sys.getenv("consumer_key"),
      country_codes = 'ar')
    
    dir.sf <- st_as_sf(x = dir, coords = c("lon", "lat"), crs = 4326)
    
    
    #Building index
    # distance from school
    escuela <- read_sf("./Datos/DatosEspaciais.gpkg", "Escuelas")
    cole <- read_sf("./Datos/DatosEspaciais.gpkg", "ParadaColectivos")
    # distancia para escuela mas proxima:
    dist.escuelas <- st_distance(dir.sf, escuela) %>% sort()
    dist.escuelas <- round(as.numeric(dist.escuelas)[1], 2)
    
    dist.paradacole <- st_distance(dir.sf, cole) %>% sort()
    dist.paradacole <- round(as.numeric(dist.paradacole)[1], 2)

    tbl <- data.frame(c("Parada Colectivos", "Escuela"), c(dist.paradacole, dist.escuelas))
    colnames(tbl) <- c("Elemento", "Distancia")
    indices <- ggplot(tbl) + geom_col(aes(x = Elemento, y = Distancia, fill = Elemento), color = "black") +
    labs(y = "Distancia (m)", x = "")+ coord_flip() +
    scale_fill_brewer(palette="Dark2") + ylim(0, 300) + theme_classic() +
    theme(legend.position = "none")
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
    
    # Añadiendo shape del Censo 2010
    censo_2010 <- st_read("C:\\Users\\Usuario\\Documents\\Ambiental Analytics\\SmartImobiliaria\\Datos\\Censo_2010\\Censo2010Fixed.shp",
    "Censo2010Fixed")
    censo_2010 <- st_transform(censo_2010, 4326)
    # Mapa
    if (input$KPI == "Población") {
      poblacion <- censo_2010$totalpobl
    } else if (input$KPI == "Hombres") {
      poblacion <- censo_2010$varon
    } else  {
      poblacion <- censo_2010$mujer
    }

    m <- leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons(data = censo_2010,
                  fillColor = colorBin(palette="YlOrRd", domain=c(min(poblacion), max(poblacion)), bins = 5)(poblacion),#colorQuantile("YlOrRd",poblacion)(poblacion), #(as.numeric(input$KPI)),
                  fillOpacity = 0.6) %>%
      addLegend(position = c("bottomright"), 
                pal = colorBin(palette="YlOrRd", domain=c(min(poblacion), max(poblacion)), bins = 5),#colorQuantile("YlOrRd",poblacion),
                values = poblacion, opacity = 0.6,
                title = "Intervalos") %>% 
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
      #addMarkers(lng = st_coordinates(cole)[,"X"],
                 #lat = st_coordinates(cole)[,"Y"]) %>% 
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
