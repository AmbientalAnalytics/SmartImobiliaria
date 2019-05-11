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
  
  bbox_polygon <- function(x) {
    bb <- sf::st_bbox(x)
    p <- matrix(
      c(bb["xmin"], bb["ymin"], 
        bb["xmin"], bb["ymax"],
        bb["xmax"], bb["ymax"], 
        bb["xmax"], bb["ymin"], 
        bb["xmin"], bb["ymin"]),
      ncol = 2, byrow = T
    )
    
    sf::st_polygon(list(p))
  }
  
  
  boton_reactivo <- eventReactive(input$buscar, {
    if(input$Rubro == "Restaurante"){
      competencia <- read_sf("./Datos/DatosEspaciais.gpkg", "Restaurantes")
    } else {
      competencia <- read_sf("./Datos/DatosEspaciais.gpkg", "Bar")
    }
    
    # direccion buscada
    dir <- osm_geocode(
      #tolower(input$direccion),
      "mitre, 2349 - posadas, ar",
      limit=1,
      key = Sys.getenv("consumer_key"),
      country_codes = 'ar')
    
    dir.sf <- st_as_sf(x = dir, coords = c("lon", "lat"), crs = 4326)
    
    # Añadiendo shape del Censo 2010
    censo_2010 <- st_read("./Datos/Censo_2010/Censo2010Fixed.shp",
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
    censo_2010$area <- st_area(censo_2010)
    posadas <-
      censo_2010 %>%
      summarise(area = sum(area)) %>% st_buffer(0.00004)
    dirVoronoi <- dir.sf %>% select() 
    competenciaVoronoi <- competencia %>% select()
    unionpts <- st_union(
      dirVoronoi, competenciaVoronoi)
    box <- st_sfc(bbox_polygon(unionpts))
    #box <- st_sfc(bbox_polygon(posadas))
    #pbox <- st_cast(posadas)
    
    voronoi <- st_voronoi(st_union(unionpts), box)
    voronoi <- st_cast(voronoi)
    voronoi <- st_sf(id = 1:length(voronoi), geom = voronoi)

    #st_sfc(voronoi)
    #plot(voronoi, col = 0)
    #voronoi <- st_cast(voronoi, "MULTIPOLYGON")
    #inherits(voronoi, "sf")
    
    voronoi$area <- round(as.numeric(st_area(voronoi)),2)
    #plot(voronoi["area"])
    dir.sf$MktShare <- (st_join(dir.sf, voronoi, join = st_intersects)[["area"]]/mean(voronoi$area))*100
    MktShare <- data.frame("MeanShare" = (st_join(unionpts, voronoi, join = st_intersects)[["area"]]/mean(voronoi$area))*100)
    MktShare <- mutate(MktShare, NormShare = MeanShare/max(MeanShare))
    mktsharePlot <- ggplot(MktShare, aes(x = NormShare, y = "")) + geom_line() +
      geom_point(x = dir.sf$MktShare, col='red') + geom_text(aes(label = "Nuevo Local"), x = dir.sf$MktShare, y = 1.1)
    output$PlotMkt <- renderPlotly({mktsharePlot})

    
    m <- leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons(data = censo_2010,
                  fillColor = colorBin(palette="YlOrRd", domain=c(min(poblacion), max(poblacion)), bins = 5)(poblacion), fillOpacity = 0.5,
                  stroke = T, color = "#323232", weight = 1.5) %>%
      addLegend(position = c("bottomright"), 
                pal = colorBin(palette="YlOrRd", domain=c(min(poblacion), max(poblacion)), bins = 5),
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
          markerColor = "red"),
        group = "Competencia") %>% 
      #addMarkers(lng = st_coordinates(cole)[,"X"],
                 #lat = st_coordinates(cole)[,"Y"]) %>% 
      addLayersControl(
        baseGroups = c("Stamen.Toner"),
        overlayGroups = c("Competencia"),
        options = layersControlOptions(collapsed = T))
    })
  
  output$mymap <- renderLeaflet({
    boton_reactivo()
  })
}
