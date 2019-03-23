# Cargando librerias
library(shiny)
library(leaflet)
library(tidyverse)

# Shiny UI
?setView
ui <- fluidPage(
        titlePanel("Inmobiliaria"),
            sidebarLayout(
              sidebarPanel(
                textInput( inputId = "texto",
                           label = "Seleccionar dirección",
                           value = "")),
             mainPanel(
               leafletOutput("mymap",height = 1000)
  )
 )
)