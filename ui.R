# Cargando librerias
library(shiny)
library(leaflet)
library(tidyverse)

# Shiny UI
#?setView
ui <- fluidPage(
        titlePanel("Inmobiliaria"),
            sidebarLayout(
              sidebarPanel(
                textInput( inputId = "Direccion",
                           label = "Seleccionar dirección",
                           value = "pedernera, 2037 - posadas")),
             mainPanel(
               leafletOutput("mymap",height = 1000)
  )
 )
)