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
                           value = "pedernera, 2037 - posadas"),
                radioButtons( inputId = "Rubro",
                           label = "Seleccionar rubro",
                           choices = c("Restaurante", "Bar", "otro1"),
                           selected = "Restaurante"),
                actionButton(inputId = "ActualizarIndicadores", label = "Buscar")),
             mainPanel(
               leafletOutput("mymap",height = 1000)
  )
 )
)