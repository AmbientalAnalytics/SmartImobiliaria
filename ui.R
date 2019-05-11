# Cargando librerias
## Cargando librerias
library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(sf)
library(rgdal)
library(plotly)
#devtools::install_github("hrbrmstr/nominatim")
library(nominatim)
library(stringr)

# ShinyDashboard UI
ui <- dashboardPage(skin = "purple",
        dashboardHeader( title = "Inmobiliaria"),
          dashboardSidebar(
            sidebarMenu(
            sidebarSearchForm(textId = "direccion", buttonId = "buscar",
                              label = "Buscar dirección"),
            radioButtons(inputId = "Rubro",
                         label = "Seleccionar rubro",
                         choices = c("Restaurante", "Bar"),
                         selected = "Restaurante"),
                        selectInput(inputId = "KPI",
                                    label = "Seleccionar opción",
                                    choices = c("Población", "Hombres", "Mujeres"),
                                    multiple = FALSE)
            )),
          dashboardBody(
            box(title = "Mapa", status = "primary",solidHeader = T,
                collapsible = T,leafletOutput("mymap",height = 500)),
            box(title = "Indicadores", status = "warning",solidHeader = T,
                collapsible = T, plotlyOutput("PlotIndices", height = 300)),
            box(title = "Indicadores Espaciales", status = "warning",solidHeader = T,
                collapsible = T, plotOutput("PlotMkt", height = 300))
      )
    )