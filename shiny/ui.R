library(shiny)
library(leaflet)
library(shinythemes)

navbarPage(
  "Milwaukee Housing",
  id = "main",
  tabPanel("Map", leafletOutput("bbmap", height = 1000)),
  tabPanel("Data", DT::dataTableOutput("data"))
)
