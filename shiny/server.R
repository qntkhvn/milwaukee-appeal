library(shiny)
library(tidyverse)
library(leaflet)
library(DT)

shinyServer(function(input, output) {
  housing <- read_csv("~/Desktop/consulting/map/housing2.csv") %>%
    filter(!is.na(lat)) %>%
    mutate(address = str_to_upper(address)) %>%
    select(
      prop_id,
      address,
      nbhd,
      bld_type,
      qual,
      cond,
      kitchen_ct,
      kitchen_rating,
      full_bath_ct,
      full_bath_rating,
      half_bath_ct,
      half_bath_rating,
      year_built,
      finished_area,
      land_sf,
      prob_appealed,
      lat,
      long
    ) %>%
    filter(prob_appealed >= 0.2) %>% 
    mutate(
      pop = str_c(
        '<strong>Address: </strong>',
        address,
        '<br><strong>Neighborhood:</strong> ',
        nbhd,
        '<br><strong>Building Type:</strong> ',
        bld_type,
        '<br><strong>Quality:</strong> ',
        qual,
        '<br><strong>Condition:</strong> ',
        cond,
        '<br><strong>Year Built:</strong> ',
        year_built,
        '<br><strong>Finished Area:</strong> ',
        finished_area,
        '<br><strong>Land (sq. ft):</strong> ',
        land_sf,
        '<br><strong>Probability of Appealed:</strong> ',
        round(prob_appealed, 4)
      )
    )
  
  
  # create a color paletter for category type in the data file
  
  # pal <- colorFactor(pal = c("midnightblue", "salmon"), domain = c("Appealed", "Not Appealed"))
  pal <-
    colorNumeric(
      palette = "PRGn",
      domain = housing$prob_appealed,
      alpha = 0.05
    )
  
  # create the leaflet map
  output$bbmap <- renderLeaflet({
    leaflet(housing) %>%
      addCircles(lng = ~ long, lat = ~ lat) %>%
      addTiles() %>%
      addCircleMarkers(
        data = housing,
        lat =  ~ lat,
        lng =  ~ long,
        radius = ~ (10 * prob_appealed),
        # radius = 3,
        popup = ~ as.character(pop),
        color = ~ pal(prob_appealed),
        stroke = TRUE,
        fillOpacity = 1
      ) %>%
      addLegend(
        pal = pal,
        values = housing$prob_appealed,
        opacity = 1,
        na.label = "Not Available"
      )
    
    # addEasyButton(easyButton(
    #   icon="fa-crosshairs", title="ME",
    #   onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  #create a data object to display data
  
  output$data <- DT::renderDataTable(datatable(
    select(housing, -pop),
    filter = "top",
    colnames = colnames(select(housing, -pop))
  ))
  
  
})
