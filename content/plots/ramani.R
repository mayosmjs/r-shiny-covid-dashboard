
addLabel <- function(data) {
  data$label <- paste0(
    '<b>', ifelse(is.na(data$`Province/State`), data$`Country/Region`, data$`Province/State`), '</b><br>
    <img src ="',paste0("images/",data$alpha2,".png"),'"/>
    <table style="width:180px; background:#fff">
    <tr><td>Confirmed:</td><td align="right">', data$Confirmed, '</td></tr>
    <tr><td>Dead:</td><td align="right">', data$Dead, '</td></tr>
    <tr><td>Recovered:</td><td align="right">', data$Recovered, '</td></tr>
    <tr><td>Active:</td><td align="right">', data$Active, '</td></tr>
    </table>'
  )
  data$label <- lapply(data$label, HTML)
  
  return(data)
}




map <- leaflet(addLabel(finalUptodate)) %>%
  setMaxBounds(-180, -90, 180, 90) %>%
  setView(lat = 14.9179823, lng = 60.607576, zoom = 4) %>%
  addTiles() %>%
  addLayersControl(
    baseGroups    = c("Light", "Satellite"),
    overlayGroups = c("Confirmed", "Recovered", "Dead", "Active")
  ) %>%
  hideGroup("Recovered") %>%
  hideGroup("Deceased") %>%
  hideGroup("Active") %>%
  addEasyButton(easyButton(
    icon    = "glyphicon glyphicon-globe", title = "Reset zoom",
    onClick = JS("function(btn, map){ map.setView([14.9179823, ], 2); }"))) %>%
  addEasyButton(easyButton(
    icon    = "glyphicon glyphicon-map-marker", title = "My Location",
    onClick = JS("function(btn, map){ map.locate({setView: true, maxZoom: 6}); }")))

observe({
  req(input$timeSlider, input$overview_map_zoom)
  zoomLevel               <- input$overview_map_zoom
  data                    <- presentData(input$timeSlider) %>% addLabel()
  
  
  leafletProxy("overview_map", data = data) %>%
    clearMarkers() %>%
    addCircleMarkers(
      lng          = ~Long,
      lat          = ~Lat,
      radius       = ~log(Confirmed^(zoomLevel / 2)),
      stroke       = FALSE,
      fillOpacity  = 0.5,
      label        = ~label,
      labelOptions = labelOptions(textsize = 15),
      group        = "Confirmed",
      color = 'red'
    )  %>%
    addCircleMarkers(
      lng          = ~Long,
      lat          = ~Lat,
      radius       = ~log(Recovered^(zoomLevel)),
      stroke       = FALSE,
      color        = "green",
      fillOpacity  = 0.5,
      label        = ~label,
      labelOptions = labelOptions(textsize = 15),
      group        = "Recovered"
    ) %>%
    addCircleMarkers(
      lng          = ~Long,
      lat          = ~Lat,
      radius       = ~log(Dead^(zoomLevel)),
      stroke       = FALSE,
      color        = "#E7590B",
      fillOpacity  = 0.5,
      label        = ~label,
      labelOptions = labelOptions(textsize = 15),
      group        = "Dead"
    ) %>%
    addCircleMarkers(
      lng          = ~Long,
      lat          = ~Lat,
      radius       = ~log(Active^(zoomLevel / 2)),
      stroke       = FALSE,
      color        = "#f49e19",
      fillOpacity  = 0.5,
      label        = ~label,
      labelOptions = labelOptions(textsize = 15),
      group        = "Active"
    )
})

output$overview_map <- renderLeaflet(map)







sumData <- function(date) {
  if (date >= min(fullData$Date)) {
    data <- finalUptodate(date) %>% summarise(
      Confirmed = sum(Confirmed),
      Recovered = sum(Recovered),
      Dead      = sum(Dead),
    )
    return(data)
  }
  return(NULL)
}

boxes <- reactive({
  data           <- sumData(input$timeSlider)
  data_yesterday <- sumData(input$timeSlider - 1)
  
  data_new <- list(
    new_confirmed <-  (data$Confirmed - data_yesterday$Confirmed) / data_yesterday$Confirmed * 100,
    new_recovered <-  (data$Recovered - data_yesterday$Recovered) / data_yesterday$Recovered * 100,
    new_deceased  <-  (data$Dead - data_yesterday$Dead) / data_yesterday$Dead * 100,
  )
  
  boxesList <- list(
    "confirmed" =  data_new$new_confirmed,
    "recovered" = HTML(paste(format(data$Recovered, big.mark = " "), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_recovered))),
    "deceased"  = HTML(paste(format(data$Dead,      big.mark = " "), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_deceased))),
  )
  return(data_new)
})


dataL <- finalUptodate %>% summarise(
  Confirmed = sum(Confirmed),
  Recovered = sum(Recovered),
  Dead      = sum(Dead),
)




output$vRecov <- renderValueBox({
  valueBox(
    dataL$Recovered[1],
    subtitle = "Recovered Cases",
    icon     = icon("notes-medical"),
    color    = "green"
  )
})

output$vDead <- renderValueBox({
  valueBox(
    dataL$Dead[1],
    subtitle = "Total Deaths",
    icon     = icon("skull"),
    color    = "red"
  )
})

output$vConf <- renderValueBox({
  valueBox(
    dataL$Confirmed[1],
    subtitle = "Confirmed Cases",
    icon     = icon("flag-checkered"),
    color    = "light-blue"
  )
})


output$kenya <- renderValueBox({
  valueBox(
    "KENYA",
    HTML(paste0("Confirmed Cases :",kenya$Confirmed, "<br>" ,"Recovered Cases : ",kenya$Recovered, "<br>",
                          "Total Deaths :", kenya$Dead)),
    icon     = icon("flag-checkered"),
    color    = "navy"
  )
})




output$valueBoxes <- renderUI(box(

      valueBoxOutput("vConf", width = 12),
      valueBoxOutput("vRecov", width = 6),
      valueBoxOutput("vDead", width = 6),
      valueBoxOutput("kenya", width = 12),
      width = 12,
      style ="background:#000"

  ))







