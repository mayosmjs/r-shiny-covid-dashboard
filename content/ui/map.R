mapBody <- dashboardBody(
  
      leafletOutput("overview_map",height="930px"),
      
      
      absolutePanel(id = "controls", class = "panel", fixed = TRUE,
                    draggable = TRUE, top = 80, left = "auto", right = 40, bottom = "auto",
                    width = 600, height = "auto",
                    h2("GLOBAL PANDEMIC OVERVIEW"),
                    h6("Author: Calvin.B.K"),
                    hr(),
                    
                  
                    uiOutput("valueBoxes"),
                    
                    
                    fluidRow(
                      column(
                        sliderInput(
                          "timeSlider",
                          label      = "Press Play Or Pick a Date",
                          min        = min(fullData$Date),
                          max        = max(fullData$Date),
                          value      = max(fullData$Date),
                          width      = "100%",
                          timeFormat = "%d.%m.%Y",
                          animate    = animationOptions(loop = TRUE)
                        ),
                        width = 12,
                        style = 'padding-left:15px; padding-right:15px;color:green'
                      )
                    )
                      
          
   
  )
)

worldMapPage <- dashboardPage(
  title   = "Maps",
  sidebar = dashboardSidebar(disable = TRUE),
  header  = dashboardHeader(disable = TRUE),
  body    = mapBody
)