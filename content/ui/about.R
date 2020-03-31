body_about <- dashboardBody(
  fluidRow(
    fluidRow(
      column(
        box(
          title = div("ABOUT CORONA DASHBOARD", style = "padding: 20px", class = "h1"),
          column(
            "This project is or was all but shear fun and learning process in the life of data enthusiast. Anyway the
            motivation to come up with such a dashboard is to draw insight with visualization about the spread of the Corona
            virus AKA Covid-19 pandemic.",
            tags$br(),
            h3("Data"),
            "Data can be found from various sources,but on this project we rely on John Hopkins Github repository which is 
            updated regularly.While using this dashboard, the figures may be different from what you see on breaking news",
            
            tags$ul(
              tags$li(tags$b("Hopkins data :"), tags$a(href = "https://github.com/CSSEGISandData/COVID-19",
                                                       "Johns Hopkins CSSE",style="color:red;")),
              tags$li(tags$b("Real Time data :"), tags$a(href = "https://www.worldometers.info/coronavirus/",
                                                         "Worldometers",style="color:red;"))
            ),
            
         
            tags$br(),
            h3("How it works"),
            "We query data at intervals of one hour using Events just like cronjobs to fetch updated dated regulary",
            tags$br(),
            
        
            width = 12,
          ),
          width = 6,
        ),
        width = 12,
      )
    )
  )
)

page_about <- dashboardPage(
  title   = "About",
  header  = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body    = body_about
)