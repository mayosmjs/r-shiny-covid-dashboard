
source("content/ui/plots.R", local = TRUE)
source("content/ui/map.R", local = TRUE)
source("content/ui/table.r", local = TRUE)
source('content/ui/about.R',local = TRUE)



# Define UI for application that draws a histogram
shinyUI(fluidPage( theme = shinytheme("slate"),
                   #shinythemes::themeSelector(),
    
    title = "THE COVID-19 PANDEMIC",
    tags$head(
        includeCSS("style.css")
    ),
    tags$style(type = "text/css", ".container-fluid {padding-left: 0px; padding-right: 0px !important;}"),
    tags$style(type = "text/css", ".navbar {margin-bottom: 0px;}"),
    tags$style(type = "text/css", ".content {padding: 0px;}"),
    tags$style(type = "text/css", ".row {margin-left: 0px; margin-right: 0px;}"),
    tags$style(HTML(".col-sm-12 { padding: 5px; margin-bottom: -15px; }")),
    tags$style(HTML(".col-sm-6 { padding: 5px; margin-bottom: -15px; }")),
    tags$style(type = "text/css", ".wrapper {overflow-y:hidden; overflow-x:hidden}"),
   
    
    
    navbarPage(
        title       = div("THE COVID-19 PANDEMIC", style = "padding-left: 10px"),
        collapsible = TRUE,
        fluid       = TRUE,
        tabPanel("World Map",worldMapPage,value = 'world_map_page'),
        tabPanel("Plots",plotPage,value = 'plot-page'),
        tabPanel("Table",tablePage,value = 'table-page'),
        tabPanel("About Dashboard", page_about, value='about-page')
    )
    
    
    

   
))#End of fluidPage
