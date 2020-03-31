plotBody <- dashboardBody(
  
  dataTableOutput("data.table")
  
)

tablePage <- dashboardPage(
  title   = "Plots",
  header  = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body    = plotBody
)




# plotDataTable <- function(){
#   datatable( populationData,
#              class = 'cell-border stripe',
#              editable = TRUE,
#              options = list(scrollX = T)
#   ) 
#   
# }