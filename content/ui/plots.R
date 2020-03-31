plotBody <- dashboardBody(
      uiOutput("ThreePlots")
)

plotPage <- dashboardPage(
  title   = "Plots",
  header  = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body    = plotBody
)