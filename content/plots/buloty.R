# source('munging.R')
# library(plotly)
# library(ggplot2)



#=1===========================================================================================================
#===========================================================================================================


output$plot2 <- renderPlotly({
  #2. Trends per country 
  # getFilteredData <- function(top5Countries,filterrr = "Confirmed"){
    fdata <- fullData %>%
      select(`Country/Region`, Date, Cases, value) %>%
      filter(`Country/Region` %in% input$selectizCountries & Cases == input$selectizCase  ) %>%
      group_by(`Country/Region`, Date) %>%
      summarise( "var" = sum(value)) %>%
      arrange(Date)
    
    
    plot_ly(fdata,
            x = ~Date,
            y = ~var,
            color = ~`Country/Region`,
            type = 'scatter',
            mode  = 'lines') %>%
      layout(
        yaxis = list(title = paste0(input$selectizCase, " ", "Cases")),
        xaxis = list(title = "Date")
      )
    
  # }
  # getFilteredData(countries,"Confirmed")
})




#=2===========================================================================================================
#===========================================================================================================




output$plot1 <- renderPlotly({
  data <- fullData %>%
    group_by(Date, Cases) %>%
    summarise("value" = sum(value))%>%
    as.data.frame()
  
  p <- plot_ly(data,
               x = ~Date,
               y = ~value,
               color = ~Cases,
               type = 'scatter',
               mode  = 'lines') %>%
    layout(
      yaxis = list(title = "Cases"),
      xaxis = list(title = "Date")
    )
  p
})





#==3==========================================================================================================
#===========================================================================================================


output$plot3 <- renderPlotly({
  datayu <- fullData %>%
    arrange(Date) %>%
    group_by(`Province/State`, `Country/Region`) %>%
    mutate(newCase = value - lag(value, 4, default = 0)) %>%
    ungroup()
  
  datay <- datayu %>%
    # filter(`Country/Region` =="China") %>%
    filter(if (input$selectizCountry == "All") TRUE else `Country/Region` %in% input$selectizCountry) %>%
    group_by(Date, Cases, `Country/Region`) %>%
    summarise(newCases = sum(newCase))
  
  
  if (input$selectizCountry == "All") {
    datay <- datay %>%
      group_by(Date, Cases) %>%
      summarise(newCases = sum(newCases))
  }
  
  
  datay %>%  plot_ly( x = ~Date, y = ~newCases, color = ~Cases, type = 'bar') %>%
    layout(
      yaxis = list(title = "New Cases"),
      xaxis = list(title = "Date"),
      title = ifelse(input$selectizCountry == "ALL","Global",input$selectizCountry)  
    )
})




######===============================================================================================



# INPUTS AND OUTPUTS

output$selectCountry <- renderUI({
  selectizeInput(
    "selectizCountry",
    label    = "Select Country",
    choices  = c("All", unique(fullData$`Country/Region`)),
    selected = "All"
  )
})


output$selectCountries <- renderUI({
  selectizeInput(
    "selectizCountries",
    label    = "Select Countries",
    choices  = unique(fullData$`Country/Region`),
    selected = top5Countries,
    multiple = TRUE
  )
})

output$selectizCases <- renderUI({
  radioButtons("selectizCase","Choose a case", choices = NULL, selected = "Confirmed",
             inline = TRUE, width = NULL, choiceNames = c("Confirmed","Dead","Recovered"),
             choiceValues = c("Confirmed","Dead","Recovered")
  )
})









output$ThreePlots <- renderUI({
  tagList(
    
    
    fluidRow(
      box(
        width = 12,
        title = "ACTIVE CASES",
        column(
          uiOutput("selectCountry"),
          width = 3,
          style = "float: right; padding: 10px; right: 400px; top:-40px"
        ),
        plotlyOutput("plot3")
      )
    ),
  
    
    
    fluidRow(
      box(
        width = 6,
        title = "PANDEMIC TREND SINCE THE OUTBREAK",
        plotlyOutput("plot1"),
        column(
          width = 3,
          style = "float: right; padding: 10px; margin-right: 50px"
        ),
        column(
          HTML("On December 31, 2019, the World Health Organisation’s (WHO) China office heard the first reports of a 
          previously-unknown virus behind a number of pneumonia cases 
          in Wuhan, a city in Eastern China with a population of over 11 million. What started as an epidemic 
          mainly limited to China has now become a truly global pandemic."),
          width = 12,
          style = " padding: 10px;"
        ),
      ),
      box(
        width = 6,
        title = "NEW CASES",
        plotlyOutput("plot2"),
        column(
          uiOutput("selectCountries"),
          width = 5,
          # style = "float: left; padding: 10px; margin-right: 50px; top:-40px;"
        ),
        column(
          uiOutput("selectizCases"),
          width = 5,
          style = " padding: 10px; top:5px;"
          
        )
      )
    )
  )
    
   
  
  
  
  
})





































#3. New cases per day since outbreak

capFirst <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}


datayu <- fullData %>%
  arrange(Date) %>%
  group_by(`Province/State`, `Country/Region`) %>%
  mutate(value_new = value - lag(value, 4, default = 0)) %>%
  ungroup()

datay <- datayu %>%
  filter(`Country/Region` =="China") %>%
  group_by(Date, Cases, `Country/Region`) %>%
  summarise(new_cases = sum(value_new))

datay %>%  plot_ly( x = ~Date, y = ~new_cases, color = ~Cases, type = 'bar') %>%
  layout(
    yaxis = list(title = "# New Cases"),
    xaxis = list(title = "Date"),
    title = "New Cases"
  )














