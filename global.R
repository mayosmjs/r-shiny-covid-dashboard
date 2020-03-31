library("fs")
library("shinydashboard")
library("shiny")
library("leaflet")
library("plotly")
library("htmltools")
library(shinythemes)
library(tidyverse)
library(reshape2)
library(DT)
# library(bigrquery)



hopkinsGithubData <- function() {
  download.file(
    url      = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip",
    destfile = "data/covid19_data.zip"
  )
  
  path <- "COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"
  unzip(
    zipfile   = "data/covid19_data.zip",
    files     = paste0(path, c("confirmed_global.csv", "deaths_global.csv", "recovered_global.csv")),
    exdir     = "data",
    junkpaths = T
  )
}


updateData <- function() {
  if (!dir_exists("data")) {
    dir.create('data')
    hopkinsGithubData()
  } else if ((!file.exists("data/covid19_data.zip")) || (as.double(Sys.time() - file_info("data/covid19_data.zip")$change_time, units = "hours") > 20)) {
    hopkinsGithubData()
  }
}

updateData()

#use read_csv, for some rasons read.csv converts the headers into soe funny x'es
raw_confirmed_data <- read_csv("data/time_series_covid19_confirmed_global.csv")
raw_dead_data  <- read_csv("data/time_series_covid19_deaths_global.csv")
raw_recovered_data <- read_csv("data/time_series_covid19_recovered_global.csv")



# Get latest data
current_date <- as.Date(names(raw_confirmed_data)[ncol(raw_confirmed_data)], format = "%m/%d/%y")
changed_date <- file_info("data/covid19_data.zip")$change_time


#Get aggregated/mergerd data of confirmed recovered and dead
confirmedData <- raw_confirmed_data %>% melt(id=c('Province/State','Country/Region','Lat','Long'),
                                             variable.name = "Date")%>%
  group_by(`Province/State`, `Country/Region`, Date, Lat, Long) %>%
  summarise("Confirmed" = sum(value))



recoveredData <- raw_recovered_data %>% melt(id=c('Province/State','Country/Region','Lat','Long'),
                                             variable.name = "Date") %>%
  group_by(`Province/State`, `Country/Region`, Date, Lat, Long) %>%
  summarise("Recovered" = sum(value))


deadData <- raw_dead_data %>% melt(id=c('Province/State','Country/Region','Lat','Long'),
                                   variable.name = "Date") %>%
  group_by(`Province/State`, `Country/Region`, Date, Lat, Long) %>%
  summarise("Dead" = sum(value))


#merged data
fullData <- confirmedData %>% 
  full_join(recoveredData) %>%
  full_join(deadData) %>% 
  mutate(Active = (Confirmed - Recovered - Dead)) %>% 
  pivot_longer(names_to = "Cases", cols = c(Confirmed, Recovered, Dead, Active)) %>%
  ungroup()

fullData$Date <- as.Date(fullData$Date, "%m/%d/%y")

fullData$value <- ifelse(is.na(fullData$value),0,fullData$value)

top5Countries <- fullData %>%
  filter(Cases == "Active", Date == current_date) %>%
  group_by(`Country/Region`) %>%
  summarise(value = sum(value)) %>%
  arrange(desc(value)) %>%
  top_n(5) %>%
  select(`Country/Region`) %>%
  pull()


#countries flags
flags <- read.csv('countries.csv') %>% select('name','alpha2')

fullData <- fullData %>% 
  left_join(flags, by = c("Country/Region" = "name"))



presentData <- function(inputDate) {
  fullData[which(fullData$Date == inputDate),] %>%
    spread(Cases,value) %>%
    filter(Confirmed > 0 | Recovered > 0 | Dead > 0 | Active > 0)
}


#Get the max dates
finalUptodate <- presentData(max(fullData$Date))



#Kenya
kenya <- finalUptodate %>% filter(`Country/Region`=='Kenya')










