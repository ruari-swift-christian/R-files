df_covid <- read.csv(file="Europe_covid_cases.csv", sep=',', dec='.', stringsAsFactors = FALSE)
;
head(df_covid)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(shiny)
attach(df_covid)
library(shinyWidgets)
library(reshape2)
library(plotly)
library(dslabs)
install.packages("dslabs")
install.packages("reshape2")
install.packages("shinyWidgets")
sort(df_covid$dateRep, descending = TRUE)
df_covid[order(as.Date(-df_covid$dateRep, format="%d/%m/%Y")),]
df_covid$dateRep <- as.Date(gsub("","", df_covid$dateRep), format="%dd%mm%YY")
sapply(df_covid, class)
dates <- as.Date(df_covid$dateRep, "%m/%d/%Y")
dates
dates <- as.factor(df_covid$dateRep)
abis<-strptime(dates,format="%d/%m/%Y") 
dates1 <- as.Date(abis, format="%Y-%m-%d")
dates1
class(dates1)
df_covid$dateRep=dates1
##shiny
stopApp(returnValue = invisible())
ui <- fluidPage(
  
  #Header
  headerPanel("Europe Covid Data Time Series Plot"),
  sidebarPanel(
    selectInput(
      inputId = "countriesandTerritories",
      label = "Select countriesAndTerritories",
      choices = unique(df_covid$countriesAndTerritories),
      selected = "Austria"
    ),
    sliderInput("dateRep", "Date:",
                min = Sys.Date()-36, max = Sys.Date()-3,
                value = Sys.Date()-3, step = 1)
  ),
  mainPanel(
  plotOutput("plot")
  )
)


server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    df_covid %>%
      filter(dateRep == input$dateRep) %>%
      ggplot(aes(cases, countriesAndTerritories)) +
      geom_line()
  })
  
}

shinyApp(ui, server)

ui <- fluidPage(
  
  #Header
  h1("Time series plot"),
  
  selectInput(
    inputId = "countriesandTerritories",
    label = "Select countriesAndTerritories",
    choices = unique(df_countriesAndTerritories),
    selected = "Dom. Rep"
  ),
  
  plotOutput("plot")
)


server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    data %>%
      filter(country == input$country) %>%
      ggplot(aes(date, price)) +
      geom_line()
  })
  
}

shinyApp(ui, server)

server <- function(input, output, session) {
output$plot = renderPlot({
  plot.df_covid <- melt(df_covid, id.vars = 'geoId')
  #not sure if input$cnt is a list or a vector
  #may need to manipulate that before passing
  plot.df_covid <- plot.df_covid[plot.df_covid$variable %in% input$countriesAndTerritories, ]
  ggplot(df_covid, aes(geoId, deaths, colour = countriesAndTerritories)) +
    geom_bar(stat="identity") +
    geom_line(mapping = aes(x = geoId, y = deaths, colour = countriesAndTerritories)) + 
    labs (x = "geoId", y = "deaths", title = "cases each day in each European Country") + 
    scale_colour_discrete(name = "countriesAndTerritories")
})
}



server <- function(input, output) {
  
  newCovid <- reactive({
    filter(df_covid, between(dateRep ,input$dateRep[1], input$dateRep[2220]))
  })

  output$plot <- renderPlot({
    
    ggplot(newCovid(), aes(x=dateRep, y = cases, color=countriesAndTerritories)) +
      geom_line() + 
      theme_bw() +
      xlab(input$dateRep) +
      ylab(input$cases) +
      ggtitle("Cases over time")
  })
  
}

shinyApp(ui=ui, server=server)

d <- reactive({
  getSymbols(countriesAndTerritories == input$countriesAndTerritories,
             from = input$dateRep[1],
             to = input$dateRep[2217],
             auto.assign = FALSE)
}) 



d <- reactive({
 df_covid %>%
    filter(countriesAndTerritories %in% input$countriesAndTerritories,
           date >= input$ydateRep[1],
           date <= input$yearInput[2217])
}) 

dplyr::filter(dplyr::between(RFR_Score, first(AvgScore_Per_Group), first(Upper_SD_Threshold)) )



reactive_data <- reactive({
  df_covid %>%
    dplyr::filter(dplyr::between(countriesAndTerritories, first(dateRep), first(cases)))
  
}) 