library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyverse)
library(gifski)
library(leaflet)
library(png)
library(tigris)
month=read.csv("covid month by month NC.csv", header=TRUE, sep=',', dec='.', stringsAsFactors=FALSE)
month=read.csv("covid-month-year-NC.csv", header=TRUE, sep=',', dec='.', stringsAsFactors=FALSE)

month<-month[,-3]
month<-as.data.frame(month)
month$Month <- c("03/20", "04/20","05/20","06/20","07/20","08/20","09/20","10/20","11/20","12/20","1/21","2/21")
date <-as.Date(month$Month, format="%m/%d/%y")
p <- ggplot(month, aes(Month, Covid.Cases, fill = Covid.Cases)) +
  geom_col() +  
  scale_fill_distiller(palette = "Greens", direction = 1)  +
  theme_minimal() +  
  geom_text(aes(label = signif(Covid.Cases, digits = 3)), nudge_y = 8, color="red") +
  theme(axis.text.x = element_text(size=6))+
  theme(plot.background = element_rect(fill = "#BFD5E3"))
  theme(panel.grid = element_blank(), panel.grid.major.y = element_line(color = "black"),
        panel.ontop = TRUE)
p
p + transition_states(Month, wrap = FALSE) +
  shadow_mark() +
  enter_grow() +
  enter_fade()
p1 <- ggplot(month, aes(Month, Covid.Cases, group = 2, color = factor(Month))) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Month", y = "Covid-19 Cases") +
  theme(legend.position = "top")
p1
p1 + 
geom_point() +
transition_reveal(Month)
month$Month <- c("1","2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
month$Month<-factor(month$Month)
month$Month <- as.numeric(month$Month)
##Census Map

map=read.csv("covid_project1.csv", header=TRUE, sep=',', dec='.', stringsAsFactors=FALSE)
map
map<-map[-26] ; map<- map[-25]
states <- states(cb=T)
states %>% leaflet() %>% 
  addTiles() %>% 
  addPolygons(popup=~NAME)
covid_state <- map %>%
  group_by(State) %>%
  summarize(total=n()) 
states_merged_covid_map <- geo_join(states, covid_state, "STUPS", "state")
map$Total.Covid.19.Deaths <- as.numeric(gsub(",","",map$Total.Covid.19.Deaths))
map$Total.Covid.19.Cases <- as.numeric(gsub(",","",map$Total.Covid.19.Cases))
map$weekly.average.cases <- as.numeric(gsub(",","",map$weekly.average.cases))
map$vaccines.distributed <- as.numeric(gsub(",","",map$vaccines.distributed))
map$Total.Population <- as.numeric(gsub(",","",map$Total.Population))
map$vaccinated <- as.numeric(gsub(",","",map$vaccinated))
map$Covid.Cases.65. <- as.numeric(gsub(",","",map$Covid.Cases.65.))
map$Covid.Cases.under.65 <- as.numeric(gsub(",","",map$Covid.Cases.under.65))
map$recovered <- as.numeric(gsub(",","",map$recovered))
map$total.number.of.tests <- as.numeric(gsub(",","",map$total.number.of.tests))
map$PCR. <- as.numeric(gsub(",","",map$PCR.))
map$PCR..1 <- as.numeric(gsub(",","",map$PCR.))
map$antigen. <- as.numeric(gsub(",","",map$antigen.))
map$antigen..1 <- as.numeric(gsub(",","",map$antigen..1))
map$Total.. <- as.numeric(gsub(",","",map$Total..))
map$Total...1 <- as.numeric(gsub(",","",map$Total...1))
map$VaccinatedPerCapita <- as.numeric(gsub(",","",map$Total...1))

install.packages("plotly")
library(plotly)
map$VaccinatedPerCapita <- round(map$vaccinated/map$Total.Population*100000,2)
map$hover <- with(map, paste(State, '<br>', "Vaccinated per 100,000 residents"))
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig <- plot_geo(map, locationmode = 'USA-states')
fig <- fig %>% 
  add_trace(
  z = ~VaccinatedPerCapita, text = ~hover, locations = map$State,
  color = ~VaccinatedPerCapita, colors = 'Blues'
  )
fig <- fig %>% colorbar(title = "vaccinated per 100,000 citizens")
fig <- fig %>% 
layout(title = 'Covid Vaccinated per 100,000 residents each US State<br>(Hover for breakdown)', 
       geo = g
       )
fig
plot_ly(type="choropleth", locations=map$State, locationmode="USA-states", z=map$vaccinated) %>% 
  layout(geo=list(scope="usa"))
map$State<-c("AL","AL","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL",
  "IN", "IA", "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE",
  "NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD",
  "TN","TX","UT","VT","VA","WA","WV","WI","WY")
