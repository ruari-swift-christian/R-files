strikes <- read.csv("Drone Strikes.csv", header = TRUE, stringsAsFactors = FALSE)
strikes
library(ggplot2)
install.packages("gganimate")
library(gganimate)
library(dplyr)
install.packages("viridis")
library(viridis)
install.packages("hrbrthemes")
library(hrbrthemes)
strikes %>%
  ggplot( aes(x=Year, y=Civilians.reported.Killed, group=President, color=President)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Civilian Casualties from US Drone Strikes 2015-2020") +
  theme_ipsum() +
  ylab("Number of civilians killed") +
  transition_reveal(Year)

anim_save("DroneStrikeCasualtiesInAfghanistan.gif")

strikes %>%
  ggplot( aes(x=Year, y=Number.of.Strikes.reported, group=President, color=President)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Number of  US Drone Strikes in Afghanistan 2015-2020") +
  theme_ipsum() +
  ylab("Number of strikes") +
  transition_reveal(Year)


anim_save("DroneStrikesAfghanistan.gif")
