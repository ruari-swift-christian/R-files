install.packages("sp")
library(sp)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(plyr)
install.packages("reshape")
install.packages("scales")
library(scales)
library(reshape)
library(mapplots)
install.packages("mapplots")
install.packages("ggrepel")
AFG_adm1 <- readRDS("gadm36_AFG_1_sp.rds")
op <- par(mar=c(0,0,0,0))
plot(AFG_adm1)
AFG_adm1$NAME_1
install.packages("plotrix")
library(plotrix) 
install.packages("ggtree")
devtools::install_github("GuangchuangYu/ggtree")
install.packages("maps")
library(ggtree)
install.packages("pipeR")
install.packages("grid")
install.packages("XML")
install.packages("gtable")
p_load(pipeR,grid,XML,gtable)
library(pipeR);library(grid);library(XML);library(gtable);
head(AFG_adm1)
install.packages("pacman")
library(pacman)
library(maps)
library(ggmap)
library(gridBase)
install.packages("gridBase")
library(lattice)
library(grid)
library(raster)
piecolors <- structure(c("#FFFFFF", "#5C9147", "#000000","#FF9900"))
Names=c("Taliban Control", "ANA Control", "ISIS Control", "Contested")
df<-read.csv("Afghanistan Province Control.csv", header = TRUE, stringsAsFactors=FALSE, dec='.')

install.packages("conflicted")
library(conflicted)
df1 <- map_data('area', 'AFG_adm1')
getLabelPoint <- function(county) {Polygon(county[c('long', 'lat')])@labpt}

rgb(.36,.57,.28)

AFG_adm1

myglyff=function(AFG_adm1) {
  floating.pie(mean(AFG_adm1$long),
               mean(AFG_adm1$lat),
               x=c(gi[1,"Taliban"]+e,
                   gi[1,"ANA"]+e,
                   gi[1,"ISIS"]+e,
                   gi[1,"Contested"]+e),
               radius=.1) #insert size variable here
}


pieval<-c(2,3,0,19)
pielabels<- c("Taliban","ANA","ISIS","Contested")
# grab the radial positions of the labels
lp<-pie3D(pieval,radius=0.9,labels=pielabels,explode=0.1,main="District Control of Badakhshan Province")
# specify some new colors
pie3D(pieval,radius=0.9,labels=pielabels,explode=0.1,main="District Control of Badakhshan Province",
      col=c("#FFFFFF", "#5C9147", "#000000","#FF9900"),labelpos=lp)

data <- c(2,3,0,19)
lab <- paste0(round(data/sum(data) * 100, 2), "%")

pie3D(data,
      col = hcl.colors(length(data), "Spectral"),
      labels = lab)



 
#Add the data for Revenue
Badakhshan=data.frame(value1=c(2, 3,0,19), 
                             Group1 = c("Taliban Control", "ANA Control", "ISIS Control", "Contested")) %>%
  mutate(Group = factor(Group1, levels = c("Taliban Control", "ANA Control", "ISIS Control", "Contested")),
         cumulative=cumsum(value1),
         midpoint = ((cumulative - value1)/2),
         label=paste0(Group1, " ",round(value1/sum(value1)*100,1),"%"))
#visualize pie chart of ISIS annual expenditures
p <- ggplot(data=Badakhshan) +
  geom_bar(aes(x="", y=value1, fill=Group1), stat="identity", width = 1)+
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_manual(values=c("darkgreen", "orange", "black","white")) +
  geom_text(aes(x=1, y = cumsum(value) - value/2, 
                label=label), position = position_dodge(width=0.9),  size=3)


#FF9900 #5C9147

p <- p + labs(title = "Badakshan Province Control",
              subtitle = "As of May 25th 2021",
              caption = "District total: 24 (2 Taliban, 3 ANA, 19 Contested)")
p <- p + theme(plot.background = element_rect(fill = "lightblue"))
p



Badakhshan <- data.frame( 
  state=c ( "afghan" ),
  Taliban = c(.0833) , 
  ANA = c(.125) , 
  ISIS = c(0.00) , 
  Contested = c(.7916)
)

Badakhshan2 <- melt( Badakhshan , id.var="state" )

threshold <- .07

Badakhshan2 <- Badakhshan2 %>%
  mutate(cs = rev(cumsum(rev(value))),
         ypos = value/2 + lead(cs, 1),
         ypos = ifelse(is.na(ypos), value/2, ypos),
         xpos = ifelse(value > threshold, 1, 1.3),
         xn = ifelse(value > threshold, 0, .5))

test <- ggplot( Badakhshan2 , aes( x=1 , y=value , fill=variable )) +
  geom_bar( width = 1 , stat = "identity" ) +
  geom_text_repel( aes( label = paste( variable , percent( value )), x = xpos, y = ypos ) , 
                   color="black" , size=5, nudge_x = Badakhshan2$xn, segment.size = .5 ) +
  coord_polar( "y" , start = 0 ) + 
  scale_fill_manual( values=c( "white", "darkgreen", "black", "orange" ) ) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())

test


test <- test + labs(title = "Badakshan Province Control",
              subtitle = "As of May 25th 2021",
              caption = "District total: 24 (2 Taliban, 3 ANA, 19 Contested)")
test <- test + theme(plot.background = element_rect(fill = "lightblue"))
test

slices <- c(2, 3, 0, 19)
lbls <- c("Taliban", "ANA", "ISIS", "Contested")
pie3D(slices,labels=lbls,explode=0.1, col=c("red", "darkgreen", "black","orange"),
      main="Control of Badakhshan Province, Afghanistan as of May 25th 2021 ")

df_wide <- dcast(df, province ~ species, value.var = "total")


df<-read.csv("Afghanistan Province Control.csv", header = TRUE, stringsAsFactors=FALSE)
df
xlim <- c(30, 37)
ylim <- c(61, 72)
xyz <- make.xyz(df$Longitude, df$Latitude, df$Taliban.Control, df$Total.Districts)
col <- c("red", "darkgreen", "black","orange")
tiff("pie-on-map.tiff", width = 8, height = 5.5, units = "in",
     res = 200, type = "cairo")
par(mai = c(0.5, 0.5, 0.35, 0.2), omi = c(0.25, 0.5, 0, 0),
    mgp = c(2.5, 0.5, 0), family = "Liberation Serif")


install.packages("plotly")
library(plotly)

p <- plot_ly(df, labels = ~ltr, values = ~wght, type = 'pie',textposition = 'outside',textinfo = 'label+percent') %>%
  layout(title = 'Letters',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

colors <- c('red', 'darkgreen', 'black', 'orange')

fig <- plot_ly(df, labels = ~ANA.Control, values = ~Taliban.Control, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = 'white'),
               hoverinfo = 'text',
               text = ~paste('Total Number of Districts Taliban Control in', State, 'Is', Taliban.Control, "Compared to", ANA.Control),
               marker = list(colors = colors,
                             line = list(color = 'black', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE)
fig <- fig %>% layout(title = 'Ratio of Province Control between Taliban and ANA',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig







fig2 <- plot_ly(df, labels = ~Total.Districts, values = ~ISIS.Control, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = 'white'),
               hoverinfo = 'text',
               text = ~paste('Number of Districts ISIS Control in', State, 'Is', ISIS.Control, "Out Of", Total.Districts),
               marker = list(colors = colors,
                             line = list(color = 'black', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE)
fig2 <- fig2 %>% layout(title = 'Taliban Control in Afghanistan By Province',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig2


afg<-getData('GADM', country='AFG', level=1) 
afg
coordinates(df)<-c("Longitude","Latitude")
proj4string(df)<-proj4string(afg)
plot(afg)



library(sp)
library(RColorBrewer)
install.packages("RColorBrewer")


heatMap <-function(data,shape=NULL,col="blue",main="Sample HeatMap"){
  # Plots a Heat Map of a Polygons Data Frame.  This will 
  # demonstrate density within a finite set of polygons
  #
  # Args:
  #   data:   Spatial Points dataframe
  #   shape:  Polygons Data Frame 
  #
  #
  #   Notes:  This function requires the sp and RColorBrewer
  #           Packages
  #
  #   Beskow: 03/28/11   
  #
  is.installed <- function(mypkg) is.element(mypkg, 
                                             installed.packages()[,1])
  if (is.installed(mypkg="sp")==FALSE)  {
    stop("sp package is not installed")}
  if (is.installed(mypkg="RColorBrewer")==FALSE)  {
    stop("RColorBrewer package is not installed")}
  if (!class(data)=="SpatialPointsDataFrame")  {
    stop("data argument is not SpatialPointsDataFrame")}
  require(sp)
  require(RColorBrewer)
  freq_table<-data.frame(tabulate(over(as(data,"SpatialPoints"),
                                       as(shape,"SpatialPolygons")),nbins=length(shape)))
  names(freq_table)<-"counts"
  
  shape1<-spChFIDs(shape,as.character(1:length(shape)))
  row.names(as(shape1,"data.frame"))
  spdf<-SpatialPolygonsDataFrame(shape1, freq_table, match.ID = TRUE)
  
  rw.colors<-colorRampPalette(c("white",col))
  spplot(spdf,scales = list(draw = TRUE),
         col.regions=rw.colors(max(freq_table)), main=main)
}





heatMap(df,Afghanistan,col="Red", main="Taliban Control in Each Province of Afghanistan")




geocode("West Point, NY")

plotPiePoly(data = df, 
            shape=afg, 
            factor = "flag", 
            size = c(0.5, 0.5), 
            legend = TRUE,
            main = "Sample Pieplot Over Polygon (WITS Data)")


nlcdcolors <- structure(c("white", "darkgreen", "black", "orange"))
statemap <- ggplot(df, aes(Longitude,Latitude,fill=dfClass)) +
  geom_tile() +
  borders('State', fill='beige') + coord_map() +
  scale_x_continuous(limits= c(61, 72), expand=c(0,0), name = 'Longitude') +
  scale_y_continuous(limits=c(30, 37), expand=c(0,0), name = 'Latitude') +
  scale_fill_manual(values = nlcdcolors, name = 'Afghanistan War Province Control make up')


xlim <- c(30, 37)
ylim <- c(61, 72)

pts.afghanistan = data.frame(df$Latitude,df$Longitude, freq.Taliban.Control, freq.b, col.a, col.b)
#Badakhshan
slices <- c(2, 3, 0, 19)
lbls <- c("Taliban Control", "ANA Control", "ISIS Control", "Contested")
pct <- round(slices/sum(slices) * 100)
lbls <- paste(lbls, pct)  # add percents to labels 
lbls <- paste(lbls, "%", sep = "")  # ad % to labels 
pie(slices, labels = lbls, col=c("red", "darkgreen", "black","orange"), main = "Badakhshan Province Control of Districts (Total 24)")

#
slices2 <- c(6, 0, 0, 7)
lbls <- c("Taliban Control", "ANA Control", "ISIS Control", "Contested")
pct <- round(slices2/sum(slices2) * 100)
lbls <- paste(lbls, pct)  # add percents to labels 
lbls <- paste(lbls, "%", sep = "")  # ad % to labels 
pie(slices2, labels = lbls, col=c("white", "darkgreen", "black","orange"), main = "Helmand Province Control of Districts (Total 13)")

install.packages("scatterpie")
library(scatterpie)
largest <- df %>%
  filter(Total.Districts > 1) %>%
  # Los Angeles metro changed CBSA from 31100 to 31080, hand correct the coordinate
  # mutate(lon = ifelse(GEOID == "31000US31080", -118.18194, lon),
  #        lat = ifelse(GEOID == "31000US31080", 34.10939, lat)) %>%
  select(State, Longitude, Latitude, total = Total.Districts, Taliban.Control, ANA.Control, ISIS.Control, Contested) %>%
  arrange(-total)

head(largest, 3)

afghanistan <- map_data("world", "Afghanistan")
ggplot(afghanistan, aes(long, lat)) +
  geom_map(map=afghanistan, aes(map_id=region), fill="grey97", color="grey") +
  geom_scatterpie(data = largest, 
                  aes(Longitude, Latitude, r = sqrt(total)/10),
                  cols = c("Taliban.Control", "ANA.Control", "ISIS.Control", "Contested"), 
                  alpha = 0.8) +
  scale_fill_manual(
    breaks = c("Taliban.Control", "ANA.Control", "ISIS.Control", "Contested"),
    labels = c("Taliban Controlled", "ANA Controlled", "ISIS Controlled", "Contested Province"),
    values = c("Taliban.Control" = "white",
               "ANA.Control" = "darkgreen",
               "ISIS.Control" = "black",
               "Contested" = "orange")
  ) +
  labs(title = "Afghanistan War Control of Territory By Province May 25th 2021",
       subtitle = "Contested would mean areas currently undergoing fighting or ANA being besieged",
       caption = "Source: FDD's Long War Journal",
       fill = NULL) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = c(0.96, 0.02),
        legend.justification = c(1, 0),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())





















