cost <-read.csv("cost.csv", header = TRUE, stringsAsFactors = FALSE)
library(ggplot2)
install.packages("png")
library(png)
library(grid)
library(ggimage)
install.packages("ggimages")
install.packages("https://cran.r-project.org/src/contrib/Archive/ggimage/ggimage_0.2.7.tar.gz", repos = NULL, type="source")
install.packages("ggplotify")
install.packages("magick")
install.packages("RCurl")
library(RCurl)
library(ggrepel)

fire <- readPNG("fire3.png")

cost$image <- "money.png"

URL1 = "https://www.freeiconspng.com/uploads/displaying-18-images-for-save-money-icon-png--16.png"
money = readPNG(getURLContent(URL1))


cost <- data.frame(category = c("DOD", "counternarcotics", "Afghan Army", "economic development", "Reconstruction", "Interest", "Veteran Care"),
                   spent = c(1500, 10, 87, 24, 30, 500, 1400),
                   image=rep(c("https://www.freeiconspng.com/uploads/displaying-18-images-for-save-money-icon-png--16.png"), each=7))
ggplot(data = cost, aes(x = category, y = spent)) +
  geom_point()



ggplot(data = cost, aes(x = category, y = spent)) +
  geom_text(aes(label = spent),
            size = 3.5) +
  annotation_custom(rasterGrob(fire, 
                               width = unit(1,"npc"),
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_image(aes(image = image), size = 0.13) +
  xlab("spent where?") +
  ylab("Cost in billions $USD") +
  ylim(0, 2000) +
  geom_label_repel(aes(category, spent,  label = sprintf('%0.1f', spent)),
                   show.legend = FALSE,
                   color = c(rep('grey50')),
                   fill = c( rep('lightblue'))) +
  ggtitle("Over 2 Trillion Dollars Were Spent on The War in Afghanistan. Where did it Go?")

ggplot(data = cost, aes(x = category, y = spent)) +
  +geom_text(aes(label=Name),hjust=0, vjust=0) +
  geom_image(aes(image = image), size = 0.10) +
  geom_point() +
  xlab("where money was allocated") +
  ylab("Cost in Billions $USD") +
  ylim(0, 2000) +
  ggtitle("Over 2 Trillion Dollars Were Spent on The War in Afghanistan. Where did it Go?")

cost

ggplot(cost, aes(x = category, y=spent)) + 
  geom_image(aes(image=image, size=.15)) +
  theme_bw() +
  scale_size_manual(values=c(0.1,0.15)) +
  coord_fixed(ratio=2/3, ylim=c(0,2000))

cost + geom_label_repel(aes(category, spent,  label = sprintf('%0.1f%%', spent)),
                 show.legend = FALSE,
                 color = c(rep(NA,21), rep('grey50',4)),
                 fill = c(rep(NA,21), rep('lightblue',4)))




install.packages("ggpubr")
library(ggpubr)

df1=read.csv("Military Spending in Vietnam and War on Terror.csv", dec='.', header=TRUE, stringsAsFactors = FALSE)
df1


ggdotchart(df1, x = "Money.Spent", y = "Year",
           color = "War",                                # Color by groups
           palette = c("#5C9147", "#3A5795"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = "War",                                # Order by groups
           dot.size = 6,                                 # Large dot size
           label = round(df1$Money.Spent),                        # Add mpg values as dot labels
           font.label = list(color = "white", size = 9, 
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
)
p
p + ylim(0,1000)
p + xlim(1963,2019)

p<-ggdotchart(df1, x = "Year", y = "Money.Spent",
           color = "War",                                # Color by groups
           palette = c("#5C9147", "#3A5795"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           rotate = TRUE,                                # Rotate vertically
           dot.size = 2,                                 # Large dot size
           y.text.col = TRUE,                            # Color y text by groups
           ggtheme = theme_pubr()                        # ggplot2 theme
)+
  theme_cleveland()   

p + ggtitle("Defense Spending in Billions $USD each Year in Vietnam and Afghanistan/Iraq")



rgb(.36,.57,.28)
rgb(58,87,149, max=255)






