rm(list=ls())
setwd("~/Desktop/nMDS_bleaching")
Data = read.csv("Bleaching_surveys_Eilat_2010_2015_2018.csv", stringsAsFactors = TRUE)
Data$Year = as.factor(Data$Year)

library(readxl)
library(vegan)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(reshape2)

##### NMDS #####

newD = as.data.frame.matrix(Data[,c(16:21,25)]) # Benthic data
rownames(newD) = paste(Data$Year, Data$Name)

sol <- metaMDS(decostand(newD, "hellinger"), distance = "bray", k = 2, trymax=50)
stressplot(sol)

NMDS = data.frame(x=sol$point[,1],y=sol$point[,2], 
                  Year = as.factor(Data$Year), Group  = as.integer(Data$Group))
head(NMDS)

# Extracting hull points 
find_hull <- function(df) df[chull(df$x, df$y), ]
library(plyr)
hulls <- ddply(NMDS, "Year", find_hull)

colo = c("#868B8E", "#FF5C4D", "#955670")
colo2 = c("#FF5C4D", "#955670")
show_col(colo)

# Generate arrows between points
arrows = data.frame()
for (i in unique(NMDS$Group)) {
  df = NMDS %>% filter(Group == i)
  df$Year = as.numeric(as.character(df$Year))
  df = df[order(df$Year),]
  arrows2 = data.frame()
  for (n in 1:(nrow(df)-1)) {
    xx = df[1+n,"x"]
    yy = df[1+n,"y"]
    year = paste(df[n,"Year"],"-",df[1+n,"Year"])
    tdata = data.frame(x = df[n,"x"], y = df[n,"y"], x2 = xx, y2 = yy,
                         years = year, group = unique(df$Group))
    arrows2 = rbind(arrows2,tdata)
  }
  arrows = rbind(arrows,arrows2)
}
arrows$years = as.factor(arrows$years)

p_nmds = 
  ggplot(data = NMDS) +
  geom_point(aes(x, y, color = Year, size = Data$Total_coral), alpha=0.8) +
  labs(x = "NMDS1", y = "NMDS2", subtitle = paste("Stress = ", round(sol$stress,2))) +
  geom_polygon(data = hulls,          
               aes(x = x, y = y, fill = Year, color = Year),      
               alpha = 0.7, size = 1) +
  scale_color_manual("Year", values = colo) +
  scale_fill_manual("Year", values = colo) +
  scale_size(range = c(1, 10), name = "Coral cover (%)") +
  ggnewscale::new_scale("color") +
  geom_segment(data = arrows, aes(x = x, y = y, xend = x2, yend = y2, color = years),
  size = 0.5, arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  scale_color_manual("", values = colo2) +
  theme_pubr()+
  theme(text=element_text(size = 12), legend.text = element_text(size = 10), 
        legend.title = element_text(size = 11), legend.position = "right")+
  guides(col = guide_legend(direction = "vertical"),
         fill = guide_legend(direction = "vertical"))
p_nmds 
