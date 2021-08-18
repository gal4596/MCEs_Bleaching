rm(list=ls())
setwd("~/xxx/xxx")
Data = read.csv("Bleaching_surveys_Eilat_2010_2015_2018.csv", stringsAsFactors = TRUE)
Data$Year = as.factor(Data$Year)

library(readxl)
library(vegan)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggnewscale)
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

# Extract the variable scores and convert to a data.frame
species.scores <- as.data.frame(scores(sol, "species"))  
species.scores$species <- rownames(species.scores)  
head(species.scores)  

# Generate arrows between centroids
NMDS.mean <- aggregate(NMDS[,1:2], list(group = NMDS$Year), mean)
arrows = data.frame()
for (n in 1:(nrow(NMDS.mean)-1)) {
    xx = NMDS.mean[1+n,"x"]
    yy = NMDS.mean[1+n,"y"]
    year = paste(NMDS.mean[n,"group"],"-",NMDS.mean[1+n,"group"])
    tdata = data.frame(x = NMDS.mean[n,"x"], y = NMDS.mean[n,"y"], x2 = xx, y2 = yy,
                         years = year)
    arrows = rbind(arrows,tdata)
}
arrows

# Assign colors
colo = c("#868B8E", "#FF5C4D", "#955670")

# Plot NMDS
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
  geom_text(data = species.scores,aes(x = NMDS1,y = NMDS2, label = species), alpha = 0.7)+
  geom_segment(data = species.scores, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), alpha = 0.7, 
               size = 0.5, arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  new_scale("color") +
  theme_pubr()+
  theme(text=element_text(size = 12), legend.text = element_text(size = 10), 
        legend.title = element_text(size = 11), legend.position = "right")+
  guides(col = guide_legend(direction = "vertical"),
         fill = guide_legend(direction = "vertical"))
p_nmds 

ggsave("p_nmds.pdf", width = 7, height = 4.635)

# Global test 
perm <- how(nperm = 999)
permanova = adonis(newD ~ NMDS$Year,
                   data = NMDS, permutations = perm)
permanova

# pairwise based on years
library(RVAideMemoire)
pairwise.perm.manova(newD, NMDS$Year, nperm = 999, test = "Wilks")


##### Relative abundance (%) of morphotypes #####

library(reshape2)
library(Rmisc)
detach("package:dplyr", unload = TRUE)
library(dplyr)

datamorph = Data[,c(1,4:15)]
# sum(Data[2,4:23]) # just checking that the sum is 100%

datamorph2 = melt(datamorph)
datamorph2 = datamorph2 %>%
  rename(Morph = "variable", Cover = "value")

datamorph2$MorphG = NA
datamorph2$Status = NA

# Simplifications...
i=1
for (i in 1:nrow(datamorph2)) {
  D = datamorph2[i,]
  D$Morph = as.factor(as.character(D$Morph))
  if (D$Morph == "BlB") {
    datamorph2$MorphG[i] = "B"
    datamorph2$Status[i] = "Bleached"
  } else if (D$Morph == "BlB") { 
    datamorph2$MorphG[i] = "B"
    datamorph2$Status[i] = "Bleached"
  } else if (D$Morph == "BlE") {
    datamorph2$MorphG[i] = "E"
    datamorph2$Status[i] = "Bleached"
  } else if (D$Morph == "BleOC") { 
    datamorph2$MorphG[i] = "eOC"
    datamorph2$Status[i] = "Bleached"
  } else if (D$Morph == "BlMa") { 
    datamorph2$MorphG[i] = "M"
    datamorph2$Status[i] = "Bleached"
  } else if (D$Morph == "NBE") { 
    datamorph2$MorphG[i] = "E"
    datamorph2$Status[i] = "Non-Bleached"
  } else if (D$Morph == "NBlB") { 
    datamorph2$MorphG[i] = "B"
    datamorph2$Status[i] = "Non-Bleached"
  } else if (D$Morph == "NBlM") { 
    datamorph2$MorphG[i] = "M"
    datamorph2$Status[i] = "Non-Bleached"
  } else if (D$Morph == "NBlOC") { 
    datamorph2$MorphG[i] = "eOC"
    datamorph2$Status[i] = "Non-Bleached"
  } else if (D$Morph == "PBB") { 
    datamorph2$MorphG[i] = "B"
    datamorph2$Status[i] = "Partial-Bleached"
  } else if (D$Morph == "PBE") { 
    datamorph2$MorphG[i] = "E"
    datamorph2$Status[i] = "Partial-Bleached"
  } else if (D$Morph == "PBlOC") { 
    datamorph2$MorphG[i] = "eOC"
    datamorph2$Status[i] = "Partial-Bleached"
  } else { 
    datamorph2$MorphG[i] = "M"
    datamorph2$Status[i] = "Partial-Bleached"
  }
}

sum = summarySE(datamorph2, 
                measurevar= "Cover",  
                groupvars=c("Year","Status", "MorphG"), na.rm=TRUE)

lev = rev(levels(sum$Year))

sum = sum %>% 
  arrange(Status)

colo3 = c("#FF2511", "#68BBE3", "#FFB001")

# Plot stacked morphologies
p_stack = sum %>%
  mutate(Year = factor(Year, levels = rev(levels(sum$Year)), ordered = TRUE)) %>% # Updating the factor levels  
  ggplot(aes(fill = Status,
                      y = Cover, x = Year)) +
  geom_bar(stat="identity", position = "stack", color="black", alpha = 0.9) +
  labs(x = "Year", y = "Cover (%)") + 
  coord_flip()+
  facet_grid(MorphG ~ .) +
  theme_pubr(legend = "bottom")+
  scale_fill_manual(values = colo3)
p_stack

ggsave("p_stack.pdf", width = 4.33, height = 4.58)

##### Coral Bleaching vs non-Bleaching #####

datableach = melt(Data[,c(1,30:32)])
datableach2 = datableach %>% 
  rename(Status = "variable", Cover = "value")

head(datableach2)
sum2 = summarySE(datableach2, 
                measurevar= "Cover",  
                groupvars=c("Year","Status"), na.rm=TRUE)
sum2

sum3 = sum2 %>%
  group_by(Year) %>%
  summarise(cover = sum(Cover))

colo4 = c("black", "#FFB001", "#68BBE3", "#FF2511")
lab = c("Total cover", "Partial bleached", "Non-bleached", "Bleached")
show_col(colo4)

p_bleach <- ggplot() +
  geom_point(data = sum3, aes(y = cover, x = Year, color = "black")) +
  geom_line(data = sum3, aes(y = cover, x = factor(Year), group = 1)) +
  geom_point(data = sum2, aes(y = Cover, x = factor(Year), color = Status), size = 2) +
  geom_errorbar(data = sum2, aes(y = Cover, x = Year, ymin = Cover-se, ymax = Cover+se, color = Status), width = .2) +
  geom_line(data = sum2, aes(y = Cover, x = factor(Year), color = Status, group = Status)) +
  scale_y_continuous("Cover (%)", breaks = seq(0,100,5)) + 
  scale_color_manual(values = paste(colo4), labels = lab) + 
  theme_pubr(legend = "bottom")
p_bleach

ggsave("p_bleach.pdf", width = 5.5, height = 4)

#----- Radar chart -----

library(fmsb)
detach("package:dplyr", unload = TRUE)
library(dplyr)
library(tidyr)

# data <- as.data.frame(matrix(ncol = 13))

datamorph2 = melt(datamorph)
datamorph2 = datamorph2 %>%
  rename(Morph = "variable", Cover = "value")
head(datamorph2)

# mean coral cover per year
Data %>%
  group_by(Year) %>%
  summarise(cover = mean(Total_coral))

Df1 = datamorph2 %>% 
  group_by(Year, Morph) %>%
  summarise(cover = mean(Cover))
Df2 = spread(Df1, Morph, cover)
Df2 = as.data.frame(Df2)
rownames(Df2) = Df2$Year
Df2$Year = NULL

data <- rbind(rep(20, ncol(Df2)), rep(0, ncol(Df2)), Df2[c(1:3),])

colors_fill = c(rgb(134,139,142,130, max = 255), rgb(255,92,112,130, max = 255), rgb(149,86,112,130, max = 255))
colo = c("#868B8E", "#FF5C4D", "#955670")

radarp = radarchart(data, axistype = 1,
                   # custom grid
                   cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0,20,5), cglwd = 0.8,
                   
                   # custom polygon
                   pcol = colo, plwd=4 , plty=1, pfcol = colors_fill
)
legend(x=1.3, y=0.6, legend = c("2010 (42.1%)","2015 (29.9%)", "2018 (22.7%)"), bty = "n", 
       pch = 20 , col=colo , text.col = "black", cex=1, pt.cex=2)

ggsave("radarp.pdf", width = 6.5, height = 5)

#----- Statistics -----

library(RVAideMemoire)
library(predictmeans)
library(lme4)

pm <- lm(Cover ~ Year*Morph, data = datamorph2)

# Statistical models
anova(pm) # no permutation
permmodels(pm, data = datamorph2, nsim = 999) # with permutation

# Pairwise 
datableach2$group = paste(datableach2$Year, datableach2$Status)
pt = pairwise.perm.t.test(datableach2$Cover, datableach2$group, 
                     nperm = 999, p.method = "fdr")
ptt2 = pt$p.value
View(ptt2)
