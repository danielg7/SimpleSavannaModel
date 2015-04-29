library(ggplot2)
library(ggthemes)
library(gridExtra)
library(plyr)

meta <- read.csv("Data/metaanalysis.csv")
hwange <- read.delim("Data/Hwange_growth.txt",skip=1)
hwange_summary <- ddply(hwange,.(Species),summarize,Average_Diameter_Increment = mean(Increment_cm), ADI_StanDev = sd(Increment_cm))
levels(hwange_summary$Species) <- c("Burkea africana","Combretum sp.","Terminalia sericea")
hwange_summary$MAP <- 650
hwange_summary$Site <- "Hwange NP, ZM"
hwange_summary$Citation <- "Holdo"

levels(meta$Species)[2] <- "Combretum sp."

meta <- rbind.fill(meta,hwange_summary)
meta <- subset(meta,Species != "Burkea africana")

toDisplay <- meta[c("Species","Site","MAP","Average_Diameter_Increment","Citation")]
names(toDisplay)[4] <- "Average_Diameter_Increment_cm"

metaPlot <- ggplot(data = meta, aes(x = MAP, y = Average_Diameter_Increment, shape = Species, group = Species, factor = Species))+
  myTheme+
  geom_point()+
  theme(legend.position="none")+
  geom_errorbar(aes(x = MAP, ymax = Average_Diameter_Increment + ADI_StanDev, ymin = Average_Diameter_Increment - ADI_StanDev))+
  ylab("Average Diameter Increment Per Site (cm)")+
  xlab("Mean Annual Precipitation (mm)")+
  stat_smooth(method = "lm",se = FALSE)+
  ylim(0,1)+
  facet_wrap(~Species,scales = "free_y")

toDisplay$Average_Diameter_Increment_cm <- round(x = toDisplay$Average_Diameter_Increment_cm,digits = 3)

metaTable <- tableGrob(toDisplay)
grid.arrange(metaPlot,metaTable,nrow=2)

for(m in unique(meta$Species)){
  l <- subset(meta,Species == m)
  print(m)
  print(summary(lm(Average_Diameter_Increment ~ MAP, l)))
}

COMO_subset <- subset(meta,Species=="Colophospermum mopane")
COMO_subset$Height <- predict(object = COMO_nls,newdata = list(Diameter = COMO_subset$Average_Diameter_Increment))
COMO_growth <- data.frame(Growth = COMO_subset$Height,MAP=COMO_subset$MAP)
