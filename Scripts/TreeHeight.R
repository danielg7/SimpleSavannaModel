library(ggplot2)
library(ggthemes)

myTheme <- theme_tufte() +
  theme(
    text = element_text(family="Arial",size=18),
    axis.line = element_line(size = .3)
  )


WideForm_Trees <- read.csv("/Users/danielgodwin/Dropbox/Graduate School/Dissertation/FireEscapeModel/Data/2014_KealaCummings_Height_Diameter_COAPTESECOMO.csv")

names(WideForm_Trees)[3] <- "Tree"

COAP_TreeHeight <- subset(WideForm_Trees,Species=="COAP")
TESE_TreeHeight <- subset(WideForm_Trees,Species=="TESE")
COMO_TreeHeight <- subset(WideForm_Trees,Species=="COMO")



# COAP Model ----
COAP_TreeHeight_agg <- aggregate(data=COAP_TreeHeight,Height ~ Diameter + Tree, base::mean)
COAP_ll <- lm(log(Height) ~ log(Diameter),data = COAP_TreeHeight_agg)
#COAP_mm_start <- SSmicmen(input=COAP_TreeHeight_agg$Diameter,Vm=max(COAP_TreeHeight_agg$Height),K=6)
#COAP_mm <- nls(Height ~ SSmicmen(input=COAP_TreeHeight_agg$Diameter,Vm=max(COAP_TreeHeight_agg$Height),K=6.3),data=COAP_TreeHeight_agg)
#COAP_nls <- nls(Height ~ SSasympOrig(Diameter, Asym,lrc),COAP_TreeHeight_agg)
coap_grid <- expand.grid(Diameter = seq(0,max(COAP_TreeHeight$Diameter),.1))
#coap_grid$Height <- exp(predict(COAP_ll,coap_grid))
coap_grid$Height <- exp(0.64 * log(coap_grid$Diameter) + 0.05)


# COAP Plot ----
coap_height <- ggplot(COAP_TreeHeight_agg,aes(x=Diameter,y=Height))
coap_height+
  myTheme+
  geom_point(data=coap_grid,color="red",aes(x=Diameter,y=Height))+
  geom_point()
# TESE Model ----
TESE_TreeHeight_agg <- aggregate(data=TESE_TreeHeight,Height ~ Diameter + Tree,base::mean)
#TESE_nls <- nls(Height ~ SSasympOrig(Diameter, Asym,lrc),TESE_TreeHeight_agg)
TESE_ll <- lm(log(Height) ~ log(Diameter),data = TESE_TreeHeight_agg)
#TESE_ll <- 
tese_grid <- expand.grid(Diameter = seq(0,max(TESE_TreeHeight$Diameter),.1))
#tese_grid$Height <- exp(predict(TESE_ll,tese_grid))
tese_grid$Height <- exp(0.63 * log(tese_grid$Diameter) - 0.02)
# TESE Plot ----
# tese_height <- ggplot(TESE_TreeHeight_agg,aes(x=Diameter,y=Height))
# tese_height+
#   myTheme+
#   geom_point(data=tese_grid,color="red",aes(x=Diameter,y=Height))+
#   geom_point()
# COMO Models ----
COMO_TreeHeight_agg <- aggregate(data=COMO_TreeHeight,Height ~ Diameter + Tree,FUN = base::mean)

#COMO_ll <- lm(log(Height) ~ log(Diameter),data = COMO_TreeHeight_agg)
COMO_nls <- nls(Height ~ SSasympOrig(Diameter, Asym,lrc),COMO_TreeHeight)
como_grid <- expand.grid(Diameter = seq(0,max(COMO_TreeHeight$Diameter),.1))
como_grid$Height <- predict(COMO_nls,como_grid)
# COMO Plot ----
como_height <- ggplot(COMO_TreeHeight,aes(x=Diameter,y=Height))
como_height+
  myTheme+
  geom_point(data=como_grid,color="red",aes(x=Diameter,y=Height))+
  geom_point()

# One Plot To Rule Them All ----
OnePlot <- ggplot(WideForm_Trees,aes(x=Diameter,y=Height,color=Species))
OnePlot+
  myTheme+
  geom_point()+
  scale_color_manual(values=c("green", "red", "blue"))+
  geom_line(data=como_grid,color="red",aes(x=Diameter,y=Height))+
  geom_line(data=tese_grid,color="blue",aes(x=Diameter,y=Height))+
  geom_line(data=coap_grid,color="green",aes(x=Diameter,y=Height))


