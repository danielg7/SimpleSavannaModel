# Copyright notice: ----
# This script is provided with a Creative Commons - Attribution license, as defined on:
# http://creativecommons.org/licenses/by/3.0/us/
#
#
# Author Contact:
# Daniel Godwin
# danielg7@gmail.com
# Savanna Ecology Lab
# Division of Biological Sciences
# University of Missouri - Columbia
#
# Script Intent: ---
# This script models fire frequency / MFRI as a f(mean rainfall)
#
# Completeness: Incomplete
#
# Inputs: ----
# 
#
#
# Outputs: ----
# 
# 
#
# TODO:  ----
# Calculate CI with dgamma       
#        
#
#        
# 
# Load required packages ----

library(raster)
library(rgdal)
library(ggplot2)
library(ggthemes)
library(reshape2)

cleanTheme <- ggthemes::theme_tufte() +
  ggplot2::theme(
    text = ggplot2::element_text(family="sans",size=15),
    axis.line = ggplot2::element_line(size = 1)
  )

# Load data ----

krugerFRI <- readOGR(dsn="Data/FRI/",layer = "fire return interval_1941to2006")
krugerMAP_UTM <- raster(x="Data/krugerMAP_UTM")

# Transform data ----

crs.k <- CRS("+proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
krugerFRI_UTM <- spTransform(krugerFRI,crs.k)

krugerFRIRaster <- raster(krugerMAP_UTM)

krugerFRIRaster <- rasterize(x = krugerFRI_UTM,
                             y = krugerFRIRaster,
                             field=krugerFRI_UTM$MFRI)

krugerMAP_FRI <- brick(krugerFRIRaster,krugerMAP_UTM)

# Clean and output data ----

names(krugerMAP_FRI) <- c("MFRI","MAP_mm")

krugerMAP_FRI_df <- as.data.frame(krugerMAP_FRI)

rm(krugerMAP_FRI,krugerFRIRaster,krugerFRI_UTM,crs.k,krugerMAP_UTM,krugerFRI)

# Plot relationship ----

MFRI_MAP_binomial <- ggplot(data = krugerMAP_FRI_df, aes(x = MAP_mm, y = 1/MFRI))+
  geom_point(alpha = .5)+
  ylab("Fire Frequency (Fires / yr)")+
  xlab("Mean Annual Precipitation (mm)")+
  cleanTheme
MFRI_MAP_binomial

MFRI_density <- ggplot(data = krugerMAP_FRI_df, aes(x = 1/MFRI))+
  geom_density()+
  xlab("Fire Frequency")+
  cleanTheme
MFRI_density


# Model relationship ----

glm_MFRI_null <- glm(data = krugerMAP_FRI_df, formula = 1/MFRI ~ 1, family = Gamma(link = log))
glm_MFRI_MAP <- glm(data = krugerMAP_FRI_df, formula = 1/MFRI ~ MAP_mm, family = Gamma(link = log))

#fitted <- MASS::fitdistr(1/(krugerMAP_FRI_df$MFRI),dgamma,start=list(mu=mean(1/(krugerMAP_FRI_df$MFRI)),phi=sd(1/(krugerMAP_FRI_df$MFRI))/sqrt(length(1/(krugerMAP_FRI_df$MFRI)))))

modelCompare <- krugerMAP_FRI_df

modelCompare$predicted <- exp(predict(object = glm_MFRI_MAP, newdata = modelCompare))

pred <- predict.glm(glm_MFRI_MAP, newdata = modelCompare, se.fit=TRUE, type="link", interval="prediction")

pred <- tidy(pred)
#pred$se.fit <- exp( pred$se.fit)

modelCompare$usd <- exp(pred$fit + pred$se.fit)
modelCompare$lsd <- exp(pred$fit - pred$se.fit)


modelCompare_melt <- melt(modelCompare,measure.vars = c("predicted","MFRI"))
names(modelCompare_melt) <- c("MAP_mm","Model","Value")

modelCompare_binom <- ggplot(data = modelCompare, aes(x = MAP_mm, y = 1/MFRI))+
  geom_jitter(alpha = .15, color = "gray25")+
  geom_line(color = "red", aes(x = MAP_mm, y = modelCompare$predicted))+
  geom_line(linetype = "dotted", aes(x = MAP_mm, y = modelCompare$usd))+
  geom_line(linetype = "dotted", aes(x = MAP_mm, y = modelCompare$lsd))+
 # geom_smooth(method = "glm", family="Gamma", formula = y ~ x, fill = "gray85", size = 2)+
  ylab("Fire Frequency")+
  xlab("Mean Annual Precipitation (mm)")+
  cleanTheme
modelCompare_binom

modelCompare_plot <- ggplot(data = modelCompare, aes(x = MFRI, y = 1/predicted))+
  geom_point()+
  
  ylab("Predicted")+
  xlab("Real")+
  cleanTheme
modelCompare_plot