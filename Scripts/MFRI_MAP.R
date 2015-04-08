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
# Data/FRI/fire_return_interval_1941to2006.shp
# Data/krugerMAP_UTM.grid
# 
# 
# Outputs: ----
# krugerMAPFRI_df
# 
#
# TODO:  ----
# Functionalize where possible.
# Annotate and clean.      
# Run through tidying tool.       
#
#        
# 
# Load required packages ----

library(raster)
library(rgdal)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(broom)
library(bbmle)
library(plyr)

cleanTheme <- ggthemes::theme_tufte() +
  ggplot2::theme(
    text = ggplot2::element_text(family="sans",size=15),
    axis.line = ggplot2::element_line(size = .5)
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

krugerMAP_FRI_df <- na.omit(krugerMAP_FRI_df)

krugerMAP_FRI_df$FF <- 1/krugerMAP_FRI_df$MFRI


# Plot relationship ----

MFRI_MAP_binomial <- ggplot(data = krugerMAP_FRI_df, aes(x = MAP_mm, y = MFRI))+
  geom_point(alpha = .5)+
  #ylab("Fire Frequency (Fires / yr)")+
  xlab("Mean Annual Precipitation (mm)")+
  ylab("Mean Fire Return Interval (yr)")+
  cleanTheme
MFRI_MAP_binomial


# Model relationship ----

glm_MFRI_null <- glm(data = krugerMAP_FRI_df, formula = MFRI ~ 1, family = Gamma(link = log))
glm_MFRI_MAP <- glm(data = krugerMAP_FRI_df, formula = MFRI ~ MAP_mm, family = Gamma(link = log))

# Model distribution of MFRI ----

mfridist_mean <- mean(krugerMAP_FRI_df$MFRI)
mfridist_var <- var(krugerMAP_FRI_df$MFRI)
mfridist_shape_est <- mfridist_mean^2 / mfridist_var
mfridist_scale_est <- mfridist_var / mfridist_mean 

#mfridist_fitted <- MASS::fitdistr(x = krugerMAP_FRI_df$MFRI,
#                         densfun = "gamma",
#                         start = list( shape = mfridist_shape_est,
#                                       scale = mfridist_scale_est))


mfridist_fitted <- bbmle::mle2(data = krugerMAP_FRI_df,MFRI ~ dgamma(shape, scale = scale),
                         start = list( shape = mfridist_shape_est,
                                       scale = mfridist_scale_est))


mfridist_shape <- mfridist_fitted@coef[1]
mfridist_scale <- mfridist_fitted@coef[2]


gammaDistDF <- data.frame(var = rgamma(n = length(krugerMAP_FRI_df),
                                       shape = mfridist_shape,
                                       scale = mfridist_scale))


MFRI_density <- ggplot(data = krugerMAP_FRI_df, aes(x = MFRI))+
  geom_density(fill = "blue", alpha = ".5")+
  xlab("MFRI")+
  geom_density(fill = "green", alpha = ".5",data = gammaDistDF, aes(x = var))+
  cleanTheme
MFRI_density

# Predict relationship of MFRI ~ MAP ----

MAP_FF_modelCompare <- krugerMAP_FRI_df

MAP_FF_modelCompare$predicted <- exp(predict(object = glm_MFRI_MAP, newdata = MAP_FF_modelCompare))

MAP_FF_predicted <- predict.glm(glm_MFRI_MAP, newdata = MAP_FF_modelCompare, se.fit=TRUE, type="link", interval="prediction")

MAP_FF_predicted <- tidy(MAP_FF_predicted)
MAP_FF_predicted <- exp(MAP_FF_predicted)

upper <- qgamma(p = .95, shape = mfridist_shape,scale = mfridist_scale)
lower <- qgamma(p = .05, shape = mfridist_shape,scale = mfridist_scale)

MAP_FF_modelCompare$usd <- MAP_FF_predicted$fit + upper * MAP_FF_predicted$se.fit
MAP_FF_modelCompare$lsd <- MAP_FF_predicted$fit - lower * MAP_FF_predicted$se.fit

# Make sampling function -----

MFRI_from_MAP <- function(MAP = numeric(0)){
  if(!is.numeric(MAP))stop("MAP must be numeric!")
  if(MAP < 400 || MAP > 950)stop("MAP must be constrained between 400 and 950!")
  
  predicted <- predict.glm(glm_MFRI_MAP, newdata = list(MAP_mm = MAP), se.fit=TRUE, type="link", interval="prediction")
  
  upper <- qgamma(p = .95, shape = mfridist_shape,scale = mfridist_shape)
  lower <- qgamma(p = .05, shape = mfridist_shape,scale = mfridist_shape)
  
  
  usd <- predicted$fit + upper * predicted$se.fit
  lsd <- predicted$fit - lower * predicted$se.fit
  
  results <- list(MAP = MAP, predicted = predicted$fit, usd = usd, lsd = lsd)
  
  return(results)
}

# Make df of potential values ---- 

MAP_range <- seq(400,950,1)
MFRI_estimates <- as.data.frame(MFRI_from_MAP(MAP_range))
MFRI_expand <- ddply(.data = MFRI_estimates, .(MAP), summarize, MFRI_estimates = seq(lsd,usd,by = .01))

