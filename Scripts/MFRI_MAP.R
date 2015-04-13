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
library(MASS)

cleanTheme <- ggthemes::theme_tufte() +
  ggplot2::theme(
    text = ggplot2::element_text(family="sans",size=15),
    axis.line = ggplot2::element_line(size = .5)
  )

if(file.exists("Data/krugerMAP_FRI_df.csv"))
{
  krugerMAP_FRI_df <- read.csv("Data/krugerMAP_FRI_df.csv")
}

if(!file.exists("Data/krugerMAP_FRI_df.csv"))
{
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
  
  write.csv(krugerMAP_FRI_df,file = "Data/krugerMAP_FRI_df.csv")
}

# Plot relationship ----



MFRI_MAP_binomial <- ggplot(data = krugerMAP_FRI_df, aes(x = MAP_mm, y = MFRI))+
  geom_point(alpha = .5)+
  ylim(0,10)+
  #ylab("Fire Frequency (Fires / yr)")+
  xlab("Mean Annual Precipitation (mm)")+
  ylab("Mean Fire Return Interval (yr)")+
  cleanTheme+
  stat_smooth(method="glm", family = "Gamma")
MFRI_MAP_binomial

# Model relationship ----
#krugerMAP_FRI_df <- subset(krugerMAP_FRI_df,MFRI <= 20)

glm_MFRI_null <- glm(data = krugerMAP_FRI_df, formula = MFRI ~ 1, family = Gamma(link = log))
glm_MFRI_MAP <- glm(data = krugerMAP_FRI_df, formula = MFRI ~ MAP_mm, family = Gamma(link = inverse))

# Model distribution of MFRI ----

mfridist_mean <- mean(krugerMAP_FRI_df$MFRI)
mfridist_var <- var(krugerMAP_FRI_df$MFRI)
mfridist_shape_est <- mfridist_mean^2 / mfridist_var
mfridist_scale_est <- mfridist_var / mfridist_mean 


mfridist_fitted <- fitdistr(krugerMAP_FRI_df$MFRI, dgamma,  start=list(shape = mfridist_shape_est, rate = 1/mfridist_scale_est),lower=0.001)

#mfridist_fitted <- bbmle::mle2(data = krugerMAP_FRI_df,MFRI ~ dgamma(shape, scale = scale),
#                         start = list( shape = mfridist_shape_est,
#                                      scale = mfridist_scale_est))


mfridist_shape <- mfridist_fitted$estimate[1]
mfridist_scale <- 1/mfridist_fitted$estimate[2]


gammaDistDF <- data.frame(var = rgamma(n = 1000000*length(krugerMAP_FRI_df),
                                       shape = mfridist_shape,
                                       scale = mfridist_scale))


MFRI_density <- ggplot(data = krugerMAP_FRI_df, aes(x = MFRI))+
  geom_density(fill = "blue", alpha = ".5")+
  xlab("MFRI")+
  geom_density(fill = "green", alpha = ".5",data = gammaDistDF, aes(x = var))+
  cleanTheme
MFRI_density


# Make sampling function -----

# Make this next one once again pull from the 95% CI of the modeled response

MFRI_from_MAP_mean <- function(MAP = numeric(0), Flat = FALSE){
  if(!is.numeric(MAP))stop("MAP must be numeric!")
  if(MAP < 400 || MAP > 950)stop("MAP must be constrained between 400 and 950!")
  if(!is.logical(Flat))stop("Flat must be TRUE or FALSE!")
  
  if(Flat == FALSE){
    predMFRI <- predict.glm(object = glm_MFRI_MAP, type = "link", newdata = list(MAP_mm = MAP))
    
    
    mean <- 1/predMFRI
    var <- summary(glm_MFRI_MAP)$dispersion * mean^2
    shape_est <- mean^2 / var
    scale_est <- var / mean 
    
    dist <- MASS::fitdistr(x = mean, densfun = dgamma,  start=list(shape = shape_est, rate = 1/scale_est), lower=c(0.01,0.01))
    rangeVals <- qgamma(p = seq(from = .05,to = .95,by = .01),shape = dist$estimate[1],scale = 1/dist$estimate[2])
    
  
    
    MFRI <- sample(x = rangeVals,size = length(MAP),replace = TRUE)
    
  }
  
  if(Flat == TRUE){
    predMFRI <- predict.glm(object = glm_MFRI_null, type = "link")
    
    
    mean <- exp(predMFRI)
    var <- summary(glm_MFRI_MAP)$dispersion * mean^2
    shape_est <- mean^2 / var
    scale_est <- var / mean 
    
    dist <- rgamma(n = 100000,shape = shape_est,scale = scale_est)
    
    MFRI <- sample(x = dist,size = length(MAP),replace = TRUE)
    
  }
  
  return(MFRI)
}

MFRI_from_MAP <- function(MAP = numeric(0), Flat = FALSE){
    if(!is.numeric(MAP))stop("MAP must be numeric!")
    if(MAP < 400 || MAP > 950)stop("MAP must be constrained between 400 and 950!")
    if(!is.logical(Flat))stop("Flat must be TRUE or FALSE!")
    
    if(Flat == FALSE){
      predMFRI <- predict.glm(object = glm_MFRI_MAP, type = "link", newdata = list(MAP_mm = MAP))
      
      
      mean <- exp(predMFRI)
      var <- summary(glm_MFRI_MAP)$dispersion * mean^2
      shape_est <- mean^2 / var
      scale_est <- var / mean 
      
      dist <- rgamma(n = 100000,shape = shape_est,scale = scale_est)
      
      MFRI <- sample(x = dist,size = length(MAP),replace = TRUE)
      
      MFRI <- exp(MFRI)
    }
    
    if(Flat == TRUE){
      predMFRI <- predict.glm(object = glm_MFRI_null, type = "link")
      
      
      mean <- exp(predMFRI)
      var <- summary(glm_MFRI_MAP)$dispersion * mean^2
      shape_est <- mean^2 / var
      scale_est <- var / mean 
      
      dist <- rgamma(n = 100000,shape = shape_est,scale = scale_est)
      
      MFRI <- sample(x = dist,size = length(MAP),replace = TRUE)
      
      MFRI <- exp(MFRI)
    }
    
    return(MFRI)
  }

sampleFromMFRI_Data <- function(n = 1, Flat = FALSE){
  if(!is.numeric(n))stop("n must be numeric!")
  if(!is.logical(Flat))stop("Flat must be TRUE or FALSE!")
  
  if(Flat == FALSE){
    rows <- sample(x = seq(from = 1, to = nrow(krugerMAP_FRI_df),by = 1), size = n, replace = TRUE)
    
    returnValues <- list(MFRI = krugerMAP_FRI_df[rows,]$MFRI, MAP_mm = krugerMAP_FRI_df[rows,]$MAP_mm)
  }
  
  if(Flat == TRUE){
    mfri_rows <- sample(x = seq(from = 1, to = nrow(krugerMAP_FRI_df),by = 1), size = n, replace = TRUE)
    map_rows <- sample(x = seq(from = 1, to = nrow(krugerMAP_FRI_df),by = 1), size = n, replace = TRUE)

    
    returnValues <- list(MFRI = krugerMAP_FRI_df[mfri_rows,]$MFRI, MAP_mm = krugerMAP_FRI_df[map_rows,]$MAP_mm)
    
  }
  
  return(returnValues)
}
