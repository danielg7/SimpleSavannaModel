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
# 
#   
# 
#
#        
# 
# Load required packages ----

# Establish function relating growth to MAP

height_max <- 600 / 100 # 600 cm as defined in Higgins et al. 2000 per Shackleton 1997

MAP_growth <- data.frame(MAP = c(300,600,1000,1400), Growth = c(35,45,60,80))
growthModel <- lm(Growth ~ MAP, data = MAP_growth) # Add sampling from CI for growth data.

COMO_growth <- structure(list(Growth = structure(c(0.0744476658720873, 0.063119002462375, 
                                                   0.0602859529635683, 0.0602859529635683, 0.0779867137032202, 0.0645353946455254
), gradient = structure(c(0.00327027162098659, 0.0027726360535244, 
                          0.00264818834561753, 0.00264818834561753, 0.00342573180300698, 
                          0.00283485408422401, 0.0743258009112551, 0.0630314184677961, 
                          0.0602060581278572, 0.0602060581278572, 0.0778529801214226, 0.0644438338705692
), .Dim = c(6L, 2L), .Dimnames = list(NULL, c("Asym", "lrc")))), 
MAP = c(564, 780, 654, 825, 504, 750)), .Names = c("Growth", 
                                                   "MAP"), row.names = c(NA, -6L), class = "data.frame")

COMO_growth$Growth <- COMO_growth$Growth*1000

COMO_growthModel <- lm(Growth ~ MAP, data = COMO_growth)

COMO_growth$Type <- "Negative Growth Samples"
MAP_growth$Type <- "Positive Growth Samples"

bothGrowth <- rbind(COMO_growth,MAP_growth)
bothGrowth$Type <- as.factor(bothGrowth$Type)

flatGrowth <- seq(from = min(bothGrowth$Growth), to = max(bothGrowth$Growth), by = .01)

treeGrowth <- function(height_previous = numeric(0), MAP = numeric(0), Type = "Positive")
{
  
  if(Type == "Positive"){
  growthRate <- predict.lm(object = growthModel, newdata = list(MAP = MAP),interval = "prediction")
  growthRate <- tidy(growthRate)
  
  growthRate$length <- length(height_previous)
  
  GR_sample <- sample(x = seq(from = growthRate$lwr,
                              to = growthRate$upr,
                              by = 0.01),
                      size = growthRate$length,
                      replace = TRUE)
  
  GR <- GR_sample
  }
  
  if(Type == "Negative"){
    growthRate <- predict.lm(object = COMO_growthModel, newdata = list(MAP = MAP))
    growthRate <- tidy(growthRate)
    
    growthRate$length <- length(height_previous)
    
    GR <- growthRate$x
  }
  
  if(Type == "Flat"){
    growthRate <- sample(x = flatGrowth,size = length(height_previous),replace = TRUE)
    
   # growthRate$length <- length(height_previous)
    
    GR <- growthRate
  }
  
  h <- height_previous + (1 + height_previous / height_max) * GR/100
  
  
  
  return(h)
}