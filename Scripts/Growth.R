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

treeGrowth <- function(height_previous = numeric(0), MAP = numeric(0), Flat = FALSE)
{
  
  
  growthRate <- predict.lm(object = growthModel, newdata = list(MAP = MAP),interval = "confidence")
  growthRate <- tidy(growthRate)
  
  growthRate$length <- length(height_previous)
  
  GR <- growthRate$fit
  
  h <- height_previous + (1 + height_previous / height_max) * GR/100
  
  
  
  return(h)
}