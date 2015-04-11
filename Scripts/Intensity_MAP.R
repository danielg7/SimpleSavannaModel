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

library(boot)

# Define functions -----

Govender2006_fuelLoad <- function(TimeSinceFire = numeric(0), previousTwoYearRain_mm = numeric(0)){
  FL <- 382.9 + 3.3 * previousTwoYearRain_mm + 979.4 * TimeSinceFire - 0.001 * TimeSinceFire^2 + 0.37*previousTwoYearRain_mm*TimeSinceFire - 161.8*TimeSinceFire^2
  return(FL)
}

Trollope2002_firelineIntensity <- function(FuelLoad = numeric(0), FuelMoisture = numeric(0), RelativeHumidity = numeric(0), WindSpeed = numeric(0))
{
  FI <- 2729 + 0.8684*FuelLoad - 530*sqrt(FuelMoisture) - 0.1907*RelativeHumidity^2 - 596 * 1/WindSpeed
  return(FI)
}

Fxn_FireIntensity_Original <- function(MAP = numeric(0)){
  FI <- 4.13 * MAP - 558.22 # Relationship between MAP and mean fire intensity, Govender et al. 2006
  return(FI)
}

# Historical EBP model ----

historicalEBP <- read.csv("Data/HistoricalEBP.csv")

names(historicalEBP) <- c("ID","Date","FireYear","Location","Landscape","Soil","Rainfall_mm","Season","Replicate","Frequency","PlotNumber","FMC","FuelLoad_kgha","AirTemp","RH","Wind_ms","Plot_ROS","Media_FuelLoad_kgm2","Heat_Yield","PlotFI","PercentPlotBurnt","TypeOfBurn","SuccessOfFire")

# Clean EBP data ----

historicalEBP <- subset(historicalEBP,RH >= 0)
historicalEBP <- subset(historicalEBP,FMC >= 0)
historicalEBP <- subset(historicalEBP,PlotFI > 0)
historicalEBP <- subset(historicalEBP,Wind_ms >= 0 & Wind_ms <= 20)


EBP_Intensity_by_Flat <- glm(PlotFI ~ 1, data = historicalEBP, family = Gamma(link = identity))
EBP_Intensity_by_MAP <- glm(PlotFI ~ Rainfall_mm, data = historicalEBP, family = Gamma(link = identity))



Fxn_FireIntensity_RedoneHistoricalEBP <- function(MAP = numeric(0), Flat = FALSE){
  
  if(Flat == FALSE){
  predFI <- predict.glm(object = EBP_Intensity_by_MAP, type = "link", newdata = list(Rainfall_mm = MAP))
  
  
  mean <- predFI
  var <- summary(EBP_Intensity_by_MAP)$dispersion * mean^2
  shape_est <- mean^2 / var
  scale_est <- var / mean 
  
  dist <- rgamma(n = 1000,shape = shape_est,scale = scale_est)
  
  FI <- sample(x = dist,size = length(MAP),replace = TRUE)
  }
  
  if(Flat == TRUE){
  predFI <- predict.glm(object = EBP_Intensity_by_Flat, type = "link")

  mean <- predFI
  var <- summary(EBP_Intensity_by_Flat)$dispersion * mean^2
  shape_est <- mean^2 / var
  scale_est <- var / mean 
  
  dist <- rgamma(n = 1000,shape = shape_est,scale = scale_est)
  
  FI <- sample(x = dist,size = length(MAP),replace = TRUE)
  }
  return(FI)
}

