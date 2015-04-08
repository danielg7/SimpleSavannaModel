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




