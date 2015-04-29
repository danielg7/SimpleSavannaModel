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
# This model integrates the other models (Growth.R, Intensity_MAP.R, MFRI_MAP.R)
# into a coherent model.
#
# Completeness: Incomplete
#
# Inputs: ----
# Growth.R
# Intensity_MAP.R
# MFRI_MAP.R
# 
# Outputs: ----
# .csvs of data
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

Fxn_IntegrativeSavannaTopkillModel <- function(Runs_Set = 1000, Trees = 100, TL = 50, GrowthFlat = "Positive", IntensityFlat = FALSE, FireFrequencyFlat = FALSE, FileName = NA)
{
  pb <- txtProgressBar(min = 0, max = Runs_Set, style = 3)
  
  source("Scripts/Growth.R")
  source("Scripts/Intensity_MAP.R")
  source("Scripts/MFRI_MAP.R")

# Create empty dataframes ----

treeHeightList <- numeric(0)
fireIntensityList <- numeric(0)
treeHeightsByMAP <- list(0)
intensityByMAP <- list(0)

# Define start conditions ----

timeLimit <- TL
treeNumbers <- Trees
Runs <- Runs_Set
MAP_previous <- 0

GrowthFlat <- GrowthFlat
IntensityFlat <- IntensityFlat
FireFrequencyFlat <- FireFrequencyFlat

# Write model metadata:
simTime <- Sys.time()

simMetadata <- data.frame(simTime,
                          timeLimit,
                          treeNumbers,
                          Runs,
                          GrowthFlat,
                          IntensityFlat,
                          FireFrequencyFlat)


  
for(u in 1:Runs){
  
  sampled_MAPMFRI <- sampleFromMFRI_Data(n = 1, Flat = FireFrequencyFlat)
  MFRI <- sampled_MAPMFRI$MFRI
  MAP_seed <- sampled_MAPMFRI$MAP
  FireFrequency <- 1/MFRI
  
  FireFrequency
  if(FireFrequency > 1) {FireFrequency <- 1}
  
  
  
  
  
  Tree_vector_height <- rep(0,treeNumbers)
  
  
  for(timeSkip in 1:timeLimit){
    
    
    Tree_vector_height <- treeGrowth(height_previous = Tree_vector_height, MAP = MAP_seed,Type = GrowthFlat)
    
    
    burnTest <- rbinom(n = 1,
                       size = 1,
                       prob = FireFrequency) 
    
    if(burnTest == 1)
    {
      
      FireIntensity <- Fxn_FireIntensity_RedoneHistoricalEBP(MAP = MAP_seed,Flat=IntensityFlat) 
      
      
      fireIntensityList <- append(fireIntensityList,FireIntensity)
      
      Tree_vector_topkills <- rbinom(n=length(Tree_vector_height),size=1,prob=(1 - (inv.logit(-3.9 * log(Tree_vector_height) + .05 * sqrt(FireIntensity) + .3 * 1)))) # Higgins et al. 2012
      Tree_vector_height <- Tree_vector_height * Tree_vector_topkills
      Tree_vector_height <- sort(Tree_vector_height)
      
      
    }
  }
  
  treeHeightList <- append(treeHeightList,Tree_vector_height)
  
  MAP_for_list <- as.character(MAP_seed)
  
  treeHeightsByMAP[[MAP_for_list]] <-treeHeightList
  intensityByMAP[[MAP_for_list]] <-fireIntensityList
  
  treeHeightList <- numeric(0)
  
  setTxtProgressBar(pb, u)
  
}

treeHeightsByMAP_df <- as.data.frame(treeHeightsByMAP)
treeHeightsByMAP_df$X0 <- NULL

treeHeightsByMAP_df_long <- melt(treeHeightsByMAP_df,variable_name = "MAP")
names(treeHeightsByMAP_df_long) <- c("MAP","Height")
treeHeightsByMAP_df_long$MAP <- as.numeric(as.character(substring(treeHeightsByMAP_df_long$MAP, 2)))

if(is.na(FileName)){FileName <- paste(simTime,"_treeHeightsByMAP",".csv",sep="")}

dataLoc <- file.path(paste("Runs/",FileName,".csv",sep = ""))
metadataLoc <- file.path("Runs/",paste(FileName,"_metadata",".csv",sep = ""))
write.csv(x = treeHeightsByMAP_df_long, file=dataLoc)
write.csv(x = simMetadata, file=metadataLoc)

return(treeHeightsByMAP_df_long)
}