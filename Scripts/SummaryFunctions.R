
Summary_TreeHeights <- function(df,MAP_low,MAP_high,MAP_sequence)
{
  
  df$Height_Cuts <- cut(x = df$Height,
                                              breaks = seq(0,
                                                           ceiling(max(df$Height)),
                                                           1),
                                              labels = FALSE)
  
  df$MAP_Cuts <- cut(x = df$MAP,
                                           right = TRUE,
                                           breaks = seq(MAP_low,
                                                        MAP_high,
                                                        MAP_sequence))
  levels(df$MAP_Cuts) <- seq(MAP_low,MAP_high,MAP_sequence)
  df$MAP_Cuts <- as.character(df$MAP_Cuts)
  df$MAP_Cuts <- as.numeric(df$MAP_Cuts)
  
  
  df_summary <- ddply(.data = df,.(MAP_Cuts,Height_Cuts),summarize,Count = length(Height_Cuts))
  df_summary <- ddply(.data = df_summary,.(MAP_Cuts),mutate,Proportion = Count / sum(Count))
  
  return(df_summary)
}

Summary_pEscape <- function(df, MAP_low, MAP_high, MAP_sequence){
  EscapeHeight <- 3 # Set the escape height in meters
  
  df$MAP_Cuts <- cut(x = df$MAP,
                     right = TRUE,
                     breaks = seq(MAP_low,
                                  MAP_high,
                                  MAP_sequence))
  levels(df$MAP_Cuts) <- seq(MAP_low,MAP_high,MAP_sequence)
  df$MAP_Cuts <- as.character(df$MAP_Cuts)
  df$MAP_Cuts <- as.numeric(df$MAP_Cuts)
  
  pEscape <- ddply(.data = df,
                   .(MAP_Cuts),
                   summarise,
                   pResult = length(which(Height > EscapeHeight)) / length(Height)
  )
  
  pEscape$MAP <- as.numeric(as.character(pEscape$MAP))
  
  pEscape <- na.omit(pEscape)
  
  return(pEscape)
}

# 
# intensityByMAP_df <- as.data.frame(melt(intensityByMAP))
# intensityByMAP_df <- intensityByMAP_df[-1,]
# names(intensityByMAP_df) <- c("intensity","MAP")
# intensityByMAP_df$MAP <- as.numeric(intensityByMAP_df$MAP)
# 
# intensityByMAP_df$MAP_Cuts <- cut(x = intensityByMAP_df$MAP,
#                                   right = TRUE,
#                                   breaks = seq(400,
#                                                900,
#                                                50))
# levels(intensityByMAP_df$MAP_Cuts) <- seq(400,900,50)
# intensityByMAP_df$MAP_Cuts <- as.character(intensityByMAP_df$MAP_Cuts)
# intensityByMAP_df$MAP_Cuts <- as.numeric(intensityByMAP_df$MAP_Cuts)
# 
# intensityByMAP_df$intensity_Cuts <- cut(x = intensityByMAP_df$intensity,
#                                         right = TRUE,
#                                         breaks = seq(0,
#                                                      7000,
#                                                      500))
# levels(intensityByMAP_df$intensity_Cuts) <- seq(0,7000,500)
# intensityByMAP_df$intensity_Cuts <- as.character(intensityByMAP_df$intensity_Cuts)
# intensityByMAP_df$intensity_Cuts <- as.numeric(intensityByMAP_df$intensity_Cuts)
# 
# 
# intensityByMAP_summary <- ddply(.data = intensityByMAP_df,.(MAP_Cuts,intensity_Cuts),summarize,Count = length(intensity_Cuts))
# intensityByMAP_summary <- ddply(.data = intensityByMAP_summary,.(MAP_Cuts),mutate,Proportion = Count / sum(Count))
# 
# intensityByMAP_summary$MAP_Cuts <- as.factor(as.character(intensityByMAP_summary$MAP_Cuts))
# intensityByMAP_summary$intensity_Cuts <- as.factor(as.character(intensityByMAP_summary$intensity_Cuts))

