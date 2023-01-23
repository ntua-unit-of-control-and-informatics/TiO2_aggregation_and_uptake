setwd('C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/Daphnia Magna Filtration Rate')

filtration_data <- read.csv('Burns et al.1969 filtration rate data.csv')

Filtration_rate_func <- function(temperature, age, adulthood_threshold=14){
  # Keep ony the data for immature or adult daphnia based on the 
  # age and adulthood_threshold given.
  if (age <= adulthood_threshold){
    df <- filtration_data[,1:2]
  } else{
    df <- filtration_data[,c(1,3)]
  }
  #Interpolation
  #Set the boundaries about the temperature
  if(temperature <= min(df$Temperature)){
    F_rate <- df[which.min(df$Temperature), 2]
  }else if(temperature >= max(df$Temperature)){
    F_rate <- df[which.max(df$Temperature), 2]
  }else{
    F_rate <- approx(df[,1], df[,2], temperature)
  }
  return(F_rate)
}




