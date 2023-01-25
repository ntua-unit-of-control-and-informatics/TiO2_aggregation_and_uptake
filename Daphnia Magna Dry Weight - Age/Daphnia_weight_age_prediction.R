weight_calc <- function(age){
  
  df <- read.csv('C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/Daphnia Magna Dry Weight - Age/weight_growth_data_Pauw_1981.csv')
  df[,1] <- round(df[,1])
  
  age_span <- df[,1]
  weight_span <- df[,2]

  if(age <= min(age_span)){
    dry_weight <- min(weight_span)
  }else if(age <= min(age_span)){
    dry_weight <- max(weight_span)
  }else{ 
    index <- which.min(abs(age - age_span))
    dry_weight <- weight_span[index]    
  }
  return(dry_weight)
}