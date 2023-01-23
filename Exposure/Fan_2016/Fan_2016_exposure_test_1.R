# This is a script to simulate the experiments in Cehn et al., 2019
# The first experiment is about the waterborne exposure of D. Magna to TiO2 nanoparticles
# The second is about the trophic exposure to TiO2 exposured algae.

# Working directory
setwd('C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/Exposure/Fan_2016')

#=================#
#  Water exposure #
#=================#

# The following dataframe is a mappin of the of the TiO2 NPs types with 
# their corresponding codes (which are simple letters)
A <- c("A", "TiO2-T1")
B <- c("B", "TiO2-T2")
C <- c("C", "TiO2-H1")
D <- c("D", "TiO2-S1")
E <- c("E", "TiO2-H2")
G <- c("G", "TiO2-S2")

Mapping <- data.frame(rbind(A, B, C, D, E, G))
colnames(Mapping) <- c("Code", "Type")
i <- 1:dim(Mapping)[2]
Mapping[,i] <- apply(Mapping[,i], 2,
                     function(x) as.character(x))
sapply(Mapping, class)

# Load data  for water exposure - uptake phase

# The values are reported as mg TiO2/g of dry daphnia
# Time is given in minutes (the uptake experiment lasted 60 minutes)
# Load the data for concentration = 0.1 mg/l
C1_data <- read.csv('data/exposure_data/uptake/0.1_uptake.csv')
colnames(C1_data)[-1] <- Mapping$Type
C1_data[,2:6] <-  C1_data[,2:6]/1e03 # transform from mg/g daphnia to mg/mg daphnia
C1_data[,1] <- C1_data[,1]/60 # Transform time to hours

# Load the data for concentration = 1.0 mg/l
C2_data <- read.csv('data/exposure_data/uptake/1_uptake.csv')
colnames(C2_data)[-1] <- Mapping$Type
C2_data[,2:6] <-  C2_data[,2:6]/1e03 # transform from mg/kg daphnia to mg/mg daphnia
C2_data[,1] <- C2_data[,1]/60 # Transform time to hours

# Load the data for concentration = 10.0 mg/l
C3_data <- read.csv('data/exposure_data/uptake/10_uptake.csv')
colnames(C3_data)[-1] <- Mapping$Type
C3_data[,2:6] <-  C3_data[,2:6]/1e03 # transform from mg/kg daphnia to mg/mg daphnia
C3_data[,1] <- C3_data[,1]/60 # Transform time to hours

# Load data for the dry weight calculation
df <- read.csv('weight_growth_data_Pauw_1981.csv')
df[,1] <- round(df[,1])

weight_calc <- function(age){
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
  return(dry_weight) # in mg
}

# The daphnias used in water exposure experiments are between 7 and 14 days old
# We consider an average age f the daphnias equal to 10 days (Pauw et al.1981)
dry_weight <- weight_calc(14) # mg dry weight of individual daphnia 

# V_water is the volume of water (in L) in the corresponding experiment.
# The volume remains constant during the experiment and equal to 100 ml
V_water <- 0.1 # L

# Population of daphnias during the experiments
# At each measurement 10 daphnias were removed from the system
N <- seq(80,10,-10)

# Filtering rate of Daphnia magna is calculated based on Burns et al. 1969.
Filtration_rate_func <- function(temperature, age, adulthood_threshold=14){
  # load the data for filtration rate
  filtration_data <- read.csv('C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/Daphnia Magna Filtration Rate/Burns et al.1969 filtration rate data.csv')
  
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
# Units of filtration rate are ml water/h/mg dry weight of daphnia
F_rate <- Filtration_rate_func(22, 10)$y
# Multiply with the average dry weight (transformed into mg) of an individual.
F_rate <- F_rate*dry_weight

# Load the predicted ksed values
ksed_predicted <- read.csv("data/Fan_2016_ksed_predictions.csv")
ksed_predicted <- ksed_predicted[, c(1,4,6)]
colnames(ksed_predicted) <- c("Name", "Concentration_mg/L", "k_sed")


##########################################################

# *** metrics ***

# The metric used for the optimization
mse_custom <- function(observed, predicted){
  mean((observed - predicted)^2)
}

mape <- function(observed, predicted){
  mean(abs(observed-predicted)*100/observed)
}

rmse <- function(observed, predicted){
  sqrt(mean((observed-predicted)^2)) 
}

#=====================================#
# Functions used for the optimization #
#=====================================#

# ode_func(): the differential equation system that deiscribes the model

ode_func <- function(time, inits, params){
  with(as.list(c(inits, params)),{
    
    # Units explanation:
    # C_water: mg TiO2 / L water (same as data)
    # C_algae: mg TiO2 / g algae 
    # C_daphnia: mg TiO2 / mg daphnia 
    # k_sed: 1/h
    # ku: 1/h
    # F_rate: L water/h per individual daphnia 
    # ke_2: 1/h
    
    # The number of Daphnias per beaker is constant to 10 for each exposure
    # at each time point. 
    
    N_current  <- 10
    
    # C_water: TiO2 concentration in water
    dC_water <- - (a*(F_rate/1000)*N_current/V_water + k_sed + ku)*C_water
    
    # Algae
    dC_algae <- ku*C_water - ke_1*C_algae
    
    # Daphnia magna
    dC_daphnia = a*(F_rate/1000)*C_water/dry_weight + F_rate*C_algae - ke_2*C_daphnia 
    
    return(list(c(dC_water, dC_algae, dC_daphnia)))
  })
}

obj_func <- function(){
  
}


