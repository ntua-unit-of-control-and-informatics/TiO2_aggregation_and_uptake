# Working directory

dir = 'C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/Exposure/Tan_2016'
setwd(dir)

# The following dataframe is a mapping of the of the uptake experiments
A <- c("A", "Low_Ca") # C_Ca = 0.2 mM
B <- c("B", "Low_Ca_refreshed") # C_Ca = 0.2 mM
C <- c("C", "High_Ca") # C_Ca = 2.0 mM
D <- c("D", "High_Ca_refreshed") # C_Ca = 2.0 mM
E <- c("E", "Low_Ca_elimination") # C_Ca = 0.2 mM

Mapping <- data.frame(rbind(A, B, C, D, E))
colnames(Mapping) <- c("Code", "Experiment")
i <- 1:dim(Mapping)[2]
Mapping[,i] <- apply(Mapping[,i], 2,
                     function(x) as.character(x))
sapply(Mapping, class)
experiments <- as.character(Mapping[,2])

# Load uptake data
# The concentrations are reported as mg Ti/g dw daphnia
# We will transform this to mg TiO2/mg dw daphnia
uptake_data <- read.csv('data/uptake_data.csv') #mg Ti/g dw daphnia

# Transform from mg Ti/g dw daphnia to mg TiO2/mg dw daphnia
frac_Ti <- 48/(48+32) # the fraction of Ti mass to the total mass of TiO2 
# C_TiO2 = C_Ti/frac_Ti
uptake_data[,2:5] <- uptake_data[,2:5]/frac_Ti/1000 #mg Ti/mg dw daphnia

uptake_simple <- uptake_data[-c(7),c(1,2,4)]
uptake_refreshed <- uptake_data[c(5:8),c(1,3,5)]

# The depuration experiment followed a 1 hour exposure for the C_Ca = 0.2 mM
# The values are given as %_ratio of the concentration measured after 1 hour of exposure
# Time refers to hours after exposure
depuration_data <- read.csv('data/depuration_data.csv')[,c(1,2)]
depuration_data$Time <- depuration_data$Time+1
# We will use the concentration measured at 1h for the Low_Ca experiment
# to calculate the concentrations during the depuration phase
C_1h <- uptake_data$Low_Ca[uptake_data$Time==1] #mg Ti/mg dw daphnia
depuration_data[,2] <- C_1h*depuration_data[,2]/100

# Betini et al. (2019)
#input: age [days], temperature [oC], food["low"/"high"]
Size_estimation <<- function(age, temperature = 22, food="high"){
  
  # T = 15 o C
  a_low_15 <-0.354
  b_low_15 <- 0.527
  a_high_15 <- 0.105
  b_high_15 <- 0.953
  
  # T = 25 o C
  a_low_25 <- 0.811
  b_low_25 <- 0.355
  a_high_25 <- 0.698
  b_high_25 <- 0.83
  
  if(food == "low"){
    if(temperature <= 15){
      a <- a_low_15
      b <- b_low_15
    }else if(temperature >= 25){  
      a <- a_low_25
      b <- b_low_25
    }else{ 
      a <- approx(c(15,25), c(a_low_15, a_low_25), temperature)$y
      b <- approx(c(15,25), c(b_low_15, b_low_25), temperature)$y
    }
  }else if (food == "high"){
    if(temperature <= 15){
      a <- a_high_15
      b <- b_high_15
    }else if(temperature >= 25){  
      a <- a_high_25
      b <- b_high_25
    }else{ 
      a <- approx(c(15,25), c(a_high_15, a_high_25), temperature)$y
      b <- approx(c(15,25), c(b_high_15, b_high_25), temperature)$y
    }
  }else{
    stop('food must be either "low" or "high" ')
  }
  return(a + b * log(age))
}

Filtration_rate_estimation <<- function(Length, Temperature = 22, method = "Preuss"){
  
  if(method == "Burns"){
    # Filtering rate of Daphnia magna is calculated based on Burns et al. 1969.
    # Input: Length [mm],Ttemperature [oC]/ Output: filtration rate[mL/h]
    F_rate_15 <- 0.153 * Length^2.16
    F_rate_20 <- 0.208 * Length^2.80
    F_rate_25 <- 0.202 * Length^2.38
    
    if(Temperature <= 15){
      F_rate <- F_rate_15
    }else if(Temperature >= 25){  
      F_rate <- F_rate_25
    }else{ 
      F_rate <- approx(c(15,20,25), c(F_rate_15, F_rate_20, F_rate_25), Temperature)$y
    }
  }else if(method == "Preuss"){
    F_rate <- 0.5*Length^2.45
  }else{
    stop("Please select a valid estimation method; either 'Burns' or 'Preuss' ")
  }
  return(F_rate)
}

# Dumont et al. (1975)
# Input: length [mm]/ Output: dry weight[mg]
dry_weight_estimation <<- function(L){
  
  w1 = (1.89e-06*(L*1000)^2.25)/1000 #Donka Lake
  w2 = (4.88e-05*(L*1000)^1.80)/1000 #River Sambre
  # Selected w1 after validation with Martin-Creuzburg et al. (2018
  return(w1)
}

# V_water is the volume of water (in L) in the corresponding experiment.
# The volume remains constant during the experiment and equal to 100 ml
V_water <- 0.7 # L

# Population of daphnias during the experiments
# At each measurement 10 daphnias were removed from the system
N <- 70

# Load the predicted ksed values
ksed_predicted <- read.csv("data/Tan_2016_ksed_predictions.csv")
ksed_predicted <- ksed_predicted[, c(1,4,6)]
colnames(ksed_predicted) <- c("Name", "Concentration_mg/L", "k_sed")

##########################################################

# *** metrics ***

rmse <- function(observed, predicted){
  sqrt(mean((observed-predicted)^2)) 
}

AAFE <- function(predictions, observations, times=NULL){
  y_obs <- unlist(observations)
  y_pred <- unlist(predictions)
  # Total number of observations
  N<- length(y_obs)
  log_ratio <- rep(NA, N) 
  for ( i in 1:N){
    log_ratio[i] <- abs(log((y_pred[i]/y_obs[i]), base = 10))
  }
  aafe <- 10^(sum(log_ratio)/N) 
  return(aafe)
}

PBKOF <- function(observed, predicted, comp.names =NULL){
  # Check if the user provided the correct input format
  if (!is.list(observed) || !is.list(predicted)){
    stop(" The observations and predictions must be lists")
  }
  # Check if the user provided equal length lists
  if (length(observed) != length(predicted)){
    stop(" The observations and predictions must have the same compartments")
  }
  Ncomp <- length(observed) # Number of compartments
  I <- rep(NA, Ncomp) # Compartment discrepancy index
  N_obs <- rep(NA, Ncomp) #Number of observations per compartment
  #loop over the compartments
  for (i in 1:Ncomp){
    Et <- 0 #relative error with observations
    St <- 0  #relative error with simulations
    N <- length(observed[[i]]) # number of observations for compartment i
    # Check if observations and predictions have equal length
    if(N != length(predicted[[i]])){
      stop(paste0("Compartment ",i," had different length in the observations and predictions"))
    }
    N_obs[i] <- N # populate the N_obs vector
    for (j in 1:N){
      # sum of relative squared errors (error = observed - predicted)
      Et <- Et + ( abs(observed[[i]][j] - predicted[[i]][j])  / observed[[i]][j] )  ^2
      St <- St + ( abs(observed[[i]][j] - predicted[[i]][j])  / predicted[[i]][j] )  ^2
    }
    
    # root mean of the square of observed values
    RMEt <- sqrt(Et/N)
    # root mean of the square of simulated values
    RMSt <- sqrt( St/N)
    
    I[i] <- (RMEt + RMSt)/2   
  }
  # Total number of observations
  Ntot <- sum(N_obs)
  # Initialise the consolidated discrepancy index
  Ic <-0
  for (i in 1:Ncomp){
    # Give weight to compartments with more observations (more information)
    Ic <- Ic +  I[i]* N_obs[i]/Ntot
  }
  # Name the list of compartment discrepancy indices
  if ( !is.null(comp.names)){
    names(I) <- comp.names
  }else if (!is.null(names(observed))){
    names(I) <- names(observed)
  } else if (!is.null(names(predicted)) && is.null(comp.names) ){
    names(I) <- names(predicted)
  }
  return(Ic)
  #return(list(Total_index = Ic, Compartment_index= I))
}

ode_func <- function(time, inits, params, experiment){
  with(as.list(c(inits, params)),{
    # Units explanation:
    # C_water: mg TiO2 / L water (same as data)
    # C_daphnia: mg TiO2 / mg daphnia 
    # k_sed: 1/h
    # F_rate: L water/h/individual daphnia 
    # ke_2: 1/h
    
    # Number of Daphnids per beaker
    if(experiment %in% c('A','C','E')){
      N <- seq(70,10,-10)
      if (time < 1){
        N_current <- N[1] # 70 daphnids
      }else if (1 <= time & time < 2){
        N_current <- N[2] # 60 daphnids
      }else if (2 <= time & time < 4){
        N_current <- N[3] # 50 daphnids
      }else if (4 <= time & time < 6){
        N_current <- N[4] # 40 daphnids
      }else if (6 <= time & time < 12){
        N_current <- N[5] # 30 daphnids
      }else if (12 <= time & time < 24){
        N_current <- N[6] # 20 daphnids
      }else if (24 <= time){
        N_current <- N[7] # 10 daphnids
      }
    }else if(experiment %in% c('B','D')){
      N <- seq(40,10,-10)
      if (time < 12){
        N_current <- N[1] # 40 daphnids
      }else if (12 <= time & time < 24){
        N_current <- N[2] # 30 daphnids
      }else if (24 <= time & time < 36){
        N_current <- N[3] # 20 daphnids
      }else if (36 <= time){
        N_current <- N[4] # 10 daphnids
      }
    }
    
    # Exponential decay of dry weight
    #dry_weight <- dry_weight* exp(-beta*time/24)
    time_d <- 1/24 + time/24 #time in days
    # Weight loss under starvation through exponential decay (Elen et al., 1989)
    reduction <-ifelse(time_d<2, (1- 0.32*(1-exp(-36.6*time_d))), 
                       (0.682 - 0.435 * (1- 16.79*exp(-1.41*time_d))))
    dry_weight_current <- reduction * dry_weight
    
    #C_water: TiO2 concentration in water
    dC_water <- -(N_current*a*(F_rate/1000)*((1-C_daphnia/C_sat)^n)*C_water)/V_water-
      k_sed*C_water
    
    # Daphnia magna
    dC_daphnia = a*(F_rate/1000)*((1-C_daphnia/C_sat)^n)*C_water/dry_weight - ke_2*C_daphnia 
    
    # Excreted from each D.magna
    dM_Daphnia_excreted <-  N_current*ke_2*C_daphnia*dry_weight
    
    # Mass in  Sediment
    dM_sed <- k_sed*C_water*V_water
    
    # TiO2 mass in all D.magna
    M_daphnia_tot <- C_daphnia*dry_weight*N_current
    
    # TiO2 mass in water
    M_water <- C_water*V_water
    
    # Mass balance of TiO2 (should always be the total mass of the system)
    Mass_balance = M_daphnia_tot + M_water + M_sed + M_Daphnia_excreted
    
    return(list(c(dC_water, dC_daphnia, dM_Daphnia_excreted, dM_sed),
                "M_daphnia_tot"=M_daphnia_tot,
                "M_water"=M_water,
                "Mass_balance"=Mass_balance))
    
  })
}

obj_func <- function(x, C_water_0, experiments, ksed_predicted, sedimentation = T, score = 'rmse'){
  
  
  # Experiment A: 
  experiment <- 'A'
  V_water = 0.7 # L
  
  a <- x[1]
  ke_2 <- x[3]
  C_sat <- x[4]
  n <- x[5]
  
  age <- 7 #days
  L = Size_estimation(age, temperature=23.5) #mm
  dry_weight =  dry_weight_estimation(L) #mg
  F_rate <- Filtration_rate_estimation(L, Temperature=23.5,  method = 'Preuss')#mL/h
  
  exp_data <- uptake_simple[,c(1,2)]
  sol_times <- seq(0, 50, 0.1)
  
  # Check the sedimentation condition
  if(sedimentation){
    k_sed <- ksed_predicted[2,3]
  }else{
    k_sed=0
  }
  
  constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                       'k_sed'= k_sed)
  fitted_params <- c("a"=a, "ke_2"=ke_2, "C_sat"=C_sat, "n"=n)
  params <- c(fitted_params, constant_params)
  
  inits <- c('C_water'=C_water_0, 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
             'M_sed'=0)      
  
  solution <- data.frame(deSolve::ode(times = sol_times,  func = ode_func, y = inits,
                                      parms = params,
                                      method="lsodes",
                                      rtol = 1e-3, atol = 1e-3,
                                      experiment=experiment))
  
  if(sum(solution$time %in% exp_data$Time) == dim(exp_data)[1]){
    results <- solution[which(solution$time %in% exp_data$Time), 'C_daphnia']
  } else{
    stop(print("Length of predictions is not equal to the length of data"))
  }
  
  if(score == "rmse"){
    score_A <- rmse(exp_data[,2], results)
  }else if(score == "AAFE"){
    score_A <- AAFE(exp_data[,2], results)  
  }else if (score == "PBKOF"){
    score_A <- PBKOF(list(exp_data[,2]), list(results))
  }
  
  # finished experiment A
  ############################################################################################
  
  # Experiment B:
  experiment <- 'B'
  V_water = 0.4 # L
  
  a <- x[1]
  ke_2 <- x[3]
  C_sat <- x[4]
  n <- x[5]
  
  age <- 7 #days
  L = Size_estimation(age, temperature=23.5) #mm
  dry_weight =  dry_weight_estimation(L) #mg
  F_rate <- Filtration_rate_estimation(L, Temperature=23.5,  method = 'Preuss')#mL/h
  
  exp_data <- uptake_refreshed[,c(1,2)]
  sol_times <- seq(0, 50, 0.1)
  
  # Check the sedimentation condition
  if(sedimentation){
    k_sed <- ksed_predicted[2,3]
  }else{
    k_sed=0
  }
  
  constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                       'k_sed'= k_sed)
  fitted_params <- c("a"=a, "ke_2"=ke_2, "C_sat"=C_sat, "n"=n)
  params <- c(fitted_params, constant_params)
  
  
  inits <- c('C_water'=C_water_0, 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
             'M_sed'=0)      
  refresh_moments <- seq(12,48, 12)
  eventdat <- data.frame(var = c("C_water"),
                         time = refresh_moments ,
                         value = rep(inits[1], length(refresh_moments)),
                         method = c(rep("rep", length(refresh_moments)))
  )
  solution <- data.frame(deSolve::ode(times = sol_times,  func = ode_func, y = inits,
                                      parms = params,
                                      method="lsodes",
                                      events = list(data = eventdat),
                                      rtol = 1e-3, atol = 1e-3,
                                      experiment=experiment))
  
  if(sum(solution$time %in% exp_data$Time) == dim(exp_data)[1]){
    results <- solution[which(solution$time %in% exp_data$Time), 'C_daphnia']
  } else{
    stop(print("Length of predictions is not equal to the length of data"))
  }
  
  if(score == "rmse"){
    score_B <- rmse(exp_data[,2], results)
  }else if(score == "AAFE"){
    score_B <- AAFE(exp_data[,2], results)  
  }else if (score == "PBKOF"){
    score_B <- PBKOF(list(exp_data[,2]), list(results))
  }
  
  # finished experiment B
  ############################################################################################
  
  # Experiment C:
  experiment <- 'C'
  V_water = 0.7# L
  
  a <- x[2]
  ke_2 <- x[3]
  C_sat <- x[4]
  n <- x[5]
  
  age <- 7 #days
  L = Size_estimation(age, temperature=23.5) #mm
  dry_weight =  dry_weight_estimation(L) #mg
  F_rate <- Filtration_rate_estimation(L, Temperature=23.5,  method = 'Preuss')#mL/h
  
  exp_data <- uptake_simple[,c(1,3)]
  sol_times <- seq(0, 50, 0.1)
  
  # Check the sedimentation condition
  if(sedimentation){
    k_sed <- ksed_predicted[2,3]
  }else{
    k_sed=0
  }
  
  constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                       'k_sed'= k_sed)
  fitted_params <- c("a"=a, "ke_2"=ke_2, "C_sat"=C_sat, "n"=n)
  params <- c(fitted_params, constant_params)
  
  inits <- c('C_water'=C_water_0, 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
             'M_sed'=0)      
  
  solution <- data.frame(deSolve::ode(times = sol_times,  func = ode_func, y = inits,
                                      parms = params,
                                      method="lsodes",
                                      rtol = 1e-3, atol = 1e-3,
                                      experiment=experiment))
  
  if(sum(solution$time %in% exp_data$Time) == dim(exp_data)[1]){
    results <- solution[which(solution$time %in% exp_data$Time), 'C_daphnia']
  } else{
    stop(print("Length of predictions is not equal to the length of data"))
  }
  
  if(score == "rmse"){
    score_C <- rmse(exp_data[,2], results)
  }else if(score == "AAFE"){
    score_C <- AAFE(exp_data[,2], results)  
  }else if (score == "PBKOF"){
    score_C <- PBKOF(list(exp_data[,2]), list(results))
  }
  
  # finished experiment C
  ############################################################################################
  
  # Experiment D:
  experiment <- 'D'
  V_water = 0.4 # L
  
  a <- x[2]
  ke_2 <- x[3]
  C_sat <- x[4]
  n <- x[5]
  
  age <- 7 #days
  L = Size_estimation(age, temperature=23.5) #mm
  dry_weight =  dry_weight_estimation(L) #mg
  F_rate <- Filtration_rate_estimation(L, Temperature=23.5,  method = 'Preuss')#mL/h
  
  exp_data <- uptake_refreshed[,c(1,3)]
  sol_times <- seq(0, 50, 0.1)
  
  # Check the sedimentation condition
  if(sedimentation){
    k_sed <- ksed_predicted[2,3]
  }else{
    k_sed=0
  }
  
  constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                       'k_sed'= k_sed)
  fitted_params <- c("a"=a, "ke_2"=ke_2, "C_sat"=C_sat, "n"=n)
  params <- c(fitted_params, constant_params)
  
  inits <- c('C_water'=C_water_0, 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
             'M_sed'=0)      
  refresh_moments <- seq(12,48, 12)
  eventdat <- data.frame(var = c("C_water"),
                         time = refresh_moments ,
                         value = rep(inits[1], length(refresh_moments)),
                         method = c(rep("rep", length(refresh_moments)))
  )
  solution <- data.frame(deSolve::ode(times = sol_times,  func = ode_func, y = inits,
                                      parms = params,
                                      method="lsodes",
                                      events = list(data = eventdat),
                                      rtol = 1e-3, atol = 1e-3,
                                      experiment=experiment))
  
  if(sum(solution$time %in% exp_data$Time) == dim(exp_data)[1]){
    results <- solution[which(solution$time %in% exp_data$Time), 'C_daphnia']
  } else{
    stop(print("Length of predictions is not equal to the length of data"))
  }
  
  if(score == "rmse"){
    score_D <- rmse(exp_data[,2], results)
  }else if(score == "AAFE"){
    score_D <- AAFE(exp_data[,2], results)  
  }else if (score == "PBKOF"){
    score_D <- PBKOF(list(exp_data[,2]), list(results))
  }
  
  # finished experiment D
  ############################################################################################
  
  # Experiment E:
  experiment <- 'E'
  
  V_water = 0.7 # L
  
  a <- x[1]
  ke_2 <- x[3]
  C_sat <- x[4]
  n <- x[5]
  
  age <- 7 #days
  L = Size_estimation(age, temperature=23.5) #mm
  dry_weight =  dry_weight_estimation(L) #mg
  F_rate <- Filtration_rate_estimation(L, Temperature=23.5,  method = 'Preuss')#mL/h
  
  exp_data <- depuration_data[,c(1,2)]
  sol_times <- seq(0, 50, 0.1)
  
  # Check the sedimentation condition
  if(sedimentation){
    k_sed <- ksed_predicted[2,3]
  }else{
    k_sed=0
  }
  
  constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                       'k_sed'= k_sed)
  fitted_params <- c("a"=a, "ke_2"=ke_2, "C_sat"=C_sat, "n"=n)
  params <- c(fitted_params, constant_params)
  
  inits <- c('C_water'=C_water_0, 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
             'M_sed'=0)      
  refresh_moments <- c(1)
  eventdat <- data.frame(var = c("C_water"),
                         time = refresh_moments ,
                         value = 0,
                         method = 'rep')
  solution <- data.frame(deSolve::ode(times = sol_times,  func = ode_func, y = inits,
                                      parms = params,
                                      method="lsodes",
                                      events = list(data = eventdat),
                                      rtol = 1e-3, atol = 1e-3,
                                      experiment=experiment))
  
  if(sum(solution$time %in% exp_data$Time) == dim(exp_data)[1]){
    results <- solution[which(solution$time %in% exp_data$Time), 'C_daphnia']
  } else{
    stop(print("Length of predictions is not equal to the length of data"))
  }
  
  if(score == "rmse"){
    score_E <- rmse(exp_data[,2], results)
  }else if(score == "AAFE"){
    score_E <- AAFE(exp_data[,2], results)  
  }else if (score == "PBKOF"){
    score_E <- PBKOF(list(exp_data[,2]), list(results))
  }
  
  return(mean(c(score_A, score_B, score_C, score_D, score_E) ))
  
}


plot_func <- function(optimization, C_water_0, experiments, ksed_predicted, sedimentation = T, score = 'rmse'){
  x<-optimization$solution
  # Experiment A: 
  experiment <- 'A'
  V_water = 0.7 # L
  
  a <- x[1]
  ke_2 <- x[3]
  C_sat <- x[4]
  n <- x[5]
  
  age <- 7 #days
  L = Size_estimation(age, temperature=23.5) #mm
  dry_weight =  dry_weight_estimation(L) #mg
  F_rate <- Filtration_rate_estimation(L, Temperature=23.5,  method = 'Preuss')#mL/h
  
  exp_data <- uptake_simple[,c(1,2)]
  sol_times <- seq(0, 50, 0.1)
  
  # Check the sedimentation condition
  if(sedimentation){
    k_sed <- ksed_predicted[2,3]
  }else{
    k_sed=0
  }
  
  constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                       'k_sed'= k_sed)
  fitted_params <- c("a"=a, "ke_2"=ke_2, "C_sat"=C_sat, "n"=n)
  params <- c(fitted_params, constant_params)
  
  inits <- c('C_water'=C_water_0, 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
             'M_sed'=0)      
  
  solution <- data.frame(deSolve::ode(times = sol_times,  func = ode_func, y = inits,
                                      parms = params,
                                      method="lsodes",
                                      rtol = 1e-3, atol = 1e-3,
                                      experiment=experiment))
  
  predictions_A <- solution[c("time", "C_daphnia")]
  
  # Experiment B:
  experiment <- 'B'
  V_water = 0.4 # L
  
  a <- x[1]
  ke_2 <- x[3]
  C_sat <- x[4]
  n <- x[5]
  
  age <- 7 #days
  L = Size_estimation(age, temperature=23.5) #mm
  dry_weight =  dry_weight_estimation(L) #mg
  F_rate <- Filtration_rate_estimation(L, Temperature=23.5,  method = 'Preuss')#mL/h
  
  exp_data <- uptake_refreshed[,c(1,2)]
  sol_times <- seq(0, 50, 0.1)
  
  # Check the sedimentation condition
  if(sedimentation){
    k_sed <- ksed_predicted[2,3]
  }else{
    k_sed=0
  }
  
  constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                       'k_sed'= k_sed)
  fitted_params <- c("a"=a, "ke_2"=ke_2, "C_sat"=C_sat, "n"=n)
  params <- c(fitted_params, constant_params)
  
  inits <- c('C_water'=C_water_0, 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
             'M_sed'=0)      
  refresh_moments <- seq(12,48, 12)
  eventdat <- data.frame(var = c("C_water"),
                         time = refresh_moments ,
                         value = rep(inits[1], length(refresh_moments)),
                         method = c(rep("rep", length(refresh_moments)))
  )
  solution <- data.frame(deSolve::ode(times = sol_times,  func = ode_func, y = inits,
                                      parms = params,
                                      method="lsodes",
                                      events = list(data = eventdat),
                                      rtol = 1e-3, atol = 1e-3,
                                      experiment=experiment))
  
  predictions_B <- solution[c("time", "C_daphnia")]
  
  # Experiment C:
  experiment <- 'C'
  V_water = 0.7# L
  
  a <- x[2]
  ke_2 <- x[3]
  C_sat <- x[4]
  n <- x[5]
  
  age <- 7 #days
  L = Size_estimation(age, temperature=23.5) #mm
  dry_weight =  dry_weight_estimation(L) #mg
  F_rate <- Filtration_rate_estimation(L, Temperature=23.5,  method = 'Preuss')#mL/h
  
  exp_data <- uptake_refreshed[,c(1,2)]
  sol_times <- seq(0, 50, 0.1)
  
  # Check the sedimentation condition
  if(sedimentation){
    k_sed <- ksed_predicted[2,3]
  }else{
    k_sed=0
  }
  
  constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                       'k_sed'= k_sed)
  fitted_params <- c("a"=a, "ke_2"=ke_2, "C_sat"=C_sat, "n"=n)
  params <- c(fitted_params, constant_params)
  
  inits <- c('C_water'=C_water_0, 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
             'M_sed'=0)      
  
  solution <- data.frame(deSolve::ode(times = sol_times,  func = ode_func, y = inits,
                                      parms = params,
                                      method="lsodes",
                                      rtol = 1e-3, atol = 1e-3,
                                      experiment=experiment))
  
  predictions_C <- solution[c("time", "C_daphnia")]
  
  
  # Experiment D:
  experiment <- 'D'
  V_water = 0.4 # L
  
  a <- x[2]
  ke_2 <- x[3]
  C_sat <- x[4]
  n <- x[5]
  
  age <- 7 #days
  L = Size_estimation(age, temperature=23.5) #mm
  dry_weight =  dry_weight_estimation(L) #mg
  F_rate <- Filtration_rate_estimation(L, Temperature=23.5,  method = 'Preuss')#mL/h
  
  exp_data <- uptake_refreshed[,c(1,2)]
  sol_times <- seq(0, 50, 0.1)
  
  # Check the sedimentation condition
  if(sedimentation){
    k_sed <- ksed_predicted[2,3]
  }else{
    k_sed=0
  }
  
  constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                       'k_sed'= k_sed)
  fitted_params <- c("a"=a, "ke_2"=ke_2, "C_sat"=C_sat, "n"=n)
  params <- c(fitted_params, constant_params)
  
  inits <- c('C_water'=C_water_0, 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
             'M_sed'=0)      
  refresh_moments <- seq(12,48, 12)
  eventdat <- data.frame(var = c("C_water"),
                         time = refresh_moments ,
                         value = rep(inits[1], length(refresh_moments)),
                         method = c(rep("rep", length(refresh_moments)))
  )
  solution <- data.frame(deSolve::ode(times = sol_times,  func = ode_func, y = inits,
                                      parms = params,
                                      method="lsodes",
                                      events = list(data = eventdat),
                                      rtol = 1e-3, atol = 1e-3,
                                      experiment=experiment))
  
  predictions_D <- solution[c("time", "C_daphnia")]
  
  # Experiment E:
  experiment <- 'E'
  
  V_water = 0.7 # L
  
  a <- x[1]
  ke_2 <- x[3]
  C_sat <- x[4]
  n <- x[5]
  
  age <- 7 #days
  L = Size_estimation(age, temperature=23.5) #mm
  dry_weight =  dry_weight_estimation(L) #mg
  F_rate <- Filtration_rate_estimation(L, Temperature=23.5,  method = 'Preuss')#mL/h
  
  exp_data <- depuration_data[,c(1,2)]
  sol_times <- seq(0, 50, 0.1)
  
  # Check the sedimentation condition
  if(sedimentation){
    k_sed <- ksed_predicted[2,3]
  }else{
    k_sed=0
  }
  
  constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                       'k_sed'= k_sed)
  fitted_params <- c("a"=a, "ke_2"=ke_2, "C_sat"=C_sat, "n"=n)
  params <- c(fitted_params, constant_params)
  
  inits <- c('C_water'=C_water_0, 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
             'M_sed'=0)      
  refresh_moments <- c(1)
  eventdat <- data.frame(var = c("C_water"),
                         time = refresh_moments ,
                         value = 0,
                         method = 'rep')
  solution <- data.frame(deSolve::ode(times = sol_times,  func = ode_func, y = inits,
                                      parms = params,
                                      method="lsodes",
                                      events = list(data = eventdat),
                                      rtol = 1e-3, atol = 1e-3,
                                      experiment=experiment))
  predictions_E <- solution[c("time", "C_daphnia")]
  
  
  strings <- c('A','B','C','D','E')
  color_codes <- scales::hue_pal()(5) # to return 5 color codes 
  cls <- c()  
  for (i in 1:length(strings)) {
    cls[i] <- color_codes[i]
    names(cls)[i] <- strings[i]
  }
  library(ggplot2)
  draw_plot <- ggplot()+
    geom_line(data = predictions_A, aes(x=time, y=C_daphnia , color=strings[1]), size=1.7)+
    geom_line(data = predictions_B, aes(x=time, y=C_daphnia , color=strings[2]), size=1.7)+
    geom_line(data = predictions_C, aes(x=time, y=C_daphnia , color=strings[3]), size=1.7)+
    geom_line(data = predictions_D, aes(x=time, y=C_daphnia , color=strings[4]), size=1.7)+
    geom_line(data = predictions_E, aes(x=time, y=C_daphnia , color=strings[5]), size=1.7)+
    
    #Experiment A
    geom_point(data = uptake_simple, aes(x=Time, y=Low_Ca , color=strings[1]), size=5)+
    #Experiment B
    geom_point(data = uptake_refreshed, aes(x=Time, y=Low_Ca_refreshed , color=strings[2]), size=5)+
    #Experiment C
    geom_point(data = uptake_simple, aes(x=Time, y=High_Ca , color=strings[3]), size=5)+
    #Experiment D
    geom_point(data = uptake_refreshed, aes(x=Time, y=High_Ca_refreshed , color=strings[4]), size=5)+
    #Experiment E
    geom_point(data = depuration_data, aes(x=Time, y=Control , color=strings[5]), size=5)+
    
    labs(title = 'Tan et al. (2016)',
         y = "Concentration in Daphnia Magna (mg TiO2/mg daphnia)", x = "Time (hours)")+
    theme(plot.title = element_text(hjust = 0.5,size=30), 
          axis.title.y =element_text(hjust = 0.5,size=25,face="bold"),
          axis.text.y=element_text(size=22),
          axis.title.x =element_text(hjust = 0.5,size=25,face="bold"),
          axis.text.x=element_text(size=22),
          legend.title=element_text(hjust = 0.5,size=25), 
          legend.text=element_text(size=22)) + 
    
    scale_color_manual("Experiments", values=cls)+
    theme(legend.key.size = unit(1.5, 'cm'),  
          legend.title = element_text(size=14),
          legend.text = element_text(size=14),
          axis.text = element_text(size = 14))
  
  return(draw_plot)
}

################################################################################

# 2 aplhas, 1 ke, 1 c_sat, 1 n
x0 <- c(0.5, 0.5, 0.01, 0.05, 3) 
lb <- c(0, 0, 0, 0.02770548, 2)
ub <- c(4, 4, 0.5, 0.4, 4)

C_water_0 <- 6.67 # mg TiO2/L

opts <- list( "algorithm" = "NLOPT_LN_SBPLX" , #"NLOPT_LN_NEWUOA"
              "xtol_rel" = 1e-07,
              "ftol_rel" = 1e-07,
              "ftol_abs" = 0.0,
              "xtol_abs" = 0.0 ,
              "maxeval" = 800,
              "print_level" = 1)

optimization <- nloptr::nloptr(x0 = x0,
                               eval_f = obj_func,
                               lb	= lb,
                               ub = ub,
                               opts = opts,
                               C_water_0 = C_water_0,
                               experiments = experiments,
                               ksed_predicted = ksed_predicted,
                               sedimentation = T,
                               score = 'rmse')

plot_func(optimization, C_water_0, experiments, ksed_predicted, sedimentation = T, score = 'rmse')
