# This is a script to simulate the experiments in Zhu et al., 2010
# The first experiment is about the waterborne exposure of D. Magna to TiO2 nanoparticles

setwd("C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/Exposure/Zhu_2010")



# Function for estimating length of D. magna based on age (from Betini et al. (2019))
# Input: age [days], temperature [oC], food["low"/"high"]/ Output: length [mm]
# Considers female D. magna
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


Filtration_rate_estimation <<- function(length, temperature = 22, method = "Preuss"){
  if(method == "Burns"){
    # Filtering rate of Daphnia magna is calculated based on Burns et al. 1969.
    # Input: length [mm],Temperature [oC]/ Output: filtration rate[mL/h]
    F_rate_15 <- 0.153 * length^2.16
    F_rate_20 <- 0.208 * length^2.80
    F_rate_25 <- 0.202 * length^2.38
    
    if(temperature <= 15){
      F_rate <- F_rate_15
    }else if(temperature >= 25){  
      F_rate <- F_rate_25
    }else{ 
      F_rate <- approx(c(15,20,25), c(F_rate_15, F_rate_20, F_rate_25), temperature)$y
    }
  }else if(method == "Preuss"){
    F_rate <- 0.5*length^2.45
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

#dry_weight_zhu <-  0.0025 
#F_rate <- 0.46
#=================#
#  Water exposure #
#=================#

# The following dataframe is a mappin of the of the TiO2 NPs types with 
# their corresponding codes (which are simple letters)
Mapping <- data.frame("A", "TiO2-P25")
colnames(Mapping) <-  c("Code", "Type")
i <- 1:dim(Mapping)[2]
Mapping[,i] <- apply(Mapping[,i], 2,
                     function(x) as.character(x))
sapply(Mapping, class)

# Load the data for water exposure

# The values are reported as mg TiO2/g of dry daphnia
# Time is given in hours 
# Load the data for concentrations 0.1 mg/l and 1.0 mg/l
# The values  are loaded with units g TiO2/kg dry daphnia
# They  will be transformed into mg TiO2/mg dry daphnia

exposure_data <- read.csv('data/water_exposure.csv')
exposure_data[,c(2,3)] <- exposure_data[,c(2,3)]*1e-03 # tranform to mg TiO2/mg dry daphnia 
C1_data <-  exposure_data[,c(1,2)] # data for 0.1 mg/l exposure
C2_data <-  exposure_data[,c(1,3)] # data for 1.0 mg/l exposure

# V_water is the volume of water (in L) in the corresponding experiment.
# The volume remains constant during the experiment and equal to 100 ml
V_water <- 0.03 # L

# Population of daphnias during the experiments
# At each measurement 10 daphnias were removed from the system
#N <- seq(80,10,-10)


# Load the predicted ksed values
ksed_predicted <- read.csv("C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/Exposure/Zhu_2010/data/Zhu_2010_ksed_predictions.csv")
ksed_predicted <- ksed_predicted[, c(1,4,6)]
colnames(ksed_predicted) <- c("Name", "Concentration_mg/L", "k_sed")

#==============================
# *** metrics ***
#=============================
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
    # F_rate: L water/h/individual daphnia 
    # ke_2: 1/h
    
    
    # Number of Daphnids per beaker
    N_current <- 20
    # Exponential decay of dry weight
    #dry_weight <- dry_weight* exp(-beta*time/24)
    C_sat <- 0.251# From fitting to Chen et al. (2019) data
    n <-  2.7
    #k_sed = 0
    
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

# obj_func function to minimize
obj_func <- function(x, C_water_0, nm_types, V_water, ksed_predicted){
  
  nm_type <- nm_types
  
  exp_data <- exposure_data 
  
  
  sol_times <- seq(0,100,0.1)
  
  score_per_conc <- c()
  
  age <- x[3]#days
  L = Size_estimation(age) #mm
  dry_weight =  dry_weight_estimation(L) #mg
  F_rate <- Filtration_rate_estimation(L)#mL/h
  
  for (i in 1:2) { #loop for the 2 different concentrations
    constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                         'k_sed'= ifelse(i==1, ksed_predicted$k_sed[1], ksed_predicted$k_sed[2]))
    fitted_params <- c("a"=x[1], "ke_2"=x[2])
    params <- c(fitted_params, constant_params)
    
    inits <- c('C_water'=C_water_0[i], 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
               'M_sed'=0)
    
    # Water is renewed during depuration after each sampling point
    refresh_moments <- rep(24,6) +c(0, 6, 12, 24, 48, 72)
    eventdat <- data.frame(var = c("C_water"),
                           time = c(refresh_moments) ,
                           value = rep(0, length(refresh_moments)),
                           method = rep("rep", (length(refresh_moments)))) 
    
    solution <<- data.frame(deSolve::ode(times = sol_times,  func = ode_func, y = inits,
                                         parms = params,
                                         events = list(data = eventdat),
                                         method="lsodes",
                                         rtol = 1e-3, atol = 1e-3))
    if(sum(solution$time %in% exp_data$Time) == dim(exp_data)[1]){
      results <- solution[which(solution$time %in% exp_data$Time), 'C_daphnia']
    } else{
      stop(print("Length of predictions is not equal to the length of data"))
    }
    
    #score_per_conc[i] <- AAFE(exp_data[,i+1], results)
    score_per_conc[i] <- PBKOF(list(exp_data[,i+1]), list(results))
    #score_per_conc[i] <- rmse(exp_data[,i+1], results)
    
  }
  
  return(mean(score_per_conc))
}


plot_func <- function(optimization, C_water_0, nm_types, V_water, ksed_predicted){
  
  library(ggplot2)
  x <- optimization$solution
  nm_type <- nm_types
  
  exp_data <- exposure_data
  
  sol_times <- seq(0,100,0.1)
  
  score_per_conc <- c()
  
  keep_predictions <- data.frame(matrix(NA, nrow = length(sol_times), ncol = 3))
  keep_predictions[,1] <- sol_times
  colnames(keep_predictions) <- c('Time', 'C1', 'C2')
  
  age <- x[3]#days
  L = Size_estimation(age) #mm
  dry_weight =  dry_weight_estimation(L) #mg
  F_rate <- Filtration_rate_estimation(L)#mL/h
  for (i in 1:2) { #loop for the 2 different concentrations
    constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                         'k_sed'= ifelse(i==1, ksed_predicted$k_sed[1], ksed_predicted$k_sed[2]))
    fitted_params <- c("a"=x[1], "ke_2"=x[2])
    params <- c(fitted_params, constant_params)
    
    inits <- c('C_water'=C_water_0[i], 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
               'M_sed'=0)
    
    # Water is renewed during depuration after each sampling point
    refresh_moments <- rep(24,6) +c(0, 6, 12, 24, 48, 72)
    eventdat <- data.frame(var = c("C_water"),
                           time = c(refresh_moments) ,
                           value = rep(0, length(refresh_moments)),
                           method = rep("rep", (length(refresh_moments)))) 
    
    solution <- data.frame(deSolve::ode(times = sol_times,  func = ode_func, y = inits,
                                        parms = params,
                                        events = list(data = eventdat),
                                        method="lsodes",
                                        rtol = 1e-3, atol = 1e-3))
    keep_predictions[,i+1] <- solution$C_daphnia
    
  }
  
  strings <- as.character(C_water_0)
  color_codes <- scales::hue_pal()(3) # to return 2 color codes 
  cls <- c()  
  for (i in 1:length(strings)) {
    strings[i] <- paste0(strings[i], " mg/l")
    cls[i] <- color_codes[i]
    names(cls)[i] <- strings[i]
  }
  
  draw_plot <- ggplot()+
    geom_line(data = keep_predictions, aes(x=Time, y=C1, color=strings[1]), size=1.7)+
    geom_line(data = keep_predictions, aes(x=Time, y=C2, color=strings[2]), size=1.7)+
    
    geom_point(data = exp_data, aes(x=Time, y=C1, color=strings[1]), size=5)+
    geom_point(data = exp_data, aes(x=Time, y=C2, color=strings[2]), size=5)+
    #scale_y_log10()+
    
    
    labs(title = nm_type,
         y = "Concentration in Daphnia Magna (mg TiO2/mg daphnia)", x = "Time (hours)")+
    theme(plot.title = element_text(hjust = 0.5,size=30), 
          axis.title.y =element_text(hjust = 0.5,size=25,face="bold"),
          axis.text.y=element_text(size=22),
          axis.title.x =element_text(hjust = 0.5,size=25,face="bold"),
          axis.text.x=element_text(size=22),
          legend.title=element_text(hjust = 0.5,size=25), 
          legend.text=element_text(size=22)) + 
    
    scale_color_manual("TiO2 mg/l", values=cls)+
    theme(legend.key.size = unit(1.5, 'cm'),  
          legend.title = element_text(size=14),
          legend.text = element_text(size=14),
          axis.text = element_text(size = 14))
  
  
  return(draw_plot)
}

############################################################################

nm_types <- as.character(Mapping[,2])
#x0 <- runif(3)
#x0 <- c("a" = 0.2933361, "ke_2" = 0.08813155, "C_sat" = 0.2781108)
x0 <- c(0.145, 0.0748,8)

C_water_0 <- c(0.1, 1) # mg/L

opts <- list( "algorithm" = "NLOPT_LN_SBPLX" , #"NLOPT_LN_NEWUOA"
              "xtol_rel" = 1e-07,
              "ftol_rel" = 1e-07,
              "ftol_abs" = 0.0,
              "xtol_abs" = 0.0 ,
              "maxeval" = 500,
              "print_level" = 1)

set.seed(531)
optimization <- nloptr::nloptr(x0 = x0,
                               eval_f = obj_func,
                               lb	= c(0, 0,0.6),
                               ub = c(4,0.2,8),
                               opts = opts,
                               C_water_0 = C_water_0,
                               nm_types = nm_types,
                               V_water = V_water,
                               ksed_predicted=ksed_predicted)

fit_pars <- optimization$solution

alpha <-fit_pars[1]
ke_2 <- fit_pars[2]
age <- fit_pars[3]
L = Size_estimation(age) #mm
dry_weight =  dry_weight_estimation(L) #mg
F_rate <- Filtration_rate_estimation(L)#mL/h
print(alpha)
print(alpha*F_rate)
print(ke_2)
print(age)


print(paste0("TWA of water is:", 
             # Time Weighted Average
             mean(c(solution[solution$time ==0,"C_water"], solution[solution$time ==3,"C_water"], 
                    solution[solution$time ==6,"C_water"], solution[solution$time ==12,"C_water"], 
                    solution[solution$time ==23.9,"C_water"]))))

plot_func(optimization, C_water_0, nm_types, V_water, ksed_predicted)
