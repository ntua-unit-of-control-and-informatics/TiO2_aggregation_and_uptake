# This is a script to simulate the experiments in Zhu et al., 2010
# The first experiment is about the waterborne exposure of D. Magna to TiO2 nanoparticles

setwd("C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/Exposure/Zhu_2010")



# Calculate the dry weight of daphnia in mg based on Pauw et al.1981 data
weight_calc <- function(age){
  
  df <- read.csv('C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/Daphnia Magna Dry Weight - Age/weight_growth_data_Pauw_1981.csv')
  df[,1] <- round(df[,1])
  
  age_span <- df[,1]
  weight_span <- df[,2]
  
  if(age <= min(age_span)){
    dry_weight <- min(weight_span)
  }else if(age >= max(age_span)){  #Change here
    dry_weight <- max(weight_span)
  }else{ 
    dry_weight <- approx(df[,1], df[,2], age)  #Change here
  }
  return(dry_weight$y)
}

# Filtering rate of Daphnia magna is calculated based on Burns et al. 1969.
Filtration_rate_func <- function(temperature, dry_mass, dry_mass_threshold=0.034){
  # UNITS
  # temperature: C
  # dry_mass: mg 
  # F_rate: ml/h/mg dry daphnia
  
  # dry_mass_threshold is the threshold to decide if a daphnia organism should be
  # considered as juvenile or adult. According to Burns et al.1969. Daphnia 
  # organisms with length lower than(approximately) 1.5 mm are considered as juveniles.
  # Based on the given equation that relates the dry mass to the length of the 
  # organism in the same paper ( W (mg) = 0.0116*L^2.67 ), the dry mass 
  # of a daphnia with length 1.5 mm has dry mass equal to 0.034mg. So daphnia 
  # organism with dry mass greater than this threshold must be considered as adults
  # for the calculation of F_rate.
  
  # Keep ony the data for immature or adult daphnia based on the 
  # age and adulthood_threshold given.
  
  filtration_data <- read.csv('C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/Daphnia Magna Filtration Rate/Burns et al.1969 filtration rate data.csv')
  
  if (dry_mass <= dry_mass_threshold){
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
  return(F_rate$y)
}

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

# Age from start until the end of the experiment
sol_time <- seq(0,100,5)
age <- 8 + sol_time/24

# The daphnias used in water exposure experiments are between 7 and 14 days old
# We consider an average age f the daphnias equal to 10 days (Pauw et al.1981)
dry_weight <- sapply(age, weight_calc) # mg dry weight of individual daphnia 

# Units of filtration rate are ml water/h/mg dry weight of daphnia
F_rate <- sapply(dry_weight, Filtration_rate_func, temperature = 22)
# Multiply with the average dry weight (transformed into mg) of an individual.
F_rate <- F_rate*dry_weight

# V_water is the volume of water (in L) in the corresponding experiment.
# The volume remains constant during the experiment and equal to 100 ml
V_water <- 0.1 # L

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
    
    N_current <- 20
    C_sat = 10000
    
    #dry_weight_exact <- approx((age-8)*24, dry_weight, time)$y
    #F_rate_exact <- approx((age-8)*24, F_rate, time)$y
    # Some times there the which statement returns two ages
    # We always select the first
    which_age <- which( abs(age - (time/24+8)) == min(abs(age - (time/24+8))))[1]
    dry_weight_exact <-  dry_weight[which_age]
    F_rate_exact <- F_rate[which_age]
    
    C_water = M_water/V_water
    C_daphnia = M_daphnia/dry_weight_exact
    C_algae = M_algae
    
    # Algae
    dM_algae <- ku*C_water*V_water - ke_1*C_algae
    
    #  M_daphnia: TiO2 mass in D.magna
    dM_daphnia = a*(F_rate_exact/1000)*(1-C_daphnia/C_sat)*C_water-
              ke_2*C_daphnia*dry_weight_exact  +  ke_1*C_algae 
    
    # M_water: TiO2 mass in water
    dM_water <- - (a*(F_rate_exact/1000)*(1-C_daphnia/C_sat)*N_current +
                       k_sed*V_water + ku*V_water)*C_water
    # Sediment
    dM_sed = k_sed*V_water*C_water
      
    
    Mass_balance = N_current*M_daphnia+M_water+M_sed- N_current*ke_2*C_daphnia*dry_weight_exact
    
    
    return(list(c(dM_water, dM_algae, dM_daphnia,dM_sed ),"C_water" = C_water, 
                "C_daphnia" = C_daphnia,
                "F_rate_exact" = F_rate_exact, "dry_weight_exact" = dry_weight_exact,
                "N_current" = N_current, "Mass_balance" = Mass_balance))
  })
}

# obj_func function to minimize
obj_func <- function(x, C_water_0, nm_types, V_water, F_rate, dry_weight,
                     age, ksed_predicted){
  
  nm_type <- nm_types
  
  exp_data <- exposure_data
  
  sol_times <- seq(0,100,0.1)
  
  score_per_conc <- c()
  
  for (i in 1:2) { #loop for the 2 different concentrations
    constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                         'k_sed'= ifelse(i==1, ksed_predicted$k_sed[1], ksed_predicted$k_sed[2]), 
                         'ku'=0, 'ke_1'=0)
    fitted_params <- c("a"=x[1], "ke_2"=x[2], "C_sat"=x[3])
    params <- c(fitted_params, constant_params)
    
    inits <- c('M_water'= C_water_0[i]*V_water, 'M_algae'=0, 'M_daphnia'=0, 'M_sed' = 0 )
    
    # create events to force C_water=0 at time = 2 hours
    eventdat <- NULL
    
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
    
    score_per_conc[i] <- rmse(exp_data[,i+1], results)
    
  }
  
  return(mean(score_per_conc))
}


plot_func <- function(optimization, C_water_0, nm_types, V_water, F_rate, dry_weight,
                      age, ksed_predicted){
  
  library(ggplot2)
  x <- optimization$solution
  nm_type <- nm_types
  
  exp_data <- exposure_data
  
  sol_times <- seq(0,100,0.1)
  
  score_per_conc <- c()
  
  keep_predictions <- data.frame(matrix(NA, nrow = length(sol_times), ncol = 3))
  keep_predictions[,1] <- sol_times
  colnames(keep_predictions) <- c('Time', 'C1', 'C2')
  
  for (i in 1:2) { #loop for the 2 different concentrations
    constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                         'k_sed'= ifelse(i==1, ksed_predicted$k_sed[1], ksed_predicted$k_sed[2]), 
                         'ku'=0, 'ke_1'=0)
    fitted_params <- c("a"=x[1], "ke_2"=x[2], "C_sat"=x[3])
    params <- c(fitted_params, constant_params)
    
    inits <- c('M_water'= C_water_0[i]*V_water, 'M_algae'=0, 'M_daphnia'=0, 'M_sed' = 0 )
    eventdat <- NULL
    
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
x0 <- c(0.2933361, 0.08813155, 0.2781108)

C_water_0 <- c(0.1, 1) # mg/L

opts <- list( "algorithm" = "NLOPT_LN_SBPLX" , #"NLOPT_LN_NEWUOA"
              "xtol_rel" = 1e-07,
              "ftol_rel" = 1e-07,
              "ftol_abs" = 0.0,
              "xtol_abs" = 0.0 ,
              "maxeval" = 500,
              "print_level" = 1)

optimization <- nloptr::nloptr(x0 = x0,
                               eval_f = obj_func,
                               lb	= c(0, 0, 0.01),
                               ub = c(4,0.2,0.5),
                               opts = opts,
                               C_water_0 = C_water_0,
                               nm_types = nm_types,
                               V_water = V_water,
                               F_rate = F_rate, 
                               dry_weight=dry_weight,
                               age = age,
                               ksed_predicted=ksed_predicted)

fit_pars <- optimization$solution

alpha <-fit_pars[1]
ke_2 <- fit_pars[2]
C_sat <- fit_pars[3]
t_0  <- fit_pars[4] 

plot_func(optimization, C_water_0, nm_types, V_water, F_rate, dry_weight, 
          age = age, ksed_predicted)

