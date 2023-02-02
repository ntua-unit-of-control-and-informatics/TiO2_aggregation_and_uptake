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
  }else if(age <= min(age_span)){
    dry_weight <- max(weight_span)
  }else{ 
    index <- which.min(abs(age - age_span))
    dry_weight <- weight_span[index]    
  }
  return(dry_weight)
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
  return(F_rate)
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

# The daphnias used in water exposure experiments are between 7 and 14 days old
# We consider an average age f the daphnias equal to 10 days (Pauw et al.1981)
dry_weight <- weight_calc(8) # mg dry weight of individual daphnia 

# V_water is the volume of water (in L) in the corresponding experiment.
# The volume remains constant during the experiment and equal to 100 ml
V_water <- 0.1 # L

# Population of daphnias during the experiments
# At each measurement 10 daphnias were removed from the system
#N <- seq(80,10,-10)

# Units of filtration rate are ml water/h/mg dry weight of daphnia
F_rate <- Filtration_rate_func(22, dry_weight)$y
# Multiply with the average dry weight (transformed into mg) of an individual.
F_rate <- F_rate*dry_weight

# Load the predicted ksed values
ksed_predicted <- read.csv("C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/Exposure/Zhu_2010/data/Zhu_2010_ksed_predictions.csv")
ksed_predicted <- ksed_predicted[, c(1,4,6)]
colnames(ksed_predicted) <- c("Name", "Concentration_mg/L", "k_sed")


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
    
    N <- seq(180,20,-20)
    if (time < 3){
      N_current <- N[1] #180
    }else if (3 <= time & time < 6){
      N_current <- N[2] #160
    }else if (6 <= time & time < 12){
      N_current <- N[3] #140
    }else if (12 <= time & time < 24){
      N_current <- N[4] #120
    }else if (24 <= time & time < 30){
      N_current <- N[5] #100
    }else if (30 <= time & time < 36){
      N_current <- N[6] #80
    }else if (36 <= time & time < 48){
      N_current <- N[7] #60
    }else if (48 <= time & time < 72){
      N_current <- N[8] #40
    }else if (72 <= time ){
      N_current <- N[9] #20
    }
    
    
    # C_water: TiO2 concentration in water
    dC_water <- - (a*(F_rate/1000)*(1-C_daphnia/C_sat)*N_current/V_water + k_sed + ku)*C_water
    
    # Algae
    dC_algae <- ku*C_water - ke_1*C_algae
    
    # Daphnia magna
    dC_daphnia = a*(F_rate/1000)*(1-C_daphnia/C_sat)*C_water/dry_weight + F_rate*C_algae - ke_2*C_daphnia  
    
    return(list(c(dC_water, dC_algae, dC_daphnia)))
  })
}

# obj_func function to minimize
obj_func <- function(x, C_water_0, nm_types, V_water, F_rate, dry_weight, ksed_predicted){
  
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
    
    inits <- c('C_water'=C_water_0[i], 'C_algae'=0, 'C_daphnia'=0 )
    
    # create events to force C_water=0 at time = 2 hours
    eventdat <- data.frame(var = c("C_water"),
                           time = 24,
                           value = 0,
                           method = 'rep'
    )

    
    solution <- data.frame(deSolve::ode(times = sol_times,  func = ode_func, y = inits,
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


plot_func <- function(optimization, C_water_0, nm_types, V_water, F_rate, dry_weight, ksed_predicted){
  
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
    
    inits <- c('C_water'=C_water_0[i], 'C_algae'=0, 'C_daphnia'=0 )
    
    # create events to force C_water=0 at time = 2 hours
    eventdat <- data.frame(var = c("C_water"),
                           time = 24,
                           value = 0,
                           method = 'rep'
    )
    # refresh_moments <- seq(3,21)
    # eventdat <- data.frame(var = c("C_water"),
    #                        time = c(refresh_moments,24) ,
    #                        value = c(rep(inits[1], length(refresh_moments)), 0),
    #                        method = c(rep("rep", (length(refresh_moments)+1)))
    # )
    
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
x0 <- c(0.2933361, 0.08813155, 0.2781108)
C_water_0 <- c(0.1, 1) # mg/L

opts <- list( "algorithm" = "NLOPT_LN_SBPLX" , #"NLOPT_LN_NEWUOA"
              "xtol_rel" = 1e-07,
              "ftol_rel" = 1e-07,
              "ftol_abs" = 0.0,
              "xtol_abs" = 0.0 ,
              "maxeval" = 100,
              "print_level" = 1)

optimization <- nloptr::nloptr(x0 = x0,
                               eval_f = obj_func,
                               lb	= c(0, 0, 0.001),
                               #ub = c(4,1,0.5),
                               opts = opts,
                               C_water_0 = C_water_0,
                               nm_types = nm_types,
                               V_water = V_water,
                               F_rate = F_rate, 
                               dry_weight=dry_weight,
                               ksed_predicted=ksed_predicted)

fitted_params <- optimization$solution

alphas_df <- data.frame(matrix(fitted_params[1:18], byrow = T, nrow = 6) )
colnames(alphas_df) <- c('0.1 mg/ml', '1.0 mg/ml', '10 mg/ml')
rownames(alphas_df) <- nm_types
ke_2 <- fitted_params[19]

plot_func(optimization, C_water_0, nm_types, V_water, F_rate, dry_weight, ksed_predicted)

