# This is a script to simulate the experiments in Cehn et al., 2019
# The first experiment is about the waterborne exposure of D. Magna to TiO2 nanoparticles
# The second is about the trophic exposure to TiO2 exposured algae.

# Working directory
setwd('C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/Exposure/Chen_2019/water_exposure')

#=================#
#  Water exposure #
#=================#

# The following dataframe is a mappin of the of the TiO2 NPs types with 
# their corresponding codes (which are simple letters)
A <- c("A", "TiO2-5A")
B <- c("B", "TiO2-10A")
C <- c("C", "TiO2-100A")
D <- c("D", "TiO2-P25")
E <- c("E", "TiO2-25R")

Mapping <- data.frame(rbind(A, B, C, D, E))
colnames(Mapping) <- c("Code", "Type")
i <- 1:dim(Mapping)[2]
Mapping[,i] <- apply(Mapping[,i], 2,
                     function(x) as.character(x))
sapply(Mapping, class)

# Load data only for water exposure scenario

# The values are reported as mg TiO2/kg of daphnia

# Load the data for concentration = 0.1 mg/l
C1_data <- read.csv('data/0.1_water_exposure.csv')
colnames(C1_data)[-1] <- Mapping$Type
C1_data[,2:6] <-  C1_data[,2:6]/1e06 # transform from mg/kg daphnia to mg/mg daphnia

# Load the data for concentration = 1.0 mg/l
C2_data <- read.csv('data/1_water_exposure.csv')
colnames(C2_data)[-1] <- Mapping$Type
C2_data[,2:6] <-  C2_data[,2:6]/1e06 # transform from mg/kg daphnia to mg/mg daphnia

# Load the data for concentration = 10.0 mg/l
C3_data <- read.csv('data/10_water_exposure.csv')
colnames(C3_data)[-1] <- Mapping$Type
C3_data[,2:6] <-  C3_data[,2:6]/1e06 # transform from mg/kg daphnia to mg/mg daphnia

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
dry_weight <- weight_calc(10) # mg dry weight of individual daphnia 

# V_water is the volume of water (in L) in the corresponding experiment.
# The volume remains constant during the experiment and equal to 100 ml
V_water <- 0.1 # L

# Population of daphnias during the experiments
# At each measurement 10 daphnias were removed from the system
N <- seq(80,10,-10)

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
  
  filtration_data <- read.csv('C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/Daphnia Magna Filtration Rate/Burns et al.1969 filtration rate data.csv')
  
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
# Units of filtration rate are ml water/h/mg dry weight of daphnia
F_rate <- Filtration_rate_func(22, dry_weight)$y
# Multiply with the average dry weight (transformed into mg) of an individual.
F_rate <- F_rate*dry_weight

# Load the predicted ksed values
ksed_predicted <- read.csv("Chen_2019_ksed_predictions.csv")
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
    N <- seq(80,10,-10)
    if (time < 3){
      N_current <- N[1]
    }else if (3 <= time & time < 6){
      N_current <- N[2]
    }else if (6 <= time & time < 12){
      N_current <- N[3]
    }else if (12 <= time & time < 24){
      N_current <- N[4]
    }else if (24 <= time & time < 27){
      N_current <- N[5]
    }else if (27 <= time & time < 30){
      N_current <- N[6]
    }else if (30 <= time & time < 36){
      N_current <- N[7]
    }else if (36 <= time & time ){
      N_current <- N[8]
    }
    
    # C_water: TiO2 concentration in water
    dC_water <- - (a*(F_rate/1000)*N_current/V_water + k_sed + ku)*C_water
    
    # Algae
    dC_algae <- ku*C_water - ke_1*C_algae
    
    # Daphnia magna
    dC_daphnia = a*(F_rate/1000)*C_water/dry_weight + F_rate*C_algae - ke_2*C_daphnia 
    
    return(list(c(dC_water, dC_algae, dC_daphnia)))
  })
}

# obj_func needs the x vector (2 values) and the nm_type
obj_func <- function(x, C_water_0, nm_types, V_water, F_rate, dry_weight, ksed_predicted){
  
  score_per_type <- c()
  
  for (j in 1:length(nm_types)) {
    nm_type <- nm_types[j]
    
    exp_data <- cbind(C1_data["Time"], C1_data[nm_type], C2_data[nm_type], C3_data[nm_type])
    colnames(exp_data) <- c('Time', 'C1', 'C2', 'C3')
    
    triplette_alphas <- c(x[(3*j-2) : (3*j)])

    sol_times <- seq(0,50, 0.5)
    
    score_per_conc <- c()
    
    for (i in 1:3) { #loop for the 3 different concentrations
      constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                           'k_sed'= ksed_predicted[which(ksed_predicted$Name==nm_type & ksed_predicted$`Concentration_mg/L`==C_water_0[i]),"k_sed"], 
                           'ku'=0, 'ke_1'=0)
      fitted_params <- c("a"=triplette_alphas[i], "ke_2"=x[16])
      params <- c(fitted_params, constant_params)
      
      inits <- c('C_water'=C_water_0[i], 'C_algae'=0, 'C_daphnia'=0 )
      refresh_moments <- seq(3,21)
      eventdat <- data.frame(var = c("C_water"),
                             time = c(refresh_moments,24) ,
                             value = c(rep(inits[1], length(refresh_moments)), 0),
                             method = c(rep("rep", (length(refresh_moments)+1)))
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
    
    score_per_type[j] <- mean(score_per_conc)
    
  }
  
  return(mean(score_per_type))
}


plot_func <- function(optimization, C_water_0, nm_types, V_water, F_rate, dry_weight, ksed_predicted){
  
  library(ggplot2)
  x <- optimization$solution
  plots_list <- list()
  for (j in 1:length(nm_types)) {
    nm_type <- nm_types[j]
    #plots
    exp_data <- cbind(C1_data["Time"], C1_data[nm_type], C2_data[nm_type], C3_data[nm_type])
    colnames(exp_data) <- c('Time', 'C1', 'C2', 'C3')
    
    triplette_alphas <- c(x[(3*j-2) : (3*j)])
    
    sol_times <- seq(0,50, 0.5)
    
    keep_predictions <- data.frame(matrix(NA, nrow = length(sol_times), ncol = 4))
    keep_predictions[,1] <- sol_times
    colnames(keep_predictions) <- c('Time', 'C1', 'C2', 'C3')
    
    for (i in 1:3) { #loop for the 3 different concentrations
      constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                           'k_sed'= ksed_predicted[which(ksed_predicted$Name==nm_type & ksed_predicted$`Concentration_mg/L`==C_water_0[i]),"k_sed"], 
                           'ku'=0, 'ke_1'=0, 'I'=0)
      fitted_params <- c("a"=triplette_alphas[i], "ke_2"=x[16])
      
      params <- c(fitted_params, constant_params)
      
      inits <- c('C_water'=C_water_0[i], 'C_algae'=0, 'C_daphnia'=0 )
      refresh_moments <- seq(3,21)
      eventdat <- data.frame(var = c("C_water"),
                             time = c(refresh_moments,24) ,
                             value = c(rep(inits[1], length(refresh_moments)), 0),
                             method = c(rep("rep", (length(refresh_moments)+1)))
      )
      
      solution <- data.frame(deSolve::ode(times = sol_times,  func = ode_func, y = inits,
                                          parms = params,
                                          events = list(data = eventdat),
                                          method="lsodes",
                                          rtol = 1e-3, atol = 1e-3))
      
      keep_predictions[,i+1] <- solution$C_daphnia
      
    }
    
    strings <- as.character(C_water_0)
    color_codes <- scales::hue_pal()(3) # to return 5 color codes 
    cls <- c()  
    for (i in 1:length(strings)) {
      strings[i] <- paste0(strings[i], " mg/l")
      cls[i] <- color_codes[i]
      names(cls)[i] <- strings[i]
    }
    
    draw_plot <- ggplot()+
      geom_line(data = keep_predictions, aes(x=Time, y=C1, color=strings[1]), size=1.7)+
      geom_line(data = keep_predictions, aes(x=Time, y=C2, color=strings[2]), size=1.7)+
      geom_line(data = keep_predictions, aes(x=Time, y=C3, color=strings[3]), size=1.7)+
      
      geom_point(data = exp_data, aes(x=Time, y=C1, color=strings[1]), size=5)+
      geom_point(data = exp_data, aes(x=Time, y=C2, color=strings[2]), size=5)+
      geom_point(data = exp_data, aes(x=Time, y=C3, color=strings[3]), size=5)+
      #scale_y_log10()+
      
      
      labs(title = nm_types[j],
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
    
    plots_list[[j]] <- draw_plot
  }
  return(plots_list)
}

take_results <- function(nm_types, N_iter){
  x0 <- rep(1e-02,16)
  C_water_0 <- c(0.1, 1, 10) # mg/L
  
  opts <- list( "algorithm" = "NLOPT_LN_SBPLX" , #"NLOPT_LN_NEWUOA"
                "xtol_rel" = 1e-07,
                "ftol_rel" = 0.0,
                "ftol_abs" = 0.0,
                "xtol_abs" = 0.0 ,
                "maxeval" = N_iter,
                "print_level" = 1)
  
  optimization <- nloptr::nloptr(x0 = x0,
                                 eval_f = obj_func,
                                 #lb	= rep(1e-5,length(x0)),
                                 ub = c(rep(1,15), 1),
                                 opts = opts,
                                 C_water_0 = C_water_0,
                                 nm_types = nm_types,
                                 V_water = V_water,
                                 F_rate = F_rate, 
                                 dry_weight=dry_weight,
                                 ksed_predicted=ksed_predicted)
  
  
  
  fitted_params <- optimization$solution

  alphas_df <- data.frame(matrix(fitted_params[1:15], byrow = T, nrow = 5) )
  colnames(alphas_df) <- c('0.1 mg/ml', '1.0 mg/ml', '10 mg/ml')
  rownames(alphas_df) <- nm_types
  ke_2 <- fitted_params[16]
  results_plots <- plot_func(optimization, C_water_0, nm_types,
                             V_water = V_water,
                             F_rate = F_rate, 
                             dry_weight=dry_weight,
                             ksed_predicted=ksed_predicted)
  
  return(list("optimization" = optimization,
              "alphas_df" = alphas_df,
              "ke_2"=ke_2,
              "plot"=results_plots))
  
}
################################################################################
nm_types <- as.character(Mapping[,2])

fit <- take_results(nm_types, 1000)
