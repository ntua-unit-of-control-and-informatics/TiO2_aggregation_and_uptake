# This is a script to simulate the experiments in Cehn et al., 2019
# The first experiment is about the waterborne exposure of D. Magna to TiO2 nanoparticles
# The second is about the trophic exposure to TiO2 exposured algae.

# Working directory
# dir = 'C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/Exposure/Chen_2019/water_exposure'
# dir_uptake <- 'C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/'
# dir_filtration <- 'C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/'

dir_uptake <- 'C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/'
dir_filtration <- 'C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/'
dir <- 'C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/Exposure/Chen_2019/water_exposure'
setwd(dir)

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

weight_calc <- function(age, directory){
  setwd(directory)
  df <- read.csv('Daphnia Magna Dry Weight - Age/weight_growth_data_Pauw_1981.csv')
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

# The daphnias used in water exposure experiments are between 7 and 14 days old
# We consider an average age f the daphnias equal to 10 days (Pauw et al.1981)
dry_weight <- weight_calc(12, dir_uptake) # mg dry weight of individual daphnia 

# V_water is the volume of water (in L) in the corresponding experiment.
# The volume remains constant during the experiment and equal to 100 ml
V_water <- 0.1 # L

# Population of daphnias during the experiments
# At each measurement 10 daphnias were removed from the system
N <- seq(80,10,-10)

# Filtering rate of Daphnia magna is calculated based on Burns et al. 1969.
Filtration_rate_func <- function(temperature, dry_mass, directory, dry_mass_threshold=0.034){
  setwd(directory)
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
  
  filtration_data <- read.csv('Daphnia Magna Filtration Rate/Burns et al.1969 filtration rate data.csv')
  
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
# Units of filtration rate are ml water/h/mg dry weight of daphnia
F_rate <- Filtration_rate_func(22, dry_weight,dir_filtration)
# Multiply with the average dry weight (transformed into mg) of an individual.
F_rate <- F_rate*dry_weight

setwd(dir)
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
    # C_daphnia: mg TiO2 / mg daphnia 
    # k_sed: 1/h
    # F_rate: L water/h/individual daphnia 
    # ke_2: 1/h
    
    # Number of Daphnids per beaker
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
    }else if (36 <= time ){
      N_current <- N[8]
    }
    
    # C_water: TiO2 concentration in water
    dC_water <- -(N_current*a*(F_rate/1000)*(1-C_daphnia/C_sat)*C_water)/V_water-
                    k_sed*C_water
    
    # Daphnia magna
    dC_daphnia = a*(F_rate/1000)*(1-C_daphnia/C_sat)*C_water/dry_weight - ke_2*C_daphnia 
    
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

# obj_func needs the x vector (2 values) and the nm_type
obj_func <- function(x, C_water_0, nm_types, V_water, F_rate, dry_weight, ksed_predicted){
  
  score_per_type <- c()
  
  for (j in 1:length(nm_types)) {
    nm_type <- nm_types[j]
    
    exp_data <- cbind(C1_data["Time"], C1_data[nm_type], C2_data[nm_type], C3_data[nm_type])
    colnames(exp_data) <- c('Time', 'C1', 'C2', 'C3')
    
    sub_a <- x[1:5]
    a <- sub_a[j] 
    sub_k <- x[6:10]
    ke_2 <- sub_k[j] 
    sub_C_sat <- x[11:15]
    C_sat <- sub_C_sat[j]
    
    sol_times <- seq(0,50, 0.5)
    
    score_per_conc <- c()
    
    for (i in 1:3) { #loop for the 3 different concentrations
      constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                           'k_sed'= ksed_predicted[which(ksed_predicted$Name==nm_type & ksed_predicted$`Concentration_mg/L`==C_water_0[i]),"k_sed"])
      fitted_params <- c("a"=a, "ke_2"=ke_2, "C_sat"=C_sat)
      params <- c(fitted_params, constant_params)
      
      inits <- c('C_water'=C_water_0[i], 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
                 'M_sed'=0)
      
      refresh_moments <- seq(3,21, 3)
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
    
    sub_a <- x[1:5]
    a <- sub_a[j] 
    sub_k <- x[6:10]
    ke_2 <- sub_k[j] 
    sub_C_sat <- x[11:15]
    C_sat <- sub_C_sat[j]    
    sol_times <- seq(0,50, 0.5)
    
    keep_predictions <- data.frame(matrix(NA, nrow = length(sol_times), ncol = 4))
    keep_predictions[,1] <- sol_times
    colnames(keep_predictions) <- c('Time', 'C1', 'C2', 'C3')
    
    for (i in 1:3) { #loop for the 3 different concentrations
      constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                           'k_sed'= ksed_predicted[which(ksed_predicted$Name==nm_type & ksed_predicted$`Concentration_mg/L`==C_water_0[i]),"k_sed"])
      fitted_params <- c("a"=a, "ke_2"=ke_2, "C_sat"=C_sat)
      
      params <- c(fitted_params, constant_params)
      
      inits <- c('C_water'=C_water_0[i], 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
                 'M_sed'=0)
      
      refresh_moments <- seq(3,21, 3)
      eventdat <- data.frame(var = c("C_water"),
                             time = c(refresh_moments,24) ,
                             value = c(rep(inits[1], length(refresh_moments)), 0),
                             method = c(rep("rep", (length(refresh_moments)+1)))
      )
      
      solution <<- data.frame(deSolve::ode(times = sol_times,  func = ode_func, y = inits,
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



################################################################################
nm_types <- as.character(Mapping[,2])

x0 <- c(rep(1,5), rep(0.001, 5), rep(0.2,5))
C_water_0 <- c(0.1, 1, 10) # mg/L

opts <- list( "algorithm" = "NLOPT_LN_SBPLX" , #"NLOPT_LN_NEWUOA"
              "xtol_rel" = 1e-07,
              "ftol_rel" = 1e-07,
              "ftol_abs" = 0.0,
              "xtol_abs" = 0.0 ,
              "maxeval" = 4000,
              "print_level" = 1)
set.seed(1515)
optimization <- nloptr::nloptr(x0 = x0,
                               eval_f = obj_func,
                               lb	= c(rep(0,10), c(0.17,0.16,0.16,0.16,0.12)),
                               ub = c(rep(4,5), rep(1,5), rep(0.4,5)),
                               opts = opts,
                               C_water_0 = C_water_0,
                               nm_types = nm_types,
                               V_water = V_water,
                               F_rate = F_rate, 
                               dry_weight=dry_weight,
                               ksed_predicted=ksed_predicted)



fitted_params <- optimization$solution

params_values <- data.frame(matrix(fitted_params, nrow = 3, byrow = T)) 
colnames(params_values) <- nm_types
rownames(params_values) <- c('a', 'ke_2', 'C_sat')

results_plots <- plot_func(optimization, C_water_0, nm_types,
                           V_water = V_water,
                           F_rate = F_rate, 
                           dry_weight=dry_weight,
                           ksed_predicted=ksed_predicted)

results_plots
