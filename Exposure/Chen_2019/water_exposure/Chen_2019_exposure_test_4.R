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

# Load the data for concentration = 1.0 mg/l
C2_data <- read.csv('data/1_water_exposure.csv')
colnames(C2_data)[-1] <- Mapping$Type

# Load the data for concentration = 10.0 mg/l
C3_data <- read.csv('data/10_water_exposure.csv')
colnames(C3_data)[-1] <- Mapping$Type

##########################################################

# *** mse_custom ***

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
    # C_algae: mg TiO2 / kg algae 
    # C_daphnia: mg TiO2 / kg daphnia (same as data)
    # k_sed: 1/h
    # ku: 1/h
    # F_rate: L water/kg daphnia 
    # ke_2: 1/h
    
    # C_water: TiO2 concentration in water
    dC_water <- - (k_sed + ku)*C_water
    
    # Algae
    dC_algae <- ku*C_water - ke_1*C_algae
    
    # Daphnia magna
    dC_daphnia = F_rate*C_water + I*C_algae - ke_2*C_daphnia 
    
    return(list(c(dC_water, dC_algae, dC_daphnia)))
  })
}

# obj_func needs the x vector (2 values) and the nm_type
obj_func <- function(x, C_water_0, nm_type){
  
  exp_data <- cbind(C1_data["Time"], C1_data[nm_type], C2_data[nm_type], C3_data[nm_type])
  colnames(exp_data) <- c('Time', 'C1', 'C2', 'C3')
  
  constant_params <- c('k_sed'=0.02, 'ku'=0, 'ke_1'=0, 'I'=0)
  fitted_params <- c("F_rate"=x[1], "ke_2"=x[2])
  
  sol_times <- seq(0,50, 0.5)
  
  scores <- c()
  
  for (i in 1:3) { #loop for the 3 different concentrations
    
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
    
    scores[i] <- rmse(exp_data[,i+1], results)
    
  }
  return(mean(scores))
}


plot_func <- function(optimization, C_water_0, plot_title){
  library(ggplot2)
  x <- optimization$solution
  #plots
  library(ggplot2)
  exp_data <- cbind(C1_data["Time"], C1_data[nm_type], C2_data[nm_type], C3_data[nm_type])
  colnames(exp_data) <- c('Time', 'C1', 'C2', 'C3')
  
  fitted_params <- c("F_rate"=x[1], "ke_2"=x[2])
  constant_params <- c('k_sed'=0.02, 'ku'=0, 'ke_1'=0, 'I'=0)
  
  sol_times <- seq(0,50, 0.5)
  
  mape_scores <- c()
  refresh_moments <- seq(3,21)
  
  keep_predictions <- data.frame(matrix(NA, nrow = length(sol_times), ncol = 4))
  keep_predictions[,1] <- sol_times
  colnames(keep_predictions) <- c('Time', 'C1', 'C2', 'C3')
  
  for (i in 1:3) { #loop for the 3 different concentrations
    
    params <- c(fitted_params, constant_params)
    
    inits <- c('C_water'=C_water_0[i], 'C_algae'=0, 'C_daphnia'=0 )
    
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
  
  head(solution)
  exp_data
  
  strings <- as.character(C_water_0)
  color_codes <- scales::hue_pal()(3) # to return 5 color codes 
  cls <- c()  
  for (i in 1:length(strings)) {
    strings[i] <- paste0(strings[i], " mg/ml")
    cls[i] <- color_codes[i]
    names(cls)[i] <- strings[i]
  }
  
  ggplot()+
    geom_line(data = keep_predictions, aes(x=Time, y=C1, color=strings[1]), size=1.7)+
    geom_line(data = keep_predictions, aes(x=Time, y=C2, color=strings[2]), size=1.7)+
    geom_line(data = keep_predictions, aes(x=Time, y=C3, color=strings[3]), size=1.7)+
    
    geom_point(data = exp_data, aes(x=Time, y=C1, color=strings[1]), size=3)+
    geom_point(data = exp_data, aes(x=Time, y=C2, color=strings[2]), size=3)+
    geom_point(data = exp_data, aes(x=Time, y=C3, color=strings[3]), size=3)+
    #scale_y_log10()
    
    
    labs(title = plot_title,
         y = "Concentration in Daphnia Magna (mg TiO2/kg daphnia)", x = "Time (hours)")+
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
}

take_results <- function(nm_type, N_iter){
  x0 <- runif(2)
  C_water_0 <- c(0.1, 1, 10) # mg/L
  #N_iter <- 3500
  
  opts <- list( "algorithm" = "NLOPT_LN_SBPLX" , #"NLOPT_LN_NEWUOA"
                "xtol_rel" = 0,
                "ftol_rel" = 0.0,
                "ftol_abs" = 0.0,
                "xtol_abs" = 0.0 ,
                "maxeval" = N_iter,
                "print_level" = 1)
  
  optimization <- nloptr::nloptr(x0 = x0,
                                 eval_f = obj_func,
                                 lb	= rep(1e-02,length(x0)),
                                 opts = opts,
                                 C_water_0 = C_water_0,
                                 nm_type = nm_type)
  
  
  
  fitted_params <- optimization$solution 
  
  params_df <- data.frame(fitted_params)
  rownames(params_df) <- c("F_rate", "ke_2")
  
  results_plot <- plot_func(optimization, C_water_0, nm_type)
  
  return(list("optimization" = optimization,
              "Fitted_params" = params_df,
              "plot"=results_plot))
  
}

################################################################################

# TiO2-5A
nm_types <- as.character(Mapping[,2])

# TiO2-5A
nm_type <- nm_types[1]
fit_A <- take_results(nm_type, 5000)
fit_A 

# TiO2-10A
nm_type <- nm_types[2]
fit_B <- take_results(nm_type, 5000)
fit_B 

# TiO2-100A
nm_type <- nm_types[3]
fit_C <- take_results(nm_type, 5000)
fit_C 

# TiO2-P25
nm_type <- nm_types[4]
fit_D <- take_results(nm_type, 5000)
fit_D 

# TiO2-25R
nm_type <- nm_types[5]
fit_E <- take_results(nm_type, 5000)
fit_E 
