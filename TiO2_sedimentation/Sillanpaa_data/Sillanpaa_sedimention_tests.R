setwd("C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/TiO2_sedimentation/Sillanpaa_data")

# A simple ODE to describe the sedimentation effect on the TiO2 concentration
sed_function <- function(time, inits, params){
  with(as.list(c(inits, params)),{
    
    dC_water <- - k_sed*C_water
    
    list(dC_water)
  })
}

# The metric used for the optimization
mse_custom <- function(observed, predicted){
  mean((observed - predicted)^2)
}


# The objective function to minimize
obj_function <- function(x, exp_data, initial_water_concentrations){
  k_sed <- x
  mse_scores <- c()
  for (i in 1:length(k_sed)) {
    sol_times <- seq(0, 350, 0.1)
    params <- c("k_sed"=k_sed[i])
    inits <- c("C_water" = initial_water_concentrations[i])
    solution <- data.frame(deSolve::ode(times = sol_times,  func = sed_function, y = inits,
                                        parms = params,
                                        method="lsodes",
                                        rtol = 1e-3, atol = 1e-3))
    results <- data.frame(solution$time, solution$C_water/inits)  # divide with C0 to calculate the fraction C/C0  
    colnames(results) <- c("Time", "Remaining_Concentration_fraction")
    keep_predictions <- results[results$Time %in% exp_data$Time,]
    mse_scores[i] <- mse_custom(exp_data[,i+1], keep_predictions$Remaining_Concentration_fraction)
  }
  minimize <- mean(mse_scores)
  return(minimize)
}

# A function that handles the optimization 
optimization_func <- function(exp_data){
  # Inputs
  initial_water_concentrations <- c(10, 100) # given in mg TiO2/L
  x0 <- runif(2, 0, 1e-03)
  N_iter <- 500
  opts <- list( "algorithm" = "NLOPT_LN_SBPLX", #"NLOPT_LN_NEWUOA",  #"NLOPT_LN_SBPLX" ,
                "xtol_rel" = 1e-06,
                "ftol_rel" = 0.0,
                "ftol_abs" = 0.0,
                "xtol_abs" = 0.0 ,
                "maxeval" = N_iter,
                "print_level" = 1)
  
  optimization <- nloptr::nloptr(x0 = x0,
                                 eval_f = obj_function,
                                 lb	= rep(0,2),
                                 opts = opts, 
                                 exp_data = exp_data,
                                 initial_water_concentrations = initial_water_concentrations)
  
  return(optimization)
}

# A function to plot the model predictions with the experimental data of a
# specific type water
plot_func <- function(optimization, exp_data, plot_title){
  initial_water_concentrations <- c(10, 100) # given in mg TiO2/L
  sol_times <- seq(0, 350, 0.1)
  k_sed <-  optimization$solution
  mse_scores <- c()
  keep_predictions <- data.frame(matrix(data=NA, nrow = length(sol_times), ncol = dim(exp_data)[2]))
  keep_predictions[,1] <- sol_times
  colnames(keep_predictions) <- c("Time", "C1", "C2")
  for (i in 1:2) {
    params <- c("k_sed" = k_sed[i])
    inits <- c("C_water" = initial_water_concentrations[i])
    solution <- data.frame(deSolve::ode(times = sol_times,  func = sed_function, y = inits,
                                        parms = params,
                                        method="lsodes",
                                        rtol = 1e-3, atol = 1e-3))
    results <- data.frame(solution$time, solution$C_water/inits)  
    colnames(results) <- c("Time", "Remaining_Concentration_fraction")
    keep_predictions[,i+1] <- results[,2]
  }
  
  library(ggplot2)
  cls <- c("10 mg/l"="#F8766D", "100 mg/l"="#7CAE00")
  
  colnames(keep_predictions) <- c("Time", "C1", "C2")
  colnames(exp_data) <- c("Time", "C1", "C2")
  
  plot <- ggplot()+
    geom_line(data = keep_predictions, aes(x=Time, y=C1, color="10 mg/l"), size=1.7)+
    geom_line(data = keep_predictions, aes(x=Time, y=C2, color="100 mg/l"), size=1.7)+

    geom_point(data = exp_data, aes(x=Time, y=C1, color="10 mg/l"), size=3)+
    geom_point(data = exp_data, aes(x=Time, y=C2, color="100 mg/l"), size=3)+

    labs(title = plot_title,
         y = "c/c0", x = "Time (minutes)")+
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
  
  return(plot)
}



#============================================================
# Load data and process the data for each type concentration
#============================================================

# Load the data for 10mg/l data
c1_data <- read.csv("10mgml_data.csv")
# Time is given in minutes
#c1_data$Time <- c1_data$Time/60
# The data for Iso freshwater report a C/C0 fraction which is 
# greater than 1. This value is impossible and maybe means that the 
# sedimented amount of TiO2 is neglible. So we trasnform the values which are
# greater than 1, equal to  1 (so the ksed = 0)
c1_data$Iso <- 1

# Load the data for 100 mg/l data
c2_data <- read.csv("100mgml_data.csv")
# Time is given in minutes
#c2_data$Time <- c2_data$Time/60

# Gather data for Simijarvi
simijarvi_data <- data.frame(cbind(c1_data$Time, c1_data$Simijarvi, c2_data$Simijarvi))
colnames(simijarvi_data) <- c("Time", "C1", "C2")

simijarvi_optimization <- optimization_func(simijarvi_data)
simijarvi_plot <- plot_func(simijarvi_optimization, simijarvi_data, 
                       "Simijarvi sedimentation at different TiO2 concentrations")
simijarvi_ksed <-  simijarvi_optimization$solution*60

# Gather data for Iso Lehmalampi
iso_data <- data.frame(cbind(c1_data$Time, c1_data$Iso, c2_data$Iso))
colnames(iso_data) <- c("Time", "C1", "C2")

iso_optimization <- optimization_func(iso_data)
iso_plot <- plot_func(iso_optimization, iso_data, 
                            "Iso Lehmalampi sedimentation at different TiO2 concentrations")
iso_ksed <-  iso_optimization$solution*60
