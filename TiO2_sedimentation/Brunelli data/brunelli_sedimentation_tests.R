setwd("C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/TiO2_sedimentation/Brunelli data")


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
  for (i in 1:4) {
    sol_times <- seq(0,60)
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
  initial_water_concentrations <- c(0.01, 0.1, 1, 10) # given in mg TiO2/L
  x0 <- runif(4, 0, 1e-03)
  N_iter <- 500
  print(exp_data)
  opts <- list( "algorithm" = "NLOPT_LN_SBPLX", #"NLOPT_LN_NEWUOA",  #"NLOPT_LN_SBPLX" ,
                "xtol_rel" = 1e-06,
                "ftol_rel" = 0.0,
                "ftol_abs" = 0.0,
                "xtol_abs" = 0.0 ,
                "maxeval" = N_iter,
                "print_level" = 1)
  
  optimization <- nloptr::nloptr(x0 = x0,
                                 eval_f = obj_function,
                                 lb	= rep(0,4),
                                 opts = opts, 
                                 exp_data = exp_data,
                                 initial_water_concentrations = initial_water_concentrations)
  
  return(optimization)
}

# A function to plot the model predictions with the experimental data of a
# specific type water
plot_func <- function(optimization, exp_data, plot_title){
  initial_water_concentrations <- c(0.01, 0.1, 1, 10) # given in mg TiO2/L
  sol_times <- seq(0,60)
  k_sed <-  optimization$solution
  mse_scores <- c()
  keep_predictions <- data.frame(matrix(data=NA, nrow = length(sol_times), ncol = dim(exp_data)[2]))
  keep_predictions[,1] <- sol_times
  colnames(keep_predictions) <- c("Time", "C1", "C2", "C3", "C4")
  for (i in 1:4) {
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
  cls <- c("0.01 mg/l"="#F8766D", "0.1 mg/l"="#7CAE00", "1 mg/l"="#00BFC4", "10 mg/l"="#C77CFF")
  
  colnames(keep_predictions) <- c("Time", "C1", "C2", "C3", "C4")
  colnames(exp_data) <- c("Time", "C1", "C2", "C3", "C4")
  
  plot <- ggplot()+
    geom_line(data = keep_predictions, aes(x=Time, y=C1, color="0.01 mg/l"), size=1.7)+
    geom_line(data = keep_predictions, aes(x=Time, y=C2, color="0.1 mg/l"), size=1.7)+
    geom_line(data = keep_predictions, aes(x=Time, y=C3, color="1 mg/l"), size=1.7)+
    geom_line(data = keep_predictions, aes(x=Time, y=C4, color="10 mg/l"), size=1.7)+
    
    geom_point(data = exp_data, aes(x=Time, y=C1, color="0.01 mg/l"), size=3)+
    geom_point(data = exp_data, aes(x=Time, y=C2, color="0.1 mg/l"), size=3)+
    geom_point(data = exp_data, aes(x=Time, y=C3, color="1 mg/l"), size=3)+
    geom_point(data = exp_data, aes(x=Time, y=C4, color="10 mg/l"), size=3)+
    
    labs(title = plot_title,
         y = "c/c0", x = "Time (hours)")+
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

#====================================================
# Load data and process the data for each type water
#====================================================

c1_data <- read.csv("C1_data.csv") # 0.01 mg/l 
c1_data[,2:7] <- exp(c1_data[,2:7])
c2_data <- read.csv("C2_data.csv") # 0.1 mg/l
c2_data[,2:7] <- exp(c2_data[,2:7])
c3_data <- read.csv("C3_data.csv") # 1 mg/l
c3_data[,2:7] <- exp(c3_data[,2:7])
c4_data <- read.csv("C4_data.csv") # 10 mg/l
c4_data[,2:7] <- exp(c4_data[,2:7])

water_types <- colnames(c1_data)[-1]

### ASW1 data for all concentrations and optimization ###
asw1_data <- data.frame(cbind(c1_data$Time, c1_data$ASW1, 
                              c2_data$ASW1, c3_data$ASW1, c4_data$ASW1))
colnames(asw1_data) <- c("Time", "C1", "C2", "C3", "C4")

asw1_optimization <- optimization_func(asw1_data)
asw1_plot <- plot_func(asw1_optimization, asw1_data, 
                      "ASW1 sedimentation at different TiO2 concentrations")
asw1_ksed <-  asw1_optimization$solution

### ASW2 data for all concentrations ###
asw2_data <- data.frame(cbind(c1_data$Time, c1_data$ASW2, 
                              c2_data$ASW2, c3_data$ASW2, c4_data$ASW2))
colnames(asw2_data) <- c("Time", "C1", "C2", "C3", "C4")

asw2_optimization <- optimization_func(asw2_data)
asw2_plot <- plot_func(asw2_optimization, asw2_data, 
                       "ASW2 sedimentation at different TiO2 concentrations")
asw2_ksed <-  asw2_optimization$solution

# AEW data for all concentrations
aew_data <- data.frame(cbind(c1_data$Time, c1_data$AEW, 
                              c2_data$AEW, c3_data$AEW, c4_data$AEW))
colnames(aew_data) <- c("Time", "C1", "C2", "C3", "C4")

aew_optimization <- optimization_func(aew_data)
aew_plot <- plot_func(aew_optimization, aew_data, 
                       "AEW sedimentation at different TiO2 concentrations")
aew_ksed <-  aew_optimization$solution

### AFW data for all concentrations ###
afw_data <- data.frame(cbind(c1_data$Time, c1_data$AFW, 
                             c2_data$AFW, c3_data$AFW, c4_data$AFW))
colnames(afw_data) <- c("Time", "C1", "C2", "C3", "C4")

afw_optimization <- optimization_func(afw_data)
afw_plot <- plot_func(afw_optimization, afw_data, 
                      "AFW sedimentation at different TiO2 concentrations")
afw_ksed <-  afw_optimization$solution

# LW data for all concentrations
lw_data <- data.frame(cbind(c1_data$Time, c1_data$LW, 
                             c2_data$LW, c3_data$LW, c4_data$LW))
colnames(lw_data) <- c("Time", "C1", "C2", "C3", "C4")

lw_optimization <- optimization_func(lw_data)
lw_plot <- plot_func(lw_optimization, lw_data, 
                      "LW sedimentation at different TiO2 concentrations")
lw_ksed <-  lw_optimization$solution

# SW data for all concentrations
sw_data <- data.frame(cbind(c1_data$Time, c1_data$SW, 
                             c2_data$SW, c3_data$SW, c4_data$SW))
colnames(sw_data) <- c("Time", "C1", "C2", "C3", "C4")

sw_optimization <- optimization_func(sw_data)
sw_plot <- plot_func(sw_optimization, sw_data, 
                     "SW sedimentation at different TiO2 concentrations")
sw_ksed <-  sw_optimization$solution
