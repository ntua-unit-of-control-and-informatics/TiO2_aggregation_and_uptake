setwd("C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/TiO2_sedimentation/Keller_data")

mesocosm_data <- read.csv("Keller_mesocosm_sedimentation.csv")
mesocosm_data[,1] <- round(mesocosm_data[,1])
colnames(mesocosm_data) <- c("Time", "C/C0")

sed_function <- function(time, inits, params){
  with(as.list(c(inits, params)),{
    
    dC_water <- - k_sed*C_water
    
    list(dC_water)
  })
}

mse_custom <- function(observed, predicted){
  mean((observed - predicted)^2)
}

obj_function <- function(x){
  k_sed <- x
  mse_scores <- c()
  initial_water_concentrations <- c(10,50,100,200) # mg TiO2/L  Initial concentrations in Keller experiments
  for (i in 1:4) {
    sol_times <- seq(0,360)
    inits <- c("C_water" = initial_water_concentrations[i])
    solution <- data.frame(deSolve::ode(times = sol_times,  func = sed_function, y = inits,
                                       parms = c("k_sed"=k_sed),
                                       method="lsodes",
                                       rtol = 1e-3, atol = 1e-3))
    results <- data.frame(solution$time, solution$C_water/inits)  
    colnames(results) <- c("Time", "Remaining_Concentration_fraction")
    keep_predictions <- results[results$Time %in% mesocosm_data$Time,]
    mse_scores[i] <- mse_custom(mesocosm_data$`C/C0`, keep_predictions$Remaining_Concentration_fraction)
  }
  minimize <- mean(mse_scores)
  return(minimize)
}

# Inputs
x0 <- 100
initial_water_concentrations <- c(10,50,100,200) # mg TiO2/L  Initial concentrations in Keller experiments

N_iter <- 100

opts <- list( "algorithm" = "NLOPT_LN_SBPLX", #"NLOPT_LN_NEWUOA",  #"NLOPT_LN_SBPLX" ,
              "xtol_rel" = 1e-06,
              "ftol_rel" = 0.0,
              "ftol_abs" = 0.0,
              "xtol_abs" = 0.0 ,
              "maxeval" = N_iter,
              "print_level" = 1)

optimization <- nloptr::nloptr( x0 = x0,
                                eval_f = obj_function,
                                lb	= 0,
                                opts = opts)

optimization

#################

# calculate the ksed for artificial seawater 
seawater_data <-read.csv("Keller_artificial_seawater_sedimentation.csv")
colnames(seawater_data) <- c("Time", "C/C0_10", "C/C0_50", "C/C0_100", "C/C0_200")

obj_function_seawater <- function(x){
  k_sed <-  x
  mse_scores <- c()
  initial_water_concentrations <- c(10,50,100,200) # mg TiO2/L  Initial concentrations in Keller experiments
  for (i in 1:4) {
    params <- c("k_sed" = k_sed[i])
    sol_times <- seq(0,360)
    inits <- c("C_water" = initial_water_concentrations[i])
    solution <- data.frame(deSolve::ode(times = sol_times,  func = sed_function, y = inits,
                                        parms = params,
                                        method="lsodes",
                                        rtol = 1e-3, atol = 1e-3))
    results <- data.frame(solution$time, solution$C_water/inits)  
    colnames(results) <- c("Time", "Remaining_Concentration_fraction")
    keep_predictions <- results[results$Time %in% seawater_data$Time,]
    mse_scores[i] <- mse_custom(seawater_data[,i+1], keep_predictions$Remaining_Concentration_fraction)
  }
  minimize <- mean(mse_scores)
  return(minimize)
}

# Inputs
set.seed(0)
x0 <- runif(4, 0, 1e-03)
initial_water_concentrations <- c(10,50,100,200) # mg TiO2/L  Initial concentrations in Keller experiments

N_iter <- 500

opts <- list( "algorithm" = "NLOPT_LN_SBPLX", #"NLOPT_LN_NEWUOA",  #"NLOPT_LN_SBPLX" ,
              "xtol_rel" = 1e-06,
              "ftol_rel" = 0.0,
              "ftol_abs" = 0.0,
              "xtol_abs" = 0.0 ,
              "maxeval" = N_iter,
              "print_level" = 1)

optimization_2 <- nloptr::nloptr(x0 = x0,
                                eval_f = obj_function_seawater,
                                lb	= rep(0,4),
                                opts = opts)

optimization_2

# visualization

k_sed <-  optimization_2$solution
mse_scores <- c()
initial_water_concentrations <- c(10,50,100,200) # mg TiO2/L  Initial concentrations in Keller experiments
sol_times <- seq(0,360)
keep_predictions <- data.frame(matrix(data=NA, nrow = length(sol_times), ncol = dim(seawater_data)[2]))
keep_predictions[,1] <- sol_times
colnames(keep_predictions) <- c("Time", "C/C0_10", "C/C0_50", "C/C0_100", "C/C0_200")
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
cls <- c("10 mg/l"="#F8766D", "50 mg/l"="#7CAE00", "100 mg/l"="#00BFC4", "200 mg/l"="#C77CFF")

colnames(keep_predictions) <- c("Time", "C_C0_10", "C_C0_50", "C_C0_100", "C_C0_200")
colnames(seawater_data) <- c("Time", "C_C0_10", "C_C0_50", "C_C0_100", "C_C0_200")

keller_1 <- ggplot()+
  geom_line(data = keep_predictions, aes(x=Time, y=C_C0_10, color="10 mg/l"), size=1.7)+
  geom_line(data = keep_predictions, aes(x=Time, y=C_C0_50, color="50 mg/l"), size=1.7)+
  geom_line(data = keep_predictions, aes(x=Time, y=C_C0_100, color="100 mg/l"), size=1.7)+
  geom_line(data = keep_predictions, aes(x=Time, y=C_C0_200, color="200 mg/l"), size=1.7)+
  
  geom_point(data = seawater_data, aes(x=Time, y=C_C0_10, color="10 mg/l"), size=3)+
  geom_point(data = seawater_data, aes(x=Time, y=C_C0_50, color="50 mg/l"), size=3)+
  geom_point(data = seawater_data, aes(x=Time, y=C_C0_100, color="100 mg/l"), size=3)+
  geom_point(data = seawater_data, aes(x=Time, y=C_C0_200, color="200 mg/l"), size=3)+
  
  labs(title = ("Seawater sedimentation at different TiO2 concentrations"),
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


###################################
# calculate the k_sed for the rest water types 
# using the data fot TiO2 concentration of 10mg/l

extra_data <- read.csv("Keller_different_water_types_10_mg_ml_sedimentation.csv")
extra_data[1,] <- 1
obj_function_seawater <- function(x){
  k_sed <-  x
  mse_scores <- c()
  initial_water_concentrations <- 10 # mg TiO2/L  Initial concentration in Keller experiments
  for (i in 1:(dim(extra_data)[2]-1)) {
    params <- c("k_sed" = k_sed[i])
    sol_times <- seq(0,360)
    inits <- c("C_water" = initial_water_concentrations)
    solution <- data.frame(deSolve::ode(times = sol_times,  func = sed_function, y = inits,
                                        parms = params,
                                        method="lsodes",
                                        rtol = 1e-3, atol = 1e-3))
    results <- data.frame(solution$time, solution$C_water/inits)  
    colnames(results) <- c("Time", "Remaining_Concentration_fraction")
    keep_predictions <- results[results$Time %in% extra_data$Time,]
    mse_scores[i] <- mse_custom(extra_data[,i+1], keep_predictions$Remaining_Concentration_fraction)
  }
  minimize <- mean(mse_scores)
  return(minimize)
}

# Inputs
set.seed(0)
x0 <- runif(7, 0, 1e-03)

N_iter <- 500

opts <- list( "algorithm" = "NLOPT_LN_SBPLX", #"NLOPT_LN_NEWUOA",  #"NLOPT_LN_SBPLX" ,
              "xtol_rel" = 1e-06,
              "ftol_rel" = 0.0,
              "ftol_abs" = 0.0,
              "xtol_abs" = 0.0 ,
              "maxeval" = N_iter,
              "print_level" = 1)

optimization_3 <- nloptr::nloptr( x0 = x0,
                                  eval_f = obj_function_seawater,
                                  lb	= rep(0,7),
                                  opts = opts)

optimization_3

# visualization

k_sed <-  optimization_3$solution
initial_water_concentrations <- 10 # mg TiO2/L  Initial concentrations in Keller experiments
sol_times <- seq(0,360)
keep_predictions <- data.frame(matrix(data=NA, nrow = length(sol_times), ncol = dim(extra_data)[2]))
keep_predictions[,1] <- sol_times
colnames(keep_predictions) <- colnames(extra_data) #c("Time", "C/C0_10", "C/C0_50", "C/C0_100", "C/C0_200")
for (i in 1:(dim(extra_data)[2]-1)) {
  params <- c("k_sed" = k_sed[i])
  inits <- c("C_water" = initial_water_concentrations)
  solution <- data.frame(deSolve::ode(times = sol_times,  func = sed_function, y = inits,
                                      parms = params,
                                      method="lsodes",
                                      rtol = 1e-3, atol = 1e-3))
  results <- data.frame(solution$time, solution$C_water/inits)  
  colnames(results) <- c("Time", "Remaining_Concentration_fraction")
  keep_predictions[,i+1] <- results[,2]
}

library(ggplot2)
# print(hue_pal()(7)) # to return 7 color codes 
cls <- c("Stormwater"="#F8766D", "Mesocosm_effluent"="#C49A00", "Treated_effluent"="#53B400", "Lagoon"="#00C094", 
         "Groundwater"="#00B6EB", "Santa_Clara_river"="#A58AFF",  "Seawater"="#FB61D7")
keller_2 <- ggplot()+
  geom_line(data = keep_predictions, aes(x=Time, y=Stormwater, color="Stormwater"), size=1.7)+
  geom_line(data = keep_predictions, aes(x=Time, y=Mesocosm_effluent, color="Mesocosm_effluent"), size=1.7)+
  geom_line(data = keep_predictions, aes(x=Time, y=Treated_effluent, color="Treated_effluent"), size=1.7)+
  geom_line(data = keep_predictions, aes(x=Time, y=Lagoon, color="Lagoon"), size=1.7)+
  geom_line(data = keep_predictions, aes(x=Time, y=Groundwater, color="Groundwater"), size=1.7)+
  geom_line(data = keep_predictions, aes(x=Time, y=Santa_Clara_river, color="Santa_Clara_river"), size=1.7)+
  geom_line(data = keep_predictions, aes(x=Time, y=Seawater, color="Seawater"), size=1.7)+
  
  
  geom_point(data = extra_data, aes(x=Time, y=Stormwater, color="Stormwater"), size=3)+
  geom_point(data = extra_data, aes(x=Time, y=Mesocosm_effluent, color="Mesocosm_effluent"), size=3)+
  geom_point(data = extra_data, aes(x=Time, y=Treated_effluent, color="Treated_effluent"), size=3)+
  geom_point(data = extra_data, aes(x=Time, y=Lagoon, color="Lagoon"), size=3)+
  geom_point(data = extra_data, aes(x=Time, y=Groundwater, color="Groundwater"), size=3)+
  geom_point(data = extra_data, aes(x=Time, y=Santa_Clara_river, color="Santa_Clara_river"), size=3)+
  geom_point(data = extra_data, aes(x=Time, y=Seawater, color="Seawater"), size=3)+
  
  labs(title = ("Seawater sedimentation at different water types with 10 mg/ml TiO2"),
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
