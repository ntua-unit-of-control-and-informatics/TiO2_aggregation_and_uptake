setwd("C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/TiO2_sedimentation/Chen_2019_data")

# Coding of the TiO2 NMs types based on the coding used
# in Chen 2019 supplemental material

A <- c("A", "TiO2-5A", 1, 5, 7.5, 10, 15)
B <- c("B", "TiO2-10A", 1, 5, 10, 15, 20)
C <- c("C", "TiO2-100A", 10, 20, 30, 40, 50)
D <- c("D", "TiO2-P25", 5, 10, 20, 30, 40)
E <- c("E", "TiO2-25R", 20, 40, 60, 80, 100)

metadata <- data.frame(rbind(A,B, C, D, E))
colnames(metadata) <- c("Code", "Type", "C1", "C2", "C3", "C4", "C5")  

i <- 3:dim(metadata)[2]
metadata[,i] <- apply(metadata[,i], 2, 
                  function(x) as.numeric(as.character(x)))

#check that the columns have been transformed to numeric type
#sapply(metadata, class)
#print(metadata)

#=======================
#   *** FUNCTIONS ***   
#=======================


# *** sed_function ***

# A simple ODE to describe the sedimentation effect on the TiO2 concentration
sed_function <- function(time, inits, params){
  with(as.list(c(inits, params)),{
    
    dC_water <- - k_sed*C_water
    
    list(dC_water)
  })
}

# *** mse_custom ***

# The metric used for the optimization
mse_custom <- function(observed, predicted){
  mean((observed - predicted)^2)
}


# *** obj_function ***

# The objective function to minimize
obj_function <- function(x, exp_data, initial_water_concentrations){
  k_sed <- x
  mse_scores <- c()
  for (i in 1:length(k_sed)) {
    sol_times <- seq(0, 30, 0.1)
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


# *** optimization_func ***

# A function that handles the optimization 
optimization_func <- function(exp_data, initial_water_concentrations){
  # Inputs
  #initial_water_concentrations <- c(10, 100) # given in mg TiO2/L
  x0 <- runif(length(initial_water_concentrations), 0, 1e-03)
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
                                 lb	= rep(0,length(x0)),
                                 opts = opts, 
                                 exp_data = exp_data,
                                 initial_water_concentrations = initial_water_concentrations)
  
  return(optimization)
}


# *** plot_func ***

# A function to plot the model predictions with the experimental data of a
# specific type water
plot_func <- function(optimization, exp_data, initial_water_concentrations, plot_title){
  sol_times <- seq(0, 30, 0.1)
  k_sed <-  optimization$solution
  mse_scores <- c()
  keep_predictions <- data.frame(matrix(data=NA, nrow = length(sol_times), ncol = dim(exp_data)[2]))
  keep_predictions[,1] <- sol_times
  colnames(keep_predictions) <- c("Time", "C1", "C2", "C3", "C4", "C5")
  for (i in 1:length(initial_water_concentrations)) {
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
  # prepare the labels for the plot legends
  strings <- as.character(initial_water_concentrations)
  color_codes <- scales::hue_pal()(5) # to return 5 color codes 
  cls <- c()  
  
  for (i in 1:length(strings)) {
    strings[i] <- paste0(strings[i], " mg/ml")
    cls[i] <- color_codes[i]
    names(cls)[i] <- strings[i]
  }

  colnames(keep_predictions) <- colnames(exp_data)
  colnames(exp_data) <- colnames(exp_data)
  
  plot <- ggplot()+
    geom_line(data = keep_predictions, aes(x=Time, y=C1, color=strings[1]), size=1.7)+
    geom_line(data = keep_predictions, aes(x=Time, y=C2, color=strings[2]), size=1.7)+
    geom_line(data = keep_predictions, aes(x=Time, y=C3, color=strings[3]), size=1.7)+
    geom_line(data = keep_predictions, aes(x=Time, y=C4, color=strings[4]), size=1.7)+
    geom_line(data = keep_predictions, aes(x=Time, y=C5, color=strings[5]), size=1.7)+
    
    geom_point(data = exp_data, aes(x=Time, y=C1, color=strings[1]), size=3)+
    geom_point(data = exp_data, aes(x=Time, y=C2, color=strings[2]), size=3)+
    geom_point(data = exp_data, aes(x=Time, y=C3, color=strings[3]), size=3)+
    geom_point(data = exp_data, aes(x=Time, y=C4, color=strings[4]), size=3)+
    geom_point(data = exp_data, aes(x=Time, y=C5, color=strings[5]), size=3)+
    
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

# *** data_loader ***
# A function to load the csv files of various TiO2 types
# These csv files contain the absorbance data of a TiO2 type for 5 different 
# concentrations (the set of concentrations is different for each TiO2 type).
# After the data processing, this function returns a dataframe 
# with the C/C0 at different concentrations.
data_loader <- function(filename){
  df <- read.csv(filename, header=TRUE) # Contains the absolute abdorbance values
  colnames(df) <- c('Time', 'C1', 'C2', 'C3', 'C4', 'C5')
  df_tr <- df # copy the df 
  A0 <- df_tr[1, 2:dim(df_tr)[2]] # The initial absorbance
  
  for (i in 2:dim(df_tr)[1]) {
    df_tr[i, 2:dim(df_tr)[2]] <-  df_tr[i, 2:dim(df_tr)[2]]/ A0
  }
  # Replace the first row with 1s (C/C0 = 1 at the beginning)
  df_tr[1, 2:dim(df_tr)[2]] <- 1
  
  return(df_tr)
}

files <-c("5A-TiO2.csv", #A
          "10A-TiO2.csv", #B
          "100A-TiO2.csv", #C
          "P25-TiO2.csv", #D
          "25R-TiO2.csv") #E

# Prepare the dataframes
df_A <- data_loader(files[1])
df_B <- data_loader(files[2])
df_C <- data_loader(files[3])
df_D <- data_loader(files[4])
df_E <- data_loader(files[5])


take_results <- function(code, exp_data){
  initial_water_concentrations <- as.numeric(metadata[metadata$Code == code, c("C1","C2","C3","C4","C5")])
  optimization <- optimization_func(exp_data, initial_water_concentrations)
  plot <- plot_func(optimization, exp_data, initial_water_concentrations, 
                      paste(as.character(metadata[which(metadata$Code==code), "Type"])," sedimentation"))
  ksed <-  optimization$solution
  names(ksed) <- as.character(initial_water_concentrations)
  
  return(list(
    "code" = code,
    "Type" = as.character(metadata[which(metadata$Code==code), "Type"]),
    "exp_data" = exp_data,
    "initial_water_concentrations"=initial_water_concentrations,
    "optimization" = optimization,
    "plot" = plot,
    "ksed" = ksed 
  ))
}

# Calculations for TiO2 A
results_A <- take_results(code="A", exp_data = df_A)

# Calculations for TiO2 B
results_B <- take_results(code="B", exp_data = df_B)

# Calculations for TiO2 C
results_C <- take_results(code="C", exp_data = df_C)

# Calculations for TiO2 D
results_D <- take_results(code="D", exp_data = df_D)

# Calculations for TiO2 E
results_E <- take_results(code="E", exp_data = df_E)
