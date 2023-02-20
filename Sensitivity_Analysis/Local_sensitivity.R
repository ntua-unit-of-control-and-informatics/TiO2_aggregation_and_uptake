setwd('C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/Sensitivity_Analysis')

# In this sensitivity analysis test we will take the pvalues of parameters from 
# the fit of the model on Fan et al. (2016) data

# Betini et al. (2019)
#input: age [days], temperature [oC], food["low"/"high"]
Size_estimation <- function(age, temperature = 22, food="high"){
  
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

Filtration_rate_estimation <- function(Length, Temperature = 22, method = "Preuss"){
  
  if(method == "Burns"){
    # Filtering rate of Daphnia magna is calculated based on Burns et al. 1969.
    # Input: Length [mm],Ttemperature [oC]/ Output: filtration rate[mL/h]
    F_rate_15 <- 0.153 * Length^2.16
    F_rate_20 <- 0.208 * Length^2.80
    F_rate_25 <- 0.202 * Length^2.38
    
    if(Temperature <= 15){
      F_rate <- F_rate_15
    }else if(Temperature >= 25){  
      F_rate <- F_rate_25
    }else{ 
      F_rate <- approx(c(15,20,25), c(F_rate_15, F_rate_20, F_rate_25), Temperature)$y
    }
  }else if(method == "Preuss"){
    F_rate <- 0.5*Length^2.45
  }else{
    stop("Please select a valid estimation method; either 'Burns' or 'Preuss' ")
  }
  return(F_rate)
}

# Dumont et al. (1975)
# Input: length [mm]/ Output: dry weight[mg]
dry_weight_estimation <- function(L){
  
  w1 = (1.89e-06*(L*1000)^2.25)/1000 #Donka Lake
  w2 = (4.88e-05*(L*1000)^1.80)/1000 #River Sambre
  # Selected w1 after validation with Martin-Creuzburg et al. (2018
  return(w1)
}

# V_water is the volume of water (in L) in the corresponding experiment.
# The volume remains constant during the experiment and equal to 100 ml
V_water <- 0.1 # L

# Population of daphnias during the experiments
# At each measurement 10 daphnias were removed from the system
N <- 10

# Load the predicted ksed values
ksed_predicted <- read.csv("C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/Exposure/Fan_2016/data/Fan_2016_ksed_predictions.csv")
ksed_predicted <- ksed_predicted[, c(1,4,6)]
colnames(ksed_predicted) <- c("Name", "Concentration_mg/L", "k_sed")
# We will keep only the three ksed values for T1 TiO2 for the three different concentrations
ksed_predicted <- ksed_predicted[1:3,]

# ode_func(): the differential equation system that describes the model

ode_func <- function(time, inits, params){
  with(as.list(c(inits, params)),{
    
    # Units explanation:
    # C_water: mg TiO2 / L water (same as data)
    # C_daphnia: mg TiO2 / mg daphnia 
    # k_sed: 1/h
    # F_rate: L water/h/individual daphnia 
    # ke_2: 1/h
    
    # Number of Daphnids per beaker
    N_current <- 10
    # Fan et al. (2016) let the daphnids in SM7 medium for 3 hours
    # prior to the experiment to empty their guts
    time_d <- 3/24+time/24 #time in days
    # Weight loss under starvation through exponential decay (Elen et al., 1989)
    reduction <-ifelse(time_d<2, (1- 0.32*(1-exp(-36.6*time_d))), 
                       (0.682 - 0.435 * (1- 16.79*exp(-1.41*time_d))))
    dry_weight_current <- reduction * dry_weight
    
    #k_sed <- 0
    # C_water: TiO2 concentration in water
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

#---------------------
# Custom AUC function
#---------------------
AUC <- function(x, y){
  individual_auc <- c()
  for (i in 1:(length(x)-1)){
    individual_auc[i] <- (y[i]+y[i+1])*(x[i+1]-x[i])/2
  }
  return(sum(individual_auc))
}

#==============================#
# *Local Sensitivity Function* #
#==============================#

Sensitivity_func <- function(model, params, targets, ranges, sol_times, eventdat, C_water_0, heatmap = FALSE){

  # The variation dp of each parameter will be equal to "ranges" value 
  if(!is.numeric(ranges) | length(ranges) != 1 | abs(ranges)>1){
    # "ranges" must be a single numeric value in (0,1]
    stop("For local sensitivity analysis \"ranges\"  should be 
         a single numeric value in (0,1]")
  }else{dp <- ranges}
  
  
  # Assign the values to the corresponding parameters
  for (i in 1:length(params)) {
    assign(names(params)[i], params[[i]])
  }
  # Calculate the physiological parameters
  L = Size_estimation(age) #mm
  dry_weight =  dry_weight_estimation(L) #mg
  F_rate <- Filtration_rate_estimation(L, method = 'Preuss')#mL/h
  
  # time points for ODEs solution
  sol_times <- sol_times
  
  # Assign the initial values of parameters to params_0 vector
  params_0 <- c('dry_weight'=dry_weight, 'F_rate'=F_rate, 
                'a'=a, 'ke_2'=ke_2, 'C_sat'=C_sat,
                'n'=n)
  
  # Later put a loop here for the concentrations
  
  # initial conditions
  inits <- c('C_water'=C_water_0, 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
             'M_sed'=0)
  k_sed <- ksed_predicted[which(ksed_predicted$`Concentration_mg/L`==inits[1]),"k_sed"]
  # add the k_sed to params_0
  params_0 <- c(params_0, 'k_sed'=k_sed)
  
  # Get the number of the parameters and targets
  N_params <- length(params_0) 
  N_targets <- length(targets)
  
  # Calculate AUC of each target-compartment for the initial parameters
  AUC_0 <- c()
  # Keep the maximum concentration of each output variable
  Cmax_0 <- c()
  # Initialize a vector to store the sensitivity indexes
  SI_auc <- matrix(NA, nrow = N_params, ncol = N_targets)
  SI_cmax <- matrix(NA, nrow = N_params, ncol = N_targets)
  
  for (i in 1:(N_params+1)) {
    if(i==1){ # Solve the ODEs for the initial parameters values
      ode_params <- c(params_0, 'V_water'=V_water)
      solution_0 <- data.frame(deSolve::ode(times = sol_times,  func = model, y = inits,
                                          parms = ode_params,
                                          events = list(data = eventdat),
                                          method="lsodes",
                                          rtol = 1e-3, atol = 1e-3))  
      

      for (j in 1:N_targets) {
        AUC_0[j] <- AUC(solution_0[,"time"],solution_0[,targets[j]])
        Cmax_0[j] <- max(solution_0[,targets[j]])
      }
      
    }else{
      params <- params_0
      params[i-1] <- params[i-1]*(1 + dp)
      # if(i==2){ # if L is changed re-calculate dry_weifht and F_rate
      #   params[2] <- dry_weight_estimation(params[[1]]) #mg
      #   params[3] <- Filtration_rate_estimation(params[[1]], method = 'Preuss')#mL/h
      # }
      ode_params <- c(params, 'V_water'=V_water)
      solution <- data.frame(deSolve::ode(times = sol_times,  func = model, y = inits,
                                          parms = ode_params,
                                          events = list(data = eventdat),
                                          method="lsodes",
                                          rtol = 1e-3, atol = 1e-3))  
      
      for (j in 1:N_targets) {
        # Calculate AUC for the target  j
        AUC_j <- AUC(solution[,"time"],solution[,targets[j]])
        # Keep the max value for the target  j
        Cmax_j <- max(solution[,targets[j]])
        # Calculate sensitivity index of parameter i 
        # Relative Sensitivity of AUC = (dAUC/AUC)/(dp/p)
        SI_auc[i-1,j] <- ((AUC_j-AUC_0[j])/AUC_0[j])/((params[i-1]- params_0[i-1])/ params_0[i-1])
        # Relative Sensitivity of Cmax = (dCmax/Cmax)/(dp/p)
        SI_cmax[i-1,j] <- ((Cmax_j-Cmax_0[j])/Cmax_0[j])/((params[i-1]- params_0[i-1])/ params_0[i-1])
      }
    }
  }
  
  rownames(SI_auc) <- names(params)
  colnames(SI_auc) <-  paste0("AUC_", targets)
  
  rownames(SI_cmax) <- names(params)
  colnames(SI_cmax) <- paste0("Max ", targets)
  
  
  #----------
  # Heatmaps
  #----------
  if(heatmap){
    
    if(length(targets)==1){
      stop("Provide more than 1 targets in order to create a heatmap
           or turn \"heatmap\" to \"FALSE\".")
    }
    
    heatmap1 <- pheatmap::pheatmap(as.matrix(abs(t(SI_auc))),
                                   cluster_rows=F,
                                   cluster_cols=F,
                                   cellwidth = 80, cellheight = 80,
                                   border_color = NA,
                                   fontsize = 20,
                                   fontsize_row = 15, 
                                   fontsize_col = 15,
                                   col = colorRampPalette(RColorBrewer::brewer.pal(8, "YlOrRd"))(25), 
                                   main=paste0("Normalized Sensitivity Indexes - C = ", C_water_0," TiO2 mg/L"))
    
    AUC_heatmap <- recordPlot(heatmap1)
    
    # heatmap2 <- pheatmap::pheatmap(as.matrix(abs(t(SI_cmax))),
    #                                cellwidth = 60, cellheight = 60,
    #                                border_color = NA,
    #                                fontsize = 20,
    #                                fontsize_row = 15, 
    #                                fontsize_col = 15,
    #                                col = colorRampPalette(RColorBrewer::brewer.pal(8, "YlOrRd"))(25),
    #                                main="Cmax Senesitivity Indexes")
    # Cmax_heatmap <- recordPlot(heatmap2)
    
    plot_list <- list("AUC_heatmap"=AUC_heatmap)#, "Cmax_heatmap"=Cmax_heatmap)
  }
  data_list <- list("Targets"=targets,
                    "Change of parameters" = dp,
                    "Parameters" = params_0, 
                    "Normalized AUC Sensitivity Coefficients"=data.frame(SI_auc), 
                    #"Normalized C_max Sensitivity Coefficients"=data.frame(SI_cmax),
                    "Plot_list"=plot_list)

  return(data_list)
}

#===============================================================================
# Water concnentrations
C_water_0 <- c(0.1, 1, 10) # mg/L

# The fitted parameters for T1-TiO2
# method = 'Preuss'
# sedimentation = T
#sol_times <- unique(c(seq(0,2, 0.01), seq(2,28,0.1)))
sol_times <- seq(0,48, 0.1)

# create events
create_events <- function(C_warer, time_points){
  eventdat <- data.frame(var = rep("C_water", length(refresh_moments)),
                         time = refresh_moments,
                         value = rep(C_warer, length(refresh_moments)),
                         method = rep('rep', length(refresh_moments))
  )
  return(eventdat)
}
refresh_moments <- seq(3,24,3)
eventdat_01 <- create_events(0.1, refresh_moments)
eventdat_1 <- create_events(1, refresh_moments)
eventdat_10 <- create_events(10, refresh_moments)

fitted_params <- c('age'=14,'a'=0.331, 'ke_2'=0.021, 'C_sat'=0.136, 'n'=2)
targets <- c('C_daphnia', 'M_Daphnia_excreted', 'M_sed')
ranges <- 0.1

SA_0.1 <- Sensitivity_func(model=ode_func, params=fitted_params, targets=targets,
                           ranges=ranges, sol_times= sol_times, eventdat=eventdat_01,
                           C_water_0=C_water_0[1], heatmap = T)
SA_1.0 <- Sensitivity_func(model=ode_func, params=fitted_params, targets=targets,
                           ranges=ranges,  sol_times= sol_times, eventdat=eventdat_1,
                           C_water_0=C_water_0[2], heatmap = T)
SA_10 <- Sensitivity_func(model=ode_func, params=fitted_params, targets=targets,
                          ranges=ranges,  sol_times= sol_times, eventdat=eventdat_10,
                          C_water_0=C_water_0[3], heatmap = T)
