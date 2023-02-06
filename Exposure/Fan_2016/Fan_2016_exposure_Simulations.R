simulation_func <- function(input_string){
  splitted_input <- strsplit(input_string, split = ' ')
  sedimentation <-  unlist(splitted_input)[1]
  score <-  unlist(splitted_input)[2]
  method <-  unlist(splitted_input)[3]
  N_iter <-  20#unlist(splitted_input)[4]
  
  # This is a script to simulate the experiments in Fan et al., 2016
  # The first experiment is about the waterborne exposure of D. Magna to TiO2 nanoparticles
  # The second is about the trophic exposure to TiO2 exposured algae.
  
  # Working directory
  dir = 'C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/Exposure/Fan_2016'
  setwd(dir)
  dir_uptake <- 'C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/'
  dir_filtration <- 'C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/'
  
  # dir_uptake <- 'C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/'
  # dir_filtration <- 'C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/'
  # dir <- 'C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/Exposure/Fan_2016'
  
  #=================#
  #  Water exposure #
  #=================#
  
  # The following dataframe is a mappin of the of the TiO2 NPs types with 
  # their corresponding codes (which are simple letters)
  A <- c("A", "TiO2-T1")
  B <- c("B", "TiO2-T2")
  C <- c("C", "TiO2-H1")
  D <- c("D", "TiO2-S1")
  E <- c("E", "TiO2-H2")
  G <- c("G", "TiO2-S2")
  
  Mapping <- data.frame(rbind(A, B, C, D, E, G))
  colnames(Mapping) <- c("Code", "Type")
  i <- 1:dim(Mapping)[2]
  Mapping[,i] <- apply(Mapping[,i], 2,
                       function(x) as.character(x))
  sapply(Mapping, class)
  
  # Load data  for water exposure - uptake phase
  
  # The values are reported as mg TiO2/g of dry daphnia
  # Time is given in minutes (the uptake experiment lasted 60 minutes)
  # Load the data for concentration = 0.1 mg/l
  C1_uptake_data <- read.csv('data/exposure_data/uptake/0.1_uptake.csv')
  colnames(C1_uptake_data)[-1] <- Mapping$Type
  C1_uptake_data[,2:7] <-  C1_uptake_data[,2:7]/1e03 # transform from mg/g daphnia to mg/mg daphnia
  C1_uptake_data[,1] <- C1_uptake_data[,1]/60 # Transform time to hours
  
  # Load the data for concentration = 1.0 mg/l
  C2_uptake_data <- read.csv('data/exposure_data/uptake/1_uptake.csv')
  colnames(C2_uptake_data)[-1] <- Mapping$Type
  C2_uptake_data[,2:7] <-  C2_uptake_data[,2:7]/1e03 # transform from mg/kg daphnia to mg/mg daphnia
  C2_uptake_data[,1] <- C2_uptake_data[,1]/60 # Transform time to hours
  
  # Load the data for concentration = 10.0 mg/l
  C3_uptake_data <- read.csv('data/exposure_data/uptake/10_uptake.csv')
  colnames(C3_uptake_data)[-1] <- Mapping$Type
  C3_uptake_data[,2:7] <-  C3_uptake_data[,2:7]/1e03 # transform from mg/kg daphnia to mg/mg daphnia
  C3_uptake_data[,1] <- C3_uptake_data[,1]/60 # Transform time to hours
  
  # Load data  for water exposure - depuration phase
  
  # The values are reported as % retained after 2 hours uptake phase
  # Time is given in hours (the depuration experiment lasted 24 hours after 2 hours uptake])
  # Load the data for concentration = 0.1 mg/l
  C1_depuration_data <- read.csv('data/exposure_data/depuration/0.1_depuration.csv')
  colnames(C1_depuration_data)[-1] <- Mapping$Type
  
  # Load the data for concentration = 1.0 mg/l
  C2_depuration_data <- read.csv('data/exposure_data/depuration/1_depuration.csv')
  colnames(C2_depuration_data)[-1] <- Mapping$Type
  
  # Load the data for concentration = 10.0 mg/l
  C3_depuration_data <- read.csv('data/exposure_data/depuration/10_depuration.csv')
  colnames(C3_depuration_data)[-1] <- Mapping$Type
  
  # Load the concentrations at the ending of depuration phase (units = mg TiO2/g daphnia)
  final_concentrations <- read.csv('data/exposure_data/depuration/final_concentrations.csv')[2:7]
  rownames(final_concentrations) <- c("C1", "C2", "C3")
  colnames(final_concentrations) <-  Mapping$Type
  final_concentrations <- final_concentrations/1e03 # transform 1 g of daphnia to 1 mg 
  
  # next i calculate the concentration of daphnia at each experiment at time_point=2 hours (C_2h)
  # from C_2h = C_final/x, where x is the % percentage of final concentration to the C_2h
  
  # Take the percentages corresponding at the end of the depuration phase
  last_point <- dim(C1_depuration_data)[1]
  percentages <- rbind(C1_depuration_data[last_point,2:7], C2_depuration_data[last_point,2:7], C3_depuration_data[last_point,2:7])/100
  rownames(percentages) <- c("C1", "C2", "C3")
  
  # The concentrations of daphnia at time=2h, (end of exposure)
  C_2h <- final_concentrations/percentages # mg TiO2/ mg daphnia
  
  # Now we are able to calculate the actual concentration of daphnia at each time point of depuration by multiplying
  # the C1_depuration_data, C2_depuration_data and C3_depuration_data with C_2h
  
  for (i in 2:7) {
    C1_depuration_data[,i] <- C_2h[1,i-1]*C1_depuration_data[,i]/100
    C2_depuration_data[,i] <- C_2h[2,i-1]*C2_depuration_data[,i]/100
    C3_depuration_data[,i] <- C_2h[3,i-1]*C3_depuration_data[,i]/100
  }
  C1_depuration_data[,1] <- C1_depuration_data[,1] +2
  C2_depuration_data[,1] <- C2_depuration_data[,1] +2
  C3_depuration_data[,1] <- C3_depuration_data[,1] +2
  
  # Concatenate the uptake and the depuration data
  C1_data <- rbind(C1_uptake_data, C1_depuration_data)
  C2_data <- rbind(C2_uptake_data, C2_depuration_data)
  C3_data <- rbind(C3_uptake_data, C3_depuration_data)
  
  
  # Betini et al. (2019)
  #input: age [days], temperature [oC], food["low"/"high"]
  Size_estimation <<- function(age, temperature = 22, food="high"){
    
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
  
  Filtration_rate_estimation <<- function(Length, Temperature = 22, method = "Preuss"){
    
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
  dry_weight_estimation <<- function(L){
    
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
  setwd(dir)
  ksed_predicted <- read.csv("data/Fan_2016_ksed_predictions.csv")
  ksed_predicted <- ksed_predicted[, c(1,4,6)]
  colnames(ksed_predicted) <- c("Name", "Concentration_mg/L", "k_sed")
  
  ##########################################################
  
  # *** metrics ***
  
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
  
  PBKOF <- function(observed, predicted, comp.names =NULL){
    # Check if the user provided the correct input format
    if (!is.list(observed) || !is.list(predicted)){
      stop(" The observations and predictions must be lists")
    }
    # Check if the user provided equal length lists
    if (length(observed) != length(predicted)){
      stop(" The observations and predictions must have the same compartments")
    }
    Ncomp <- length(observed) # Number of compartments
    I <- rep(NA, Ncomp) # Compartment discrepancy index
    N_obs <- rep(NA, Ncomp) #Number of observations per compartment
    #loop over the compartments
    for (i in 1:Ncomp){
      Et <- 0 #relative error with observations
      St <- 0  #relative error with simulations
      N <- length(observed[[i]]) # number of observations for compartment i
      # Check if observations and predictions have equal length
      if(N != length(predicted[[i]])){
        stop(paste0("Compartment ",i," had different length in the observations and predictions"))
      }
      N_obs[i] <- N # populate the N_obs vector
      for (j in 1:N){
        # sum of relative squared errors (error = observed - predicted)
        Et <- Et + ( abs(observed[[i]][j] - predicted[[i]][j])  / observed[[i]][j] )  ^2
        St <- St + ( abs(observed[[i]][j] - predicted[[i]][j])  / predicted[[i]][j] )  ^2
      }
      
      # root mean of the square of observed values
      RMEt <- sqrt(Et/N)
      # root mean of the square of simulated values
      RMSt <- sqrt( St/N)
      
      I[i] <- (RMEt + RMSt)/2   
    }
    # Total number of observations
    Ntot <- sum(N_obs)
    # Initialise the consolidated discrepancy index
    Ic <-0
    for (i in 1:Ncomp){
      # Give weight to compartments with more observations (more information)
      Ic <- Ic +  I[i]* N_obs[i]/Ntot
    }
    # Name the list of compartment discrepancy indices
    if ( !is.null(comp.names)){
      names(I) <- comp.names
    }else if (!is.null(names(observed))){
      names(I) <- names(observed)
    } else if (!is.null(names(predicted)) && is.null(comp.names) ){
      names(I) <- names(predicted)
    }
    return(Ic)
    #return(list(Total_index = Ic, Compartment_index= I))
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
      N_current <- 10
      # Fan et al. (2016) let the daphnids in SM7 medium for 3 hours
      # prior to the experiment to empty their guts
      time_d <- 3+time/24 #time in days
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
  
  
  # obj_func function to minimize
  obj_func <- function(x, C_water_0, nm_types, V_water, ksed_predicted, sedimentation, score){
    
    score_per_type <- c()
    
    age <- x[19]#days
    L = Size_estimation(age) #mm
    dry_weight =  dry_weight_estimation(L) #mg
    F_rate <- Filtration_rate_estimation(L, method = method)#mL/h
    n <- x[20]
    
    for (j in 1:length(nm_types)) {
      nm_type <- nm_types[j]
      
      exp_data <- cbind(C1_data["Time"], C1_data[nm_type], C2_data[nm_type], C3_data[nm_type])
      colnames(exp_data) <- c('Time', 'C1', 'C2', 'C3')
      exp_data[,1] <- c(0.16, 0.33, 0.67, 1.0, 2.0, 5.0, 8.0, 14.0, 26.0) # hours
      
      sub_a <- x[1:6]
      a <- sub_a[j] 
      sub_k <- x[7:12]
      ke_2 <- sub_k[j] 
      sub_C_sat <- x[13:18]
      C_sat <- sub_C_sat[j]    
      
      sol_times <- unique(c(seq(0,2, 0.01), seq(2,28,0.1)))
      
      score_per_conc <- c()
      ksed_nm_type <- substr(nm_type, nchar(nm_type) - 2 + 1, nchar(nm_type)) # Extract last 2 characters
      
      for (i in 1:3) { #loop for the 3 different concentrations
        
        # Check the sedimentation condition
        if(sedimentation){
          k_sed <- ksed_predicted[which(ksed_predicted$Name==ksed_nm_type & ksed_predicted$`Concentration_mg/L`==C_water_0[i]),"k_sed"]
        }else{
          k_sed=0
        }
        
        
        constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                             'k_sed'= k_sed)
        fitted_params <- c("a"=a, "ke_2"=ke_2, "C_sat"=C_sat, "n"=n)
        params <- c(fitted_params, constant_params)
        
        inits <- c('C_water'=C_water_0[i], 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
                   'M_sed'=0)      
        # create events to force C_water=0 at time = 2 hours
        eventdat <- data.frame(var = c("C_water"),
                               time = 2,
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
        
        if(score == "rmse"){
          score_per_conc[i] <- rmse(exp_data[,i+1], results)
        }else if(score == "AAFE"){
          score_per_conc[i] <- AAFE(exp_data[,i+1], results)  
        }else if (score == "PBKOF"){
          score_per_conc[i] <- PBKOF(list(exp_data[,i+1]), list(results))
        }
        
      }
      
      score_per_type[j] <- mean(score_per_conc)
      
    }
    
    return(mean(score_per_type))
  }
  
  plot_func <- function(optimization, C_water_0, nm_types, V_water, ksed_predicted, sedimentation, score){
    
    library(ggplot2)
    x <- optimization$solution
    
    age <- x[19]#days
    L = Size_estimation(age) #mm
    dry_weight =  dry_weight_estimation(L) #mg
    F_rate <- Filtration_rate_estimation(L, method = method)#mL/h
    n <- x[20]
    
    plots_list <- list()
    for (j in 1:length(nm_types)) {
      nm_type <- nm_types[j]
      
      exp_data <- cbind(C1_data["Time"], C1_data[nm_type], C2_data[nm_type], C3_data[nm_type])
      colnames(exp_data) <- c('Time', 'C1', 'C2', 'C3')
      exp_data[,1] <- c(0.16, 0.33, 0.67, 1.0, 2.0, 5.0, 8.0, 14.0, 26.0) # hours
      
      sub_a <- x[1:6]
      a <- sub_a[j] 
      sub_k <- x[7:12]
      ke_2 <- sub_k[j] 
      sub_C_sat <- x[13:18]
      C_sat <- sub_C_sat[j]
      
      sol_times <- unique(c(seq(0,2, 0.01), seq(2,28,0.1)))
      
      score_per_conc <- c()
      ksed_nm_type <- substr(nm_type, nchar(nm_type) - 2 + 1, nchar(nm_type)) # Extract last 2 characters
      
      keep_predictions <- data.frame(matrix(NA, nrow = length(sol_times), ncol = 4))
      keep_predictions[,1] <- sol_times
      colnames(keep_predictions) <- c('Time', 'C1', 'C2', 'C3')
      
      for (i in 1:3) { #loop for the 3 different concentrations
        
        # Check the sedimentation condition
        if(sedimentation){
          k_sed <- ksed_predicted[which(ksed_predicted$Name==ksed_nm_type & ksed_predicted$`Concentration_mg/L`==C_water_0[i]),"k_sed"]
        }else{
          k_sed=0
        }
        
        
        constant_params <- c("F_rate" = F_rate, "V_water" = V_water, "dry_weight" = dry_weight,
                             'k_sed'= k_sed)
        fitted_params <- c("a"=a, "ke_2"=ke_2, "C_sat"=C_sat, "n"=n)
        params <- c(fitted_params, constant_params)
        
        inits <- c('C_water'=C_water_0[i], 'C_daphnia'=0, 'M_Daphnia_excreted'=0,
                   'M_sed'=0)            
        # create events to force C_water=0 at time = 2 hours
        eventdat <- data.frame(var = c("C_water"),
                               time = 2,
                               value = 0,
                               method = 'rep'
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
  
  
  
  
  ############################################################################
  
  nm_types <- as.character(Mapping[,2])

  x0 <- c(rep(0.5,6), rep(0.001, 6), rep(0.25,6), 14, 2.5)
  lb <- c(rep(0,12), as.numeric(1.1*C3_data[C3_data$Time==2,2:7]), 10, 2)
  ub <- c(rep(4,6), rep(1,6), rep(0.4,6),18, 4)

  C_water_0 <- c(0.1, 1, 10) # mg/L
  
  opts <- list( "algorithm" = "NLOPT_LN_SBPLX" , #"NLOPT_LN_NEWUOA"
                "xtol_rel" = 1e-07,
                "ftol_rel" = 1e-07,
                "ftol_abs" = 0.0,
                "xtol_abs" = 0.0 ,
                "maxeval" = N_iter,
                "print_level" = 1)
  
  optimization <- nloptr::nloptr(x0 = x0,
                                 eval_f = obj_func,
                                 lb	= lb,
                                 ub = ub,
                                 opts = opts,
                                 C_water_0 = C_water_0,
                                 nm_types = nm_types,
                                 V_water = V_water,
                                 ksed_predicted = ksed_predicted,
                                 sedimentation = sedimentation,
                                 score = score)
  
  fitted_params <- optimization$solution
  
  params_values <- data.frame(matrix(fitted_params[1:18], nrow = 3, byrow = T)) 
  colnames(params_values) <- nm_types
  rownames(params_values) <- c('a', 'ke_2', 'C_sat')
  age <- fitted_params[19]
  n <- fitted_params[20]
  
  temperature = 22 #oC
  food = "high" #low/high
  L = Size_estimation(age,temperature,food) #mm
  dry_weight =  dry_weight_estimation(L) #mg
  F_rate <- Filtration_rate_estimation(L,temperature)#mL/h
  
  input <- c('sedimentation'=sedimentation,
             'score'=score,
             'N_iter'=N_iter,
             'method'=method)
  
  physiological_params <- c('Length_(mm)'=L,
                            'Dry_weight_(mg)'=dry_weight,
                            'Filtration_rate_(ml/h)'=F_rate)
  
  return(list('input'=input,
              'best_score'=optimization$objective,
              'optimized_params'=params_values,
              'age'=age,
              'n'=n,
              'physiological_params'=physiological_params
  ))
  
}
################################################################################


########################################

#---------------|---------|-------|-------|
#sedimentations | T       | F     |-------|
#---------------|---------|-------|-------|
#score          | PBKOF   | rmse  | AAFE  |
#---------------|---------|-------|-------|
#method         | Preuss  | Burns |-------|
#---------------|---------|-------|-------|


tests <- list('T PBKOF Preuss',
              'T PBKOF Burns',
              'T rmse Preuss',
              'T rmse Burns',
              'T AAFE Preuss',
              'T AAFE Burns',
              'F PBKOF Preuss',
              'F PBKOF Burns',
              'F rmse Preuss',
              'F rmse Burns',
              'F AAFE Preuss',
              'F AAFE Burns')

Initialization_ls <- list()

library(parallel)
start_time <- Sys.time()
numCores <- detectCores()
cl <- makeCluster(numCores-2)
s.time <- Sys.time()
paste0("The process started at ", s.time, ".")
output <- parLapply(cl, tests, simulation_func)
f.time <- Sys.time()
Total_duration <-  f.time - s.time
paste0("The process finished at ", f.time, ".")
paste0("Total duration of optimization: ", Total_duration)
stopCluster(cl)
########################################