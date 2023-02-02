# This is a script to simulate the experiments in Fan et al., 2016
# The first experiment is about the waterborne exposure of D. Magna to TiO2 nanoparticles
# The second is about the trophic exposure to TiO2 exposured algae.

# Working directory

dir = 'C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/Exposure/Fan_2018'
setwd(dir)
dir_2 <- 'C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/'

# dir <- 'C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/Exposure/Fan_2018'
# dir_uptake <- 'C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/'
# dir_filtration <- 'C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/'

#=================#
#  Water exposure #
#=================#

# The following dataframe is a mappin of the of the TiO2 NPs types with 
# their corresponding codes (which are simple letters)
A <- c("A", "TiO2-H0")
B <- c("B", "TiO2-S0")
C <- c("C", "TiO2-H1")
D <- c("D", "TiO2-H3")
E <- c("E", "TiO2-S3")
G <- c("G", "TiO2-H5")
H <- c("H", "TiO2-S5")

Mapping <- data.frame(rbind(A, B, C, D, E, G, H))
colnames(Mapping) <- c("Code", "Type")
i <- 1:dim(Mapping)[2]
Mapping[,i] <- apply(Mapping[,i], 2,
                     function(x) as.character(x))
sapply(Mapping, class)

# Load data  for water exposure - uptake phase

# Uptake - exposure = 1 mg/L
# Time is given in minutes
# Concentrations are given as mg Ti/g dry daphnia
C1_uptake <- read.csv('data/1_uptake.csv')

# Uptake - exposure = 10 mg/L
# Time is given in minutes
# Concentrations are given as mg Ti/g dry daphnia
C2_uptake <- read.csv('data/10_uptake.csv')

# Transform time from minutes to hours
C1_uptake[,1] <- C1_uptake[,1]/60
C2_uptake[,1] <- C2_uptake[,1]/60

# Transform measured concentrations from 1 g dry basis to 1 mg dry basis of daphnia magna
C1_uptake[,2:9] <- C1_uptake[,2:9]/1000 
C2_uptake[,2:9] <- C2_uptake[,2:9]/1000

# Transform the concentrations of Ti to concentrations of TiO2
Ti_H_frac <- 73.15/100 # the mass fraction of Ti in all H type Nanoparticles measured in Fan et al. (2018)
Ti_S_frac <- 74.85/100 # the mass fraction of Ti in all S type Nanoparticles measured in Fan et al. (2018)

# Divide the concentrations of all H nanoparticles with Ti_H_frac
C1_uptake[,c(2,4,6,8)] <- C1_uptake[,c(2,4,6,8)]/Ti_H_frac  # mg TiO2/mg DW D. magna
C2_uptake[,c(2,4,6,8)] <- C2_uptake[,c(2,4,6,8)]/Ti_H_frac  # mg TiO2/mg DW D. magna

# Divide the concentrations of all S nanoparticles with Ti_S_frac
C1_uptake[,c(3,5,7,9)] <- C1_uptake[,c(3,5,7,9)]/Ti_S_frac  # mg TiO2/mg DW D. magna
C2_uptake[,c(3,5,7,9)] <- C2_uptake[,c(3,5,7,9)]/Ti_S_frac  # mg TiO2/mg DW D. magna

# Depuration - exposure = 1 mg/L
# Time is given in hours
# values represent the retained TiO2 concentration as a % ratio of the 
# measured concentration after a 2-hour exposure
C1_depuration <- read.csv('data/1_depuration.csv')

# Depuration - exposure = 10 mg/L
# Time is given in hours
# values represent the retained TiO2 concentration as a % ratio of the 
# measured concentration after a 2-hour exposure
C2_depuration <- read.csv('data/10_depuration.csv')

# Load the measured concentrations at the end of depuration phase
# given in the supplementary material of Fan et al. (2018)
# The concentrations are given as mg Ti/ g dry weight of D. magna
final_concentrations <- read.csv('data/final_concentrations.csv')

# Transform the concentrations to mg TiO2 / mg dry daphnia 
# as we did previously
final_concentrations[,c(2,4,6,8)] <- final_concentrations[,c(2,4,6,8)]/Ti_H_frac/1000
final_concentrations[,c(3,5,7,9)] <- final_concentrations[,c(3,5,7,9)]/Ti_S_frac/1000

# Calculate the actual concentrations measured during the depuration phase
# The concentration at the beginning of the depuration is C_2h (because it followed
# a 2-hour exposure) and is equal to C_2h = C / %_ratio
C_2h_C1 <- final_concentrations[1,2:9]/(C1_depuration[6,2:9]/100) 
C_2h_C2 <- final_concentrations[2,2:9]/(C2_depuration[6,2:9]/100) 

# The measured concentrations at 2 hours between the exposure and the depuration
# experiments have small differences. For this reason, we will take the mean value 
# of this 2 measurments and we will replace the concentrations at 2 hours 
# at dataframes C1_uptake and C2_uptake

# C1_uptake: correct the values at 2 hours
C_2h_corrected <- c()
for (i in 1:length(C_2h_C1)) { # loop for every TiO2 type
  C_2h_corrected[i] <-  mean(c(C1_uptake[4,i+1], as.numeric(C_2h_C1[i]))) 
}
# replace the C1_uptake concentrations at 2 hours with the corrected values
C1_uptake[4,2:9] <- C_2h_corrected


# C2_uptake: correct the values at 2 hours
C_2h_corrected <- c()
for (i in 1:length(C_2h_C2)) { # loop for every TiO2 type
  C_2h_corrected[i] <-  mean(c(C2_uptake[4,i+1], as.numeric(C_2h_C2[i]))) 
}
# replace the C1_uptake concentrations at 2 hours with the corrected values
C2_uptake[4,2:9] <- C_2h_corrected
