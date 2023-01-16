# This is a script to simulate the experiments in Cehn et al., 2019
# The first experiment is about the waterborne exposure of D. Magna to TiO2 nanoparticles
# The second is about the trophic exposure to TiO2 exposured algae.

# Working directory
setwd('C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/Exposure/Chen_2019/water_exposure')

#=====================================#
# Functions used for the optimization #
#=====================================#

# ode_func(): the differential equation system that deiscribes the model

ode_func <- function(time, inits, params){
  with(as.list(c(inits, params)),{
    
    # C_water: TiO2 concentration in water
    dC_water <- - (k_sed + ku + F_rate)*C_water
    
    # Algae
    dC_algae <- ku*Cwater - ke_1*C_algae - C_algae/L1
    
    # Daphnia magna
    dC_daphnia = F_rate*C_water + I*C_algae - ke_2*C_daphnia - C_daphnia/L2
     
    list(dC_water, dC_algae, dC_daphnia)
  })
}

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

# Load data for water exposure only scenario
# Nominal concentratiopn: mg/L
# Actual concentratiopn: mg/L
# Daphnia concentration: mg/kg (dry or wet weight ????)

water_exp_data <- openxlsx::read.xlsx('water_exposure_data.xlsx')

df_A <- data.frame()


