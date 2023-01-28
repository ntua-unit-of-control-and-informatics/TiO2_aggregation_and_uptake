

#Total Amount of TiO2
V = 0.1 # Medium volume [L]
C = 1 # TiO2 concentration [mg/L]
M_init = C*V # Mass of TiO2 in medium [mg]


age = 8 # [days]
N = 20 # Number of individuals in beaker of volume V
w = weight_calc(age)
Frate = w*Filtration_rate_func(22, w)

# Maximum Removal of TiO2 by daphnids if tank had steady concentration (continuous renewal)
M_eaten = 24*(Frate/1000)*C
C_Daphnid = M_eaten/w 
print(C_Daphnid)
Total_eaten = N* M_eaten


M_end =  M_init - Total_eaten
C_end = M_end/V
print(C_end)


(max(solution$M_water)-max(solution$M_daphnia_tot))/V


solution[solution$time==23.8,"C_water"]
print(paste0("sediment: ", solution[solution$time==23.8,5]))

print(paste0("Total in Daphnia: ", solution[solution$time==23.8,2]))

print(paste0("Water : ", solution[solution$time==23.8,4]))

# Time Weighted Average
mean(c(solution[solution$time ==0,"C_water"], solution[solution$time ==3,"C_water"], 
       solution[solution$time ==6,"C_water"], solution[solution$time ==12,"C_water"], 
       solution[solution$time ==23.9,"C_water"]))