


Frate <- function(C, C_sat = 0.28667491, F_max =F_max,n=2.31671804){
  return(F_max*(1-(C/C_sat)^n))
}

n <-4
C_sat <- 0.200
age <- 5
L = Size_estimation(age) #mm
dry_weight =  dry_weight_estimation(L) #mg
F_max <- Filtration_rate_estimation(L)#mL/h
C = seq(0.01,C_sat,0.001)

F_est <- Frate(C,  C_sat =C_sat, F_max =F_max, n=n)

plot(C,F_est)
abline(h = F_max/3, v = 0.16485149)
