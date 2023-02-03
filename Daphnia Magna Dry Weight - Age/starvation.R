setwd("C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake//Daphnia Magna Dry Weight - Age")
starvation <- read.csv("C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/Daphnia Magna Dry Weight - Age/starvation.csv")
starvation

# First part
time1 <- starvation$Time[1:4]
dw1 <-  starvation$DW_quant[1:4]
test1 <- data.frame(time = time1, dw = dw1)
nonlin <- function(t, a, b) { 1- a * (1-exp(-b*t)) }
nlsfit2 <- nls(dw ~ nonlin(time, a, b), data=test1, start=list(a=0.32, b=32))

plot(time1, dw1)
lines(seq(0,2,0.01), (1- 0.32*(1-exp(-36.6*seq(0,2,0.01)))))

# Secomd part
time2 <- starvation$Time[4:8]
dw2 <-  starvation$DW_quant[4:8]
test2 <- data.frame(time = time2, dw = dw2)
nonlin2 <- function(t, a, b,c) { 0.682- a * (1-c*exp(-b*t)) }
nlsfit2 <- nls(dw ~ nonlin2(time, a, b, c), data=test2, start=list(a=0.438, b=1.15, c =10 ))

plot(time2, dw2)
lines(seq(2,6,0.01), (0.682 - 0.4349 * (1- 16.7901*exp(-1.4106*seq(2,6,0.01)))))


time <- seq(0,6,0.0001)
y <-ifelse(time<2, (1- 0.32*(1-exp(-36.6*time))), (0.682 - 0.435 * (1- 16.79*exp(-1.41*time))))

plot(starvation$Time, starvation$DW_quant)
lines(time,y)

