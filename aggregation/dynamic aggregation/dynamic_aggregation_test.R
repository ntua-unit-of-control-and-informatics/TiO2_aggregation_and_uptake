library(ggplot2)
## Chaos in the atmosphere
differential <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <- a*exp(b*t)
   
    list(c(dX))
  })
}

obj<- function(x,time,diameter){
  state <- c(X = 671)
  times <-  seq(1,time[length(time)],0.1)
  params <- list("a" = x[1], "b" = x[2])
  out <- deSolve::ode(y = state, times = times, func = differential, parms = params, method="bdf",rtol = 1e-05, atol = 1e-05)
  out <- out[out[,1] %in% time,]
  sum = 0
  for (i in 1:length(time)){
    sum <- sum + (out[i,2]-diameter[i])^2
  }
  return(sum)
}


setwd("C:/Users/vassi/Documents/GitHub/TiO2_aggregation_and_uptake/aggregation/dynamic aggregation")
data <- read.csv("Hydrodynamic_diameter_data.csv") 
time <- round(data[2:dim(data)[1],1])
diameter <- data[2:dim(data)[1],2]
N_iter <- 1000
names(x0) <- c("a", "b")

opts <- list( "algorithm" = "NLOPT_LN_SBPLX", #"NLOPT_LN_NEWUOA",  #"NLOPT_LN_SBPLX" ,
              "xtol_rel" = 1e-06,
              "ftol_rel" = 0.0,
              "ftol_abs" = 0.0,
              "xtol_abs" = 0.0 ,
              "maxeval" = 0.0,
              "print_level" = 1)


x0 = c(5, -0.7)

optimization <- nloptr::nloptr( x0 = x0,
                                eval_f = obj,
                                opts = opts, time = time, diameter = diameter)
state      <- c(X = 671)

out <- as.data.frame(deSolve::ode(y = state, times = time, func = differential,
                                 parms = list("a"=optimization$solution[1],
                                              "b"=optimization$solution[2])))
                    #"c"=optimization$solution[3], "d"=optimization$solution[4]))
  

plot(data$Time,data$Diameter, pch=19)
lines(out)

ggplot(data, aes(x=Time, y=Diameter))+
  geom_point()+
  geom_line(data = out, aes(x=out[,1], y=out[,2]))

# polynomial model 

polynomial_model <- lm(Diameter ~ poly(Time, 2, raw=T), data=data)
summary(polynomial_model)

#y_pred <- predict(polynomial_model)
time_pred <- seq(0,150,0.1)
y_pred <- polynomial_model$coefficients[1] + polynomial_model$coefficients[2]*time_pred +polynomial_model$coefficients[3]*time_pred^2

data_pred <- as.data.frame(cbind(time_pred, y_pred))

colnames(data_pred) <- c("Time", "y_pred")

ggplot(data, aes(x=Time, y=Diameter))+
  geom_point()+
  geom_line(data = data_pred, aes(x=Time, y=y_pred))

plot(density(rlnorm(1000, 5.8, 0.37)))



