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
  out <- ode(y = state, times = times, func = differential, parms = params, method="bdf",rtol = 1e-05, atol = 1e-05)
  out <- out[out[,1] %in% time,]
  sum = 0
  for (i in 1:length(time)){
    sum <- sum + (out[i,2]-diameter[i])^2
  }
  return(sum)
}


setwd("~/")
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
out <- ode(y = state, times = times, func = differential,
           parms = list("a"=optimization$solution[1], "b"=optimization$solution[2]))
                        #"c"=optimization$solution[3], "d"=optimization$solution[4]))
  

plot(out)

