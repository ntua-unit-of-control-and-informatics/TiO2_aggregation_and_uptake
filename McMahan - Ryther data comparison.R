setwd("C:/Users/vassi/Documents/LAB")

### Ryther (1954) data - Chlorella vulgaris
data <- read.csv("ryther_data2.csv", header=F) 
colnames(data) <- c("Concentration", "Filtration_rate")
dim(data)

data[,1] <- data[,1]
#data_plot <- cbind(c(data, data[,2]*data[,1]))
ingestion_rate <- data$Filtration_rate*data$Concentration

data_ryther <- cbind(data, ingestion_rate)
names(data_ryther)[3] <- "Ingestion_rate"

# plot(x=data_ryther$Concentration, y=data_ryther$Ingestion_rate)
# plot(x=data_ryther$Concentration, y=data_ryther$`Filtration_rate`)
# 
# data$`Filtration_rate`


#### McMahon and Rigler (1965) data - Chlorella vulgaris
data2 <- read.csv("McMahon_data.csv", header=F) 
colnames(data2) <- c("Concentration","Ingestion_rate") 
# Concentration units: 1e06 cell/ml
# Feeding rate units: 1e06 cells/ind/hour


cell_weight <- 2e-11 # g/cell
Concentration_mass <- data2$Concentration*cell_weight # g algae/ml
Filtration_rate <- data2$Ingestion_rate/data2$Concentration # 
# Filtration_rate units: ml of medium/hour

data_mcmahon <- cbind(data2, Filtration_rate)

#plot(data2$Concentration, Filtration_rate)

library(ggplot2)
Ingestion_rates_plot <- ggplot()+
  geom_line(data = data_ryther, aes(x=Concentration, y=Ingestion_rate,colour = "Ryther"), size=1.7)+
  geom_line(data = data_mcmahon, aes(x=Concentration, y=Ingestion_rate, colour = "McMahon"), size=1.7)+
  labs(title = "Ingestion rate vs Algae concentration",
       y = "Ingestion rate (10^6 cells/(ind*hour))",
       x = "Algae Concentration (10^6 cells/ml)")+
  theme(plot.title = element_text(hjust = 0.5,size=30), 
        axis.title.y =element_text(hjust = 0.5,size=25,face="bold"),
        axis.text.y=element_text(size=22),
        axis.title.x =element_text(hjust = 0.5,size=25,face="bold"),
        axis.text.x=element_text(size=22),
        legend.title=element_text(hjust = 0.5,size=25), 
        legend.text=element_text(size=22)) + 
  #scale_color_manual("Color", values=cls)+
  theme(legend.key.size = unit(1.5, 'cm'),  
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.text = element_text(size = 14))
Ingestion_rates_plot


Filtration_rates_plot <- ggplot()+
  geom_line(data = data_ryther, aes(x=Concentration, y=Filtration_rate,colour = "Ryther"), size=1.7)+
  geom_line(data = data_mcmahon, aes(x=Concentration, y=Filtration_rate, colour = "McMahon"), size=1.7)+
  labs(title = "Filtration rate vs Algae concentration",
       y = "Filtration rate (ml/hour)",
       x = "Algae Concentration (10^6 cells/ml)")+
  theme(plot.title = element_text(hjust = 0.5,size=30), 
        axis.title.y =element_text(hjust = 0.5,size=25,face="bold"),
        axis.text.y=element_text(size=22),
        axis.title.x =element_text(hjust = 0.5,size=25,face="bold"),
        axis.text.x=element_text(size=22),
        legend.title=element_text(hjust = 0.5,size=25), 
        legend.text=element_text(size=22)) + 
  #scale_color_manual("Color", values=cls)+
  theme(legend.key.size = unit(1.5, 'cm'),  
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.text = element_text(size = 14))
Filtration_rates_plot


# Filtration = f(C_algae): Fit a 2nd order plynomial to McMahon data for Chlorella Vulgaris
ill_point <- 0.2
max_filtration_rate <- mean(data_mcmahon$Filtration_rate[1:3]) #2.7
data_after_ill_point <- data_mcmahon[data_mcmahon$Concentration > ill_point,]
data_after_ill_point <- rbind(c(ill_point, ill_point*max_filtration_rate, max_filtration_rate), data_after_ill_point)
filtration_rate_model <- lm(Filtration_rate ~ poly(Concentration, 2, raw=T), data=data_after_ill_point)
summary(filtration_rate_model)
model_coefficients <- filtration_rate_model$coefficients


Filtration_rate <- function(c_algae, ill_point, max_filtration_rate, model_coefficients){
  filtration_results <- c()
  for (i in 1:length(c_algae)) {
    c <- c_algae[i]
    if(c_algae[i] <= ill_point){
      filtration_results[i] <- max_filtration_rate
    }else{
      filtration_results[i] <- model_coefficients[1] + model_coefficients[2]*c + model_coefficients[3]*(c^2) 
    }    
  }
  return(filtration_results)
}

c_seq <- seq(0,1,0.01)
y_pred <- Filtration_rate(c_seq, ill_point, max_filtration_rate, model_coefficients)
y_obs <- data_mcmahon$Filtration_rate

data_model <-as.data.frame(cbind(c_seq, y_pred))
colnames(data_model)[1] <- "Concentration"

ggplot()+
  geom_line(data=data_model, aes(x=Concentration, y=y_pred, colour = "Prediction"), size=1.7)+
  geom_point(data=data_mcmahon, aes(x=Concentration, y=Filtration_rate, colour = "Observed"), size=1.7)

