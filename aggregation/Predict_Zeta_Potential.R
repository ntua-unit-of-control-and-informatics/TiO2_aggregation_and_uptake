
options(max.print=999999)

#LOAD PACKAGES
library(caret)
library(xgboost)
library(randomForest)
library(neuralnet)
library(ggplot2)

#### FUNCTIONS FOR MODEL STATISTICS
rmse = function(m, o){
  sqrt(mean((m - o)^2))
}

## Rsquared FUNCTION (2 DIFFERENT WAYS OF CALCULATION)

Rsquare=function(m,o){
  (cov(m,o)/sqrt(var(m)*var(o)))^2
}

R2=function(m,o){
  y<-mean(o)
  metric1<-(o - m) ^ 2
  metric2<- (o-y)^2
  s1<-sum(metric1)
  s2<-sum(metric2)
  ss<-s1/s2
  1-s1/s2
}

## SET WORKING DIRECTORY
setwd("C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/aggregation")

data<-openxlsx::read.xlsx("hydrodynamic_sizes.xlsx")
data$`Zeta.Potential.(mV)` <- as.numeric(data$`Zeta.Potential.(mV)`)
data$`Debye.length.est.(nm)` <- as.numeric(data$`Debye.length.est.(nm)`)
zeta_df <- data[!is.na(data$`Zeta.Potential.(mV)`),]

drops <- c("Doi","Nanoparticle", "Mean.Hydrodynamic.size.mean.(nm)", "Polydispersity.index.(PDI)", 
           "Zeta.Potential.est.(mV)","Electrophoretic.Mobility.(1e-08m^2/s/V)",   "Attachement.efficiency" ,
           "Amorphous.crystal.(%)", "Rutile.Crystal.(%)",  "Dynamic.Viscosity.(mPA*s)", "Dielectric.constant",
           "Henry.function",  "Medium.Type.in.Publication", "Sonication.power.(W/L)", 
           "Stock.suspension.(Yes/No)", "UV.radiation.intensity.(mW/cm^2)", 
           "Debye.length.est.(nm)", "Sonication.frequency.(kHz)",
           "Medium.Group",  
           "Conductivity.(mS/cm)",  "Inorganic.electrolyte",
           "Temperature.(oC)",   "FBS.(%)", "General.medium.type", "NOM.(mg/L)", "Surface.property", "Coating",
           "SS.(mg/L)","Aspect.ratio", "Electrolyte.mixture.(Yes/No/Not_Applicable)",
           "Concentration.of.+2.charge.ions.(mol/L)", "TDS.(mg/L)", "Concentration.of.-1.charge.ions.(mol/L)",
           "Concentration.of.+1.charge.ions.(mol/L)")

zeta_df <- zeta_df[ , !(names(zeta_df) %in% drops)]                  

colnames(zeta_df) <- c("Study" , "Nominal.Size", "Nominal.SSA" ,     
                       "Zeta.Potential","Anatase.Crystal",         
                       "Time", "Concentration",  
                       "Ionic.Strength",      "p3.ions",    
                       "n2.ions", "n3.ions", "Salinity", "Alkalinity", "Hardness", "TOC",              
                       "DOC",  "Light.Conditions",
                       "pH",   "BSA", "Humic.Acid", "Tannic.acid",      
                       "Fulvic.Acid", "a-amylase", "Alginate","SDBS","SDS", 
                       "Stock",      
                       "Sonication.time",    "Sonication.power")

mean_y<-mean(zeta_df$Zeta.Potential, na.rm = TRUE) ## 0
sd_y<-sd(zeta_df$Zeta.Potential, na.rm = TRUE)


#Scale the dataset
preprocessParams2 <- preProcess(zeta_df, method=c("center","scale"))
zeta_scaled<-predict(preprocessParams2, zeta_df)


## ONE HOT ENCODING 

dmyRTD<-dummyVars(~.,data=zeta_scaled)
zeta_scaled_dmy<-data.frame(predict(dmyRTD, newdata=zeta_scaled))
zeta_scaled_dmy[is.na(zeta_scaled_dmy)]<--20

# RF
trainControl<-trainControl(method="repeatedcv", number=5, repeats=3)
tunegrid <- expand.grid(.mtry=c(30))
set.seed(135)
RF_all<-train( Zeta.Potential~ ., data = zeta_scaled_dmy, method="rf", trControl=trainControl ,tuneGrid=tunegrid,
              ntree=50,importance=TRUE)
RF_all


pred_zeta_scaled<-predict(RF_all, newdata=zeta_scaled_dmy)
pred_zeta <- pred_zeta_scaled*sd_y+mean_y
cor(pred_zeta, zeta_df$Zeta.Potential)
rmse(pred_zeta,zeta_df$Zeta.Potential)
Rsquare(pred_zeta,zeta_df$Zeta.Potential)
R2(pred_zeta, zeta_df$Zeta.Potential)
ggplot(zeta_df, mapping=aes(Zeta.Potential,pred_zeta, color=pred_zeta))+
  geom_point()+
  geom_abline()

#--------------------
#Predict on all 
#------------------

data<-openxlsx::read.xlsx("hydrodynamic_sizes.xlsx")
data$`Zeta.Potential.(mV)` <- as.numeric(data$`Zeta.Potential.(mV)`)
data$`Debye.length.est.(nm)` <- as.numeric(data$`Debye.length.est.(nm)`)

drops <- c("Doi","Nanoparticle", "Mean.Hydrodynamic.size.mean.(nm)", "Polydispersity.index.(PDI)", 
           "Zeta.Potential.est.(mV)","Electrophoretic.Mobility.(1e-08m^2/s/V)",   "Attachement.efficiency" ,
           "Amorphous.crystal.(%)", "Rutile.Crystal.(%)",  "Dynamic.Viscosity.(mPA*s)", "Dielectric.constant",
           "Henry.function",  "Medium.Type.in.Publication", "Sonication.power.(W/L)", 
           "Stock.suspension.(Yes/No)", "UV.radiation.intensity.(mW/cm^2)", 
           "Debye.length.est.(nm)", "Sonication.frequency.(kHz)",
           "Medium.Group",  
           "Conductivity.(mS/cm)",  "Inorganic.electrolyte",
           "Temperature.(oC)",   "FBS.(%)", "General.medium.type", "NOM.(mg/L)", "Surface.property", "Coating",
           "SS.(mg/L)","Aspect.ratio", "Electrolyte.mixture.(Yes/No/Not_Applicable)",
           "Concentration.of.+2.charge.ions.(mol/L)", "TDS.(mg/L)", "Concentration.of.-1.charge.ions.(mol/L)",
           "Concentration.of.+1.charge.ions.(mol/L)")

df <- data[ , !(names(data) %in% drops)]                  

colnames(df) <- c("Study" , "Nominal.Size", "Nominal.SSA" ,     
                       "Zeta.Potential","Anatase.Crystal",         
                       "Time", "Concentration",  
                       "Ionic.Strength",      "p3.ions",    
                       "n2.ions", "n3.ions", "Salinity", "Alkalinity", "Hardness", "TOC",              
                       "DOC",  "Light.Conditions",
                       "pH",   "BSA", "Humic.Acid", "Tannic.acid",      
                       "Fulvic.Acid", "a-amylase", "Alginate","SDBS","SDS", 
                       "Stock",      
                       "Sonication.time",    "Sonication.power")
df_scaled<-predict(preprocessParams2, df)
df_scaled_dmy<-data.frame(predict(dmyRTD, newdata=df_scaled))
df_scaled_dmy[is.na(df_scaled_dmy)]<--20
preds_scaled<-predict(RF_all, newdata=df_scaled_dmy)
preds <- preds_scaled*sd_y+mean_y

write.csv(preds, "zeta_predictions.csv")