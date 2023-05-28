
options(max.print=999999)

#LOAD PACKAGES
library(caret)
library(xgboost)
library(randomForest)
library(e1701)
library(neuralnet)


## R version 3.5.1
## SET WORKING DIRECTORY
setwd("C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/aggregation")
setwd("C:/Users/user/Documents/GitHub/TiO2_aggregation_and_uptake/aggregation")

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
for(i in 1:dim(zeta_df)[1] ){
  if(zeta_df[i,"Coating"]!="No"){
    zeta_df[i,"Coating"] <- "Yes"
  }
}

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


# Convert all categorical variables to factors
zeta_df$Coating <- as.factor(zeta_df$Coating )
zeta_df$Surface.property <- as.factor(zeta_df$Surface.property)
zeta_df$Inorganic.electrolyte <- as.factor(zeta_df$Inorganic.electrolyte)
zeta_df$Electrolyte.mixture <- as.factor(zeta_df$Electrolyte.mixture)
zeta_df$Light.Conditions <- as.factor(zeta_df$Light.Conditions)
zeta_df$Medium.type <- as.factor(zeta_df$Medium.type )
#zeta_df <- zeta_df[, !names(zeta_df) %in% c( "p1.ions","n1.ions")]
str(zeta_df)




studies <- unique(zeta_df$Study)
Test_studies <- c("Fan et al., 2016", "Fan et al., 2018", 
                  "Wang et al.2014", "Brzicova et al.2019",  "Almusallam et al., 2011", 
                  "Gong et al.2013",   "Marucco et al.2013", "Sun et al., 2014", 
                  "Zhu et al., 2015", "Avellán-Llaguno et al.2022", 
                  "Jiang et al., 2009", "Chowdhury et al., 2010"," Hu et al.2017", "Hu et al.2019",
                  "Ilina et al.2017")
Test_data <- zeta_df[zeta_df$Study %in% Test_studies,]
Test_id <- Test_data$Study
Test_data <- Test_data[,!names(Test_data) %in% "Study"]
Train_data <- zeta_df[!zeta_df$Study %in% Test_studies,]
Train_id <- Train_data$Study
Train_data <- Train_data[,!names(Train_data) %in% "Study"]


mean_y<-mean(Train_data$Zeta.Potential) ## 0
sd_y<-sd(Train_data$Zeta.Potential)


#Scale the dataset
preprocessParams2 <- preProcess(Train_data, method=c("center","scale"))
preprocessParams21 <- preProcess(Train_data[,!names(Train_data) %in%  "Zeta.Potential"], method=c("center","scale"))

Train_scaled<-predict(preprocessParams2, Train_data)
Test_scaled<-predict(preprocessParams2, Test_data)


## ONE HOT ENCODING 

dmyRTD<-dummyVars(~.,data=Train_scaled)
Train_scaled_dmy<-data.frame(predict(dmyRTD, newdata=Train_scaled))
Train_scaled_dmy[is.na(Train_scaled_dmy)]<--20
Test_scaled_dmy<-data.frame(predict(dmyRTD, newdata=Test_scaled))
Test_scaled_dmy[is.na(Test_scaled_dmy)]<--20

# 
# # Correlation  
# correlation <- cor(Train_scaled_dmy)
# for(i in 1:(dim(correlation)[1]-1)){
#   for (j in (i+1):dim(correlation)[2]){
#       if(abs(correlation[i,j]) > 0.6){
#         print(paste0(names(Train_scaled_dmy)[i], " with ", names(Train_scaled_dmy)[j], " have correlation: ",  correlation[i,j]))
#       }
#   }
# }

# RF
trainControl<-trainControl(method="repeatedcv", number=5, repeats=3)
tunegrid <- expand.grid(.mtry=c(30))
set.seed(135)
fitRF<-train( Zeta.Potential~ ., data = Train_scaled_dmy, method="rf", trControl=trainControl ,tuneGrid=tunegrid,
              ntree=50,importance=TRUE)
fitRF


library(tidyverse)
varImp(fitRF)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  theme_bw()


#SVM
# part c: save parameters in tune grid object
tune_grid_svm <- expand.grid(sigma = c(0.5),
                             C = c( 100))
set.seed(135)
fitSVM<-train(Zeta.Potential~ ., data = Train_scaled_dmy, method="svmRadial", 
              trControl=trainControl, tuneGrid=tune_grid_svm)
fitSVM


#GBM
trainControl <- trainControl(method="cv", number=5)

set.seed(99)
caretGrid <- expand.grid(interaction.depth=c(15), n.trees = c(10000),
                         shrinkage=c(0.01),
                         n.minobsinnode=c(0.1))
trainControl<-trainControl(method="repeatedcv", number=5, repeats=3,savePredictions=TRUE)
set.seed(63454)
fitGBM <- train( Zeta.Potential~ ., data = Train_scaled_dmy,distribution="gaussian", 
                 method = "gbm", trControl = trainControl,
                 tuneGrid=caretGrid, metric="Rsquared", bag.fraction=0.75, verbose=FALSE)
fitGBM

#NN
trainControl<-trainControl(method="repeatedcv", number=5, repeats=3,savePredictions=TRUE)
set.seed(135)
tune.grid.neuralnet <- expand.grid(
  layer1 = 30,
  layer2 = 100,
  layer3 = 30
)
fitNN <- caret::train(Zeta.Potential~ ., data = Train_scaled_dmy,
  method = "neuralnet",
  tuneGrid = tune.grid.neuralnet,
  trControl = trainControl, metric="MAE")
fitNN


#xgb 
trainControl<-trainControl(method="repeatedcv", number=5, repeats=3,verboseIter = FALSE,allowParallel = FALSE, savePredictions=TRUE)
tunegrid <- expand.grid(nrounds = 300,
                        max_depth = 8 ,
                        eta = 0.25,
                        gamma = 0,
                        colsample_bytree = 0.8,
                        min_child_weight = 1,
                        subsample = 0.5
)
set.seed(125)
fitXGB<-train(Zeta.Potential~ ., data = Train_scaled_dmy,method="xgbTree", trControl=trainControl ,
              tuneGrid=tunegrid , importance=TRUE,verbosity=0)
fitXGB
# 



#-----------------------------
# Predictions on Train
#-----------------------------

predRF_train_scaled<-predict(fitRF, newdata=Train_scaled_dmy)
predRF_train<-predRF_train_scaled*sd_y+mean_y
cor(predRF_train, Train_data$Zeta.Potential)
rmse(predRF_train,Train_data$Zeta.Potential)
Rsquare(predRF_train,Train_data$Zeta.Potential)
R2(predRF_train, Train_data$Zeta.Potential)
ggplot(Train_data, mapping=aes(Zeta.Potential,predRF_train, color=predRF_train))+
  geom_point()+
  geom_abline()

predSVM_train_scaled<-predict(fitSVM, newdata=Train_scaled_dmy)
predSVM_train<-predSVM_train_scaled*sd_y+mean_y
cor(predSVM_train, Train_data$Zeta.Potential)
rmse(predSVM_train,Train_data$Zeta.Potential)
Rsquare(predSVM_train,Train_data$Zeta.Potential)
R2(predSVM_train, Train_data$Zeta.Potential)
ggplot(Train_data, mapping=aes(Zeta.Potential,predSVM_train, color=predSVM_train))+
  geom_point()+
  geom_abline()


predNN_train_scaled<-predict(fitNN, newdata=Train_scaled_dmy)
predNN_traiN<-predNN_train_scaled*sd_y+mean_y
cor(predNN_traiN, Train_data$Zeta.Potential)
rmse(predNN_traiN,Train_data$Zeta.Potential)
Rsquare(predNN_traiN,Train_data$Zeta.Potential)
R2(predNN_traiN, Train_data$Zeta.Potential)
ggplot(Train_data, mapping=aes(Zeta.Potential,predNN_traiN, color=predNN_traiN))+
  geom_point()+
  geom_abline()


predXGB_train_scaled<-predict(fitXGB, newdata=Train_scaled_dmy)
predXGB_train<-predXGB_train_scaled*sd_y+mean_y
cor(predXGB_train, Train_data$Zeta.Potential)
rmse(predXGB_train,Train_data$Zeta.Potential)
Rsquare(predXGB_train,Train_data$Zeta.Potential)
R2(predXGB_train, Train_data$Zeta.Potential)
ggplot(Train_data, mapping=aes(Zeta.Potential,predXGB_train, color=predXGB_train))+
  geom_point()+
  geom_abline()


predGBM_train_scaled<-predict(fitGBM, newdata=Train_scaled_dmy)
predGBM_train<-predGBM_train_scaled*sd_y+mean_y
cor(predGBM_train, Train_data$Zeta.Potential)
rmse(predGBM_train,Train_data$Zeta.Potential)
Rsquare(predGBM_train,Train_data$Zeta.Potential)
R2(predGBM_train, Train_data$Zeta.Potential)
ggplot(Train_data, mapping=aes(Zeta.Potential,predGBM_train, color=predGBM_train))+
  geom_point()+
  geom_abline()
### PREDICTIONS OF THE MODELS ON THE TEST SET

predRF_test_scaled<-predict(fitRF, newdata=Test_scaled_dmy)
predRF_test<-predRF_test_scaled*sd_y+mean_y
cor(predRF_test, Test_data$Zeta.Potential)
rmse(predRF_test,Test_data$Zeta.Potential)
Rsquare(predRF_test,Test_data$Zeta.Potential)
R2(predRF_test, Test_data$Zeta.Potential)
ggplot(Test_data, mapping=aes(Zeta.Potential,predRF_test, color=predRF_test))+
  geom_point()+
  geom_abline()

predSVM_test_scaled<-predict(fitSVM, newdata=Test_scaled_dmy)
predSVM_test<-predSVM_test_scaled*sd_y+mean_y
cor(predSVM_test, Test_data$Zeta.Potential)
rmse(predSVM_test,Test_data$Zeta.Potential)
Rsquare(predSVM_test,Test_data$Zeta.Potential)
R2(predSVM_test, Test_data$Zeta.Potential)
ggplot(Test_data, mapping=aes(Zeta.Potential,predSVM_test, color=predSVM_test))+
  geom_point()+
  geom_abline()

predNN_test_scaled<-predict(fitNN, newdata=Test_scaled_dmy)
predNN_test<-predNN_test_scaled*sd_y+mean_y
cor(predNN_test, Test_data$Zeta.Potential)
rmse(predNN_test,Test_data$Zeta.Potential)
Rsquare(predNN_test,Test_data$Zeta.Potential)
R2(predNN_test, Test_data$Zeta.Potential)
ggplot(Test_data, mapping=aes(Zeta.Potential,predNN_test, color=predNN_test))+
  geom_point()+
  geom_abline()

predXGB_test_scaled<-predict(fitXGB, newdata=Test_scaled_dmy)
predXGB_test<-predXGB_test_scaled*sd_y+mean_y
cor(predXGB_test, Test_data$Zeta.Potential)
rmse(predXGB_test,Test_data$Zeta.Potential)
Rsquare(predXGB_test,Test_data$Zeta.Potential)
R2(predXGB_test, Test_data$Zeta.Potential)
ggplot(Test_data, mapping=aes(Zeta.Potential,predXGB_test, color=predXGB_test))+
  geom_point()+
  geom_abline()


predGBM_test_scaled<-predict(fitGBM, newdata=Test_scaled_dmy)
predGBM_test<-predGBM_test_scaled*sd_y+mean_y
cor(predGBM_test, Test_data$Zeta.Potential)
rmse(predGBM_test,Test_data$Zeta.Potential)
Rsquare(predGBM_test,Test_data$Zeta.Potential)
R2(predGBM_test, Test_data$Zeta.Potential)
ggplot(Test_data, mapping=aes(Zeta.Potential,predGBM_test, color=predGBM_test))+
  geom_point()+
  geom_abline()
### SUMMARY & MODEL CORRELATION

Resembles<-resamples(list(RF=fitRF,SVM=fitSVM, XGB=fitXGB, GBM=fitGBM ))
summary(Resembles)
dotplot(Resembles)
modelCor(Resembles, metric="RMSE")
splom(Resembles, metric="RMSE")




### SUMMARY & MODEL CORRELATION

Resembles<-resamples(list(RF=fitRF,XGB=fitXGB))
summary(Resembles)
dotplot(Resembles)
modelCor(Resembles, metric="RMSE")
splom(Resembles, metric="RMSE")





### ENSEMBLES ON THE TRAIN SET *********************************************
#### ENSEMBLE with 2 MODELS
#A.- RF+XGB
## Data
Ensemble_Train <-data.frame(RF= predRF_train_scaled, XGB=predXGB_train_scaled, Mean.HD = Train_scaled_dmy$Mean.HD)
Ensemble_Test <-data.frame(RF= predRF_test_scaled, XGB=predXGB_test_scaled, Mean.HD = Test_scaled_dmy$Mean.HD)

## Stack the Ensemble with RF algorithm

trainControl<-trainControl(method="repeatedcv", number=10, repeats=3,savePredictions=TRUE)
tunegrid <- expand.grid(.mtry=2)

set.seed(135)
RFEnsemble<-caret::train(Mean.HD~ ., data = Ensemble_Train, method="rf", trControl=trainControl,ntree=100,importance=TRUE)
RFEnsemble

## Make predictions on the datasets with the Ensemble

RFEnsembleTrain_scaled <- predict(RFEnsemble, newdata = Ensemble_Train)
RFEnsembleTrain <- RFEnsembleTrain_scaled*sd_y+mean_y
cor(RFEnsembleTrain, Train_data$Mean.HD)
R2(RFEnsembleTrain,  Train_data$Mean.HD)
Rsquare(RFEnsembleTrain,  Train_data$Mean.HD)
rmse(RFEnsembleTrain,  Train_data$Mean.HD)
ggplot(Train_data, mapping=aes(Mean.HD,RFEnsembleTrain, color=RFEnsembleTrain))+
  geom_point()+
  geom_abline()


RFEnsembleTest_scaled <- predict(RFEnsemble, newdata = Ensemble_Test)
RFEnsembleTest <- RFEnsembleTest_scaled*sd_y+mean_y
cor(RFEnsembleTest, Test_data$Mean.HD)
R2(RFEnsembleTest, Test_data$Mean.HD)
Rsquare(RFEnsembleTest, Test_data$Mean.HD)
rmse(RFEnsembleTest, Test_data$Mean.HD)
ggplot(Test_data, mapping=aes(Mean.HD,RFEnsembleTest, color=RFEnsembleTest))+
  geom_point()+
  geom_abline()


COMBO_Test<-data.frame( ID = Test_id, Data= Test_data$Mean.HD, RF=predRF_test, XGB =predXGB_test, ENS =RFEnsembleTest )
COMBO_Train<-data.frame( ID = Train_id, Data= Train_data$Mean.HD, RF=predRF_train, XGB =predXGB_train, ENS = RFEnsembleTrain)

