
options(max.print=999999)

#LOAD PACKAGES
library(caret)
library(xgboost)
library(randomForest)
library(neuralnet)
library(tidyverse)


## R version 3.5.1
## SET WORKING DIRECTORY
setwd("C:/Users/ptsir/Documents/GitHub/TiO2_aggregation_and_uptake/aggregation")
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


data<-openxlsx::read.xlsx("hydrodynamic_sizes_zeta_predicted.xlsx")
data$`Zeta.Potential.(mV)` <- as.numeric(data$`Zeta.Potential.(mV)`)
data$`Debye.length.est.(nm)` <- as.numeric(data$`Debye.length.est.(nm)`)

drops <- c("Doi","Nanoparticle","Polydispersity.index.(PDI)", 
           "Zeta.Potential.est.(mV)","Electrophoretic.Mobility.(1e-08m^2/s/V)",   "Attachement.efficiency" ,
           "Amorphous.crystal.(%)", "Rutile.Crystal.(%)",  "Dynamic.Viscosity.(mPA*s)", "Dielectric.constant",
           "Henry.function",  "Medium.Type.in.Publication", "Sonication.power.(W/L)", 
           "Stock.suspension.(Yes/No)", "UV.radiation.intensity.(mW/cm^2)", 
           "Debye.length.est.(nm)", "Sonication.frequency.(kHz)",
           "Medium.Group", "Inorganic.electrolyte","Temperature.(oC)",   "FBS.(%)",
           "Zeta.Potential_pred", "Zeta.Potential.(mV)")

data$zeta <- 0
for (i in 1:dim(data)[1]){
  if(!is.na(data[i, "Zeta.Potential.(mV)"])){
    data[i,"zeta"] <- data[i, "Zeta.Potential.(mV)"]
  }else{
    data[i,"zeta"] <- data[i, "Zeta.Potential_pred"]
  }
  
  if(data[i,"Coating"]!="No"){
    data[i,"Coating"] <- "Yes"
  }
}

aggregation_df <- data[ , !(names(data) %in% drops)]                  


colnames(aggregation_df) <- c("Study" , "Nominal.Size",  "Aspect.ratio",  "Nominal.SSA" ,    "Mean.HD",    
                       "Anatase.Crystal",  "Coating",  "Surface.property",       
                       "Time", "Concentration",   "Conductivity",  
                       "Ionic.Strength","Electrolyte.mixture", "p1.ions", "p2.ions", "p3.ions",
                       "n1.ions", "n2.ions", "n3.ions",   "Salinity", "Alkalinity", "Hardness", "TOC", "SS",             
                       "DOC", "TDS","NOM",  "Light.Conditions",
                       "pH",  "Medium.type", "BSA",   "Humic.Acid",  "Tannic.acid",  "Fulvic.Acid",      
                       "a-amylase",  "Alginate", "SDBS",  "SDS",
                       "Stock",      
                       "Sonication.time",    "Sonication.power", "Zeta.Potential")


# Convert all categorical variables to factors
aggregation_df$Coating <- as.factor(aggregation_df$Coating )
aggregation_df$Surface.property <- as.factor(aggregation_df$Surface.property)
# aggregation_df$Inorganic.electrolyte <- as.factor(aggregation_df$Inorganic.electrolyte)
aggregation_df$Electrolyte.mixture <- as.factor(aggregation_df$Electrolyte.mixture)
aggregation_df$Light.Conditions <- as.factor(aggregation_df$Light.Conditions)
aggregation_df$Medium.type <- as.factor(aggregation_df$Medium.type )
#aggregation_df <- aggregation_df[, !names(aggregation_df) %in% c( "p1.ions","n1.ions")]
str(aggregation_df)

# Drop missing values in hydrodynamic diameter and remove strings 
aggregation_df <-  aggregation_df[!is.na(aggregation_df$Mean.HD), ]
aggregation_df <- aggregation_df[!grepl(">", aggregation_df$Mean.HD), ]
aggregation_df$Mean.HD <- as.numeric(aggregation_df$Mean.HD)

#Remove all data points that are greater than 3000 because we dont consider them reliable
aggregation_df <- aggregation_df[!aggregation_df$Mean.HD>3000, ]


studies <- unique(aggregation_df$Study)
Test_studies <- c("Chen et al., 2019",      "Lu et al., 2015",         
                 "Arze et al.,2020",       "Tada-Oikawa et al.,2016",     "Fang et al., 2017",                   
                  "Suzuki et al., 2020",    "Tso et al., 2010",  
                   "Xiong et al., 2013", "Keller et al., 2010", "Tan et al., 2021",
                  "Hartmann et al.2009", "Dai et al., 2022", "Li et al.2013" )

Test_data <- aggregation_df[aggregation_df$Study %in% Test_studies,]
Test_id <- Test_data$Study
Test_data <- Test_data[,!names(Test_data) %in% "Study"]
Train_data <- aggregation_df[!aggregation_df$Study %in% Test_studies,]
Train_id <- Train_data$Study
Train_data <- Train_data[,!names(Train_data) %in% "Study"]


mean_y<-mean(Train_data$Mean.HD) ## 0
sd_y<-sd(Train_data$Mean.HD)


#Scale the dataset
preprocessParams2 <- preProcess(Train_data, method=c("center","scale"))

Train_scaled<-predict(preprocessParams2, Train_data)
Test_scaled<-predict(preprocessParams2, Test_data)


## ONE HOT ENCODING 

dmyRTD<-dummyVars(~.,data=Train_scaled)
Train_scaled_dmy<-data.frame(predict(dmyRTD, newdata=Train_scaled))
Train_scaled_dmy[is.na(Train_scaled_dmy)]<--20
Test_scaled_dmy<-data.frame(predict(dmyRTD, newdata=Test_scaled))
Test_scaled_dmy[is.na(Test_scaled_dmy)]<--20


# RF
trainControl<-trainControl(method="repeatedcv", number=5, repeats=3)
tunegrid <- expand.grid(.mtry=c(30))
set.seed(135)
fitRF<-train(Mean.HD ~ ., data = Train_scaled_dmy, method="rf", trControl=trainControl ,tuneGrid=tunegrid,
              ntree= 50,importance=TRUE)
fitRF
varImp(fitRF)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  theme_bw()


#XGB

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
fitXGB<-train(Mean.HD~ ., data = Train_scaled_dmy,method="xgbTree", trControl=trainControl ,
              tuneGrid=tunegrid , importance=TRUE,verbosity=0)
fitXGB


#SVM
# part c: save parameters in tune grid object
tune_grid_svm <- expand.grid(sigma = c(0.5),
                             C = c( 100))
set.seed(135)
fitSVM<-train(Mean.HD~ ., data = Train_scaled_dmy, method="svmRadial", 
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
fitGBM <- train( Mean.HD~ ., data = Train_scaled_dmy,distribution="gaussian", 
                 method = "gbm", trControl = trainControl,
                 tuneGrid=caretGrid, metric="Rsquared", bag.fraction=0.75, verbose=FALSE)
fitGBM

#NN
trainControl<-trainControl(method="repeatedcv", number=5, repeats=3,savePredictions=TRUE)
set.seed(135)
tune.grid.neuralnet <- expand.grid(
  layer1 = 100,
  layer2 = 100,
  layer3 = 100
)
fitNN <- caret::train(Mean.HD~ ., data = Train_scaled_dmy,
                      method = "neuralnet",
                      tuneGrid = tune.grid.neuralnet,
                      trControl = trainControl, metric="MAE")
fitNN


#-----------------------------
# Predictions on Train
#-----------------------------

predRF_train_scaled<-predict(fitRF, newdata=Train_scaled_dmy)
predRF_train<-predRF_train_scaled*sd_y+mean_y
cor(predRF_train, Train_data$Mean.HD)
rmse(predRF_train,Train_data$Mean.HD)
Rsquare(predRF_train,Train_data$Mean.HD)
R2(predRF_train, Train_data$Mean.HD)
ggplot(Train_data, mapping=aes(Mean.HD,predRF_train, color=predRF_train))+
  geom_point()+
  geom_abline()

predSVM_train_scaled<-predict(fitSVM, newdata=Train_scaled_dmy)
predSVM_train<-predSVM_train_scaled*sd_y+mean_y
cor(predSVM_train, Train_data$Mean.HD)
rmse(predSVM_train,Train_data$Mean.HD)
Rsquare(predSVM_train,Train_data$Mean.HD)
R2(predSVM_train, Train_data$Mean.HD)
ggplot(Train_data, mapping=aes(Mean.HD,predSVM_train, color=predSVM_train))+
  geom_point()+
  geom_abline()


predNN_train_scaled<-predict(fitNN, newdata=Train_scaled_dmy)
predNN_traiN<-predNN_train_scaled*sd_y+mean_y
cor(predNN_traiN, Train_data$Mean.HD)
rmse(predNN_traiN,Train_data$Mean.HD)
Rsquare(predNN_traiN,Train_data$Mean.HD)
R2(predNN_traiN, Train_data$Mean.HD)
ggplot(Train_data, mapping=aes(Mean.HD,predNN_traiN, color=predNN_traiN))+
  geom_point()+
  geom_abline()


predXGB_train_scaled<-predict(fitXGB, newdata=Train_scaled_dmy)
predXGB_train<-predXGB_train_scaled*sd_y+mean_y
cor(predXGB_train, Train_data$Mean.HD)
rmse(predXGB_train,Train_data$Mean.HD)
Rsquare(predXGB_train,Train_data$Mean.HD)
R2(predXGB_train, Train_data$Mean.HD)
ggplot(Train_data, mapping=aes(Mean.HD,predXGB_train, color=predXGB_train))+
  geom_point()+
  geom_abline()


predGBM_train_scaled<-predict(fitGBM, newdata=Train_scaled_dmy)
predGBM_train<-predGBM_train_scaled*sd_y+mean_y
cor(predGBM_train, Train_data$Mean.HD)
rmse(predGBM_train,Train_data$Mean.HD)
Rsquare(predGBM_train,Train_data$Mean.HD)
R2(predGBM_train, Train_data$Mean.HD)
ggplot(Train_data, mapping=aes(Mean.HD,predGBM_train, color=predGBM_train))+
  geom_point()+
  geom_abline()
### PREDICTIONS OF THE MODELS ON THE TEST SET

predRF_test_scaled<-predict(fitRF, newdata=Test_scaled_dmy)
predRF_test<-predRF_test_scaled*sd_y+mean_y
cor(predRF_test, Test_data$Mean.HD)
rmse(predRF_test,Test_data$Mean.HD)
Rsquare(predRF_test,Test_data$Mean.HD)
R2(predRF_test, Test_data$Mean.HD)
ggplot(Test_data, mapping=aes(Mean.HD,predRF_test, color=predRF_test))+
  geom_point()+
  geom_abline()

predSVM_test_scaled<-predict(fitSVM, newdata=Test_scaled_dmy)
predSVM_test<-predSVM_test_scaled*sd_y+mean_y
cor(predSVM_test, Test_data$Mean.HD)
rmse(predSVM_test,Test_data$Mean.HD)
Rsquare(predSVM_test,Test_data$Mean.HD)
R2(predSVM_test, Test_data$Mean.HD)
ggplot(Test_data, mapping=aes(Mean.HD,predSVM_test, color=predSVM_test))+
  geom_point()+
  geom_abline()

predNN_test_scaled<-predict(fitNN, newdata=Test_scaled_dmy)
predNN_test<-predNN_test_scaled*sd_y+mean_y
cor(predNN_test, Test_data$Mean.HD)
rmse(predNN_test,Test_data$Mean.HD)
Rsquare(predNN_test,Test_data$Mean.HD)
R2(predNN_test, Test_data$Mean.HD)
ggplot(Test_data, mapping=aes(Mean.HD,predNN_test, color=predNN_test))+
  geom_point()+
  geom_abline()

predXGB_test_scaled<-predict(fitXGB, newdata=Test_scaled_dmy)
predXGB_test<-predXGB_test_scaled*sd_y+mean_y
cor(predXGB_test, Test_data$Mean.HD)
rmse(predXGB_test,Test_data$Mean.HD)
Rsquare(predXGB_test,Test_data$Mean.HD)
R2(predXGB_test, Test_data$Mean.HD)
ggplot(Test_data, mapping=aes(Mean.HD,predXGB_test, color=predXGB_test))+
  geom_point()+
  geom_abline()


predGBM_test_scaled<-predict(fitGBM, newdata=Test_scaled_dmy)
predGBM_test<-predGBM_test_scaled*sd_y+mean_y
cor(predGBM_test, Test_data$Mean.HD)
rmse(predGBM_test,Test_data$Mean.HD)
Rsquare(predGBM_test,Test_data$Mean.HD)
R2(predGBM_test, Test_data$Mean.HD)
ggplot(Test_data, mapping=aes(Mean.HD,predGBM_test, color=predGBM_test))+
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

