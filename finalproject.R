setwd("/Users/jaeyoungbaak/Desktop/skku/2022-2/finance/Final_report/data")
library(MCS)
load("results_forecasts_Final.RData")
data = read.csv("DataKorea.csv", header = TRUE)

tcode = data[1,]  # first element: Transformation code
tcode

### Transformation 
data = data[-1,]  # deleting transform line
data = data[-(153:154),] # delete missing(~22.08)

tdata = data[-1,] # maximum: first differnce
head(tdata[,1:5])
ncol(data)
for (i in 2:ncol(data)){
  
  if(tcode[i] == 1){
    tdata[,i] <- data[-1,i]
  } # no transformation  
  
  if(tcode[i] == 2){
    tdata[,i] <- diff(data[,i])
  } # first difference
  
  if(tcode[i] == 4){
    tdata[,i] <- log(data[-1,i])
  } # no transformation  
  
  if(tcode[i] == 5){
    tdata[,i] <- diff(log(data[,i]))
  } # log differencing
}
# column 103: COFIX
Y = cbind(tdata[,103],tdata[,c(-1,-103)]) # Y: (COFIX, rest of the data), without date
Y = as.matrix(Y)
Y
#========================================================================
# Forecasting
#========================================================================
# window size: 121
# number of prediction = 30(20%), tdata(151obs)
# forecast horizon: 1,3,6

#Random walk
source("main/functions/func-rw.r")
rw1=rw.rolling.window(Y,npred=30,indice=1,lag=1)
rw3=rw.rolling.window(Y,npred=30,indice=1,lag=3)
rw6=rw.rolling.window(Y,npred=30,indice=1,lag=6)

#AR
source("main/functions/func-ar.r")
ar1=ar.rolling.window(Y,nprev=30,indice=1,lag=1)
ar3=ar.rolling.window(Y,nprev=30,indice=1,lag=3)
ar6=ar.rolling.window(Y,nprev=30,indice=1,lag=6)

#Lasso
source("main/functions/func-lasso.r")
library(glmnet)
library(HDeconometrics)
lasso1=lasso.rolling.window(Y,nprev=30,indice=1,lag=1,alpha=1,type="lasso")
lasso3=lasso.rolling.window(Y,nprev=30,indice=1,lag=3,alpha=1,type="lasso")
lasso6=lasso.rolling.window(Y,nprev=30,indice=1,lag=6,alpha=1,type="lasso")

#adaptive Lasso
adalasso1=lasso.rolling.window(Y,nprev=30,indice=1,lag=1,alpha=1,type="adalasso")
adalasso3=lasso.rolling.window(Y,nprev=30,indice=1,lag=3,alpha=1,type="adalasso")
adalasso6=lasso.rolling.window(Y,nprev=30,indice=1,lag=6,alpha=1,type="adalasso")

#Elastic net
elasticnet1=lasso.rolling.window(Y,nprev=30,indice=1,lag=1,alpha=0.5,type="lasso")
elasticnet3=lasso.rolling.window(Y,nprev=30,indice=1,lag=3,alpha=0.5,type="lasso")
elasticnet6=lasso.rolling.window(Y,nprev=30,indice=1,lag=6,alpha=0.5,type="lasso")

#adaptive elastic net  
adaelasticnet1=lasso.rolling.window(Y,nprev=30,indice=1,lag=1,alpha=0.5,type="adalasso")
adaelasticnet3=lasso.rolling.window(Y,nprev=30,indice=1,lag=3,alpha=0.5,type="adalasso")
adaelasticnet6=lasso.rolling.window(Y,nprev=30,indice=1,lag=6,alpha=0.5,type="adalasso")

#Ridge (alpha=0)
ridge1=lasso.rolling.window(Y,nprev=30,indice=1,lag=1,alpha=0,type="ridge")
ridge3=lasso.rolling.window(Y,nprev=30,indice=1,lag=3,alpha=0,type="ridge")
ridge6=lasso.rolling.window(Y,nprev=30,indice=1,lag=6,alpha=0,type="ridge")


#target factors  ####문제있음
source("main/functions/func-fact.r")
source("main/functions/func-baggit.r")
source("main/functions/func-tfact.r")
tfact1=tfact.rolling.window(Y,nprev=30,indice=1,lag=1)
tfact3=tfact.rolling.window(Y,nprev=30,indice=1,lag=3)
tfact6=tfact.rolling.window(Y,nprev=30,indice=1,lag=6)


#random forest
source("main/functions/func-rf.r")
library("randomForest")
rf1=rf.rolling.window(Y,nprev=30,indice=1,lag=1)
rf3=rf.rolling.window(Y,nprev=30,indice=1,lag=3)
rf6=rf.rolling.window(Y,nprev=30,indice=1,lag=6)


#xgboost
source("main/functions/func-xgb.r")
library("xgboost")
xgb1=xgb.rolling.window(Y,nprev=30,indice=1,lag=1)
xgb3=xgb.rolling.window(Y,nprev=30,indice=1,lag=3)
xgb6=xgb.rolling.window(Y,nprev=30,indice=1,lag=6)

#lstm
source("main/functions/func-LSTM.r")
library(keras)
library(tidyverse)
library(tensorflow)
lstm1=mul.lstm.rolling.window(Y, nprev=30,indice=1,lag=1)
lstm3=mul.lstm.rolling.window(Y, nprev=30,indice=1,lag=3)
lstm6=mul.lstm.rolling.window(Y, nprev=30,indice=1,lag=6)


lag1_error=rbind(rw1$errors,ar1$errors,lasso1$errors,adalasso1$errors,elasticnet1$errors,adaelasticnet1$errors,ridge1$errors,tfact1$errors,rf1$errors,xgb1$errors)
rownames(lag1_error) <- c('rw', 'ar', 'lasso', 'adalasso', 'elasticnet', 'adaelasticnet','ridge','tfact', 'rf', 'xgb')

lag3_error=rbind(rw3$errors,ar3$errors,lasso3$errors,adalasso3$errors,elasticnet3$errors,adaelasticnet3$errors,ridge3$errors,tfact3$errors,rf3$errors,xgb3$errors)
rownames(lag3_error) <- c('rw', 'ar', 'lasso', 'adalasso', 'elasticnet', 'adaelasticnet','ridge','tfact', 'rf', 'xgb')

lag6_error=rbind(rw6$errors,ar6$errors,lasso6$errors,adalasso6$errors,elasticnet6$errors,adaelasticnet6$errors,ridge6$errors,tfact6$errors,rf6$errors,xgb6$errors)
rownames(lag6_error) <- c('rw', 'ar', 'lasso', 'adalasso', 'elasticnet', 'adaelasticnet','ridge','tfact', 'rf', 'xgb')

lag1_pred=cbind(rw1$pred,ar1$pred,lasso1$pred,adalasso1$pred,elasticnet1$pred,adaelasticnet1$pred,ridge1$pred,tfact1$pred,rf1$pred,xgb1$pred)
colnames(lag1_pred) <- c('rw', 'ar', 'lasso', 'adalasso', 'elasticnet', 'adaelasticnet','ridge','tfact', 'rf', 'xgb') 

lag3_pred=cbind(rw3$pred,ar3$pred,lasso3$pred,adalasso3$pred,elasticnet3$pred,adaelasticnet3$pred,ridge3$pred,tfact3$pred,rf3$pred,xgb3$pred)
colnames(lag3_pred) <- c('rw', 'ar', 'lasso', 'adalasso', 'elasticnet', 'adaelasticnet','ridge','tfact', 'rf', 'xgb')

lag6_pred=cbind(rw6$pred,ar6$pred,lasso6$pred,adalasso6$pred,elasticnet6$pred,adaelasticnet6$pred,ridge6$pred,tfact6$pred,rf6$pred,xgb6$pred)
colnames(lag6_pred) <- c('rw', 'ar', 'lasso', 'adalasso', 'elasticnet', 'adaelasticnet','ridge','tfact', 'rf', 'xgb')
write.csv(lag1_error, 'lag1_error.csv')
write.csv(lag1_pred, 'lag1_pred.csv')
write.csv(lag3_error, 'lag3_error.csv')
write.csv(lag3_pred, 'lag3_pred.csv')
write.csv(lag6_error, 'lag6_error.csv')
write.csv(lag6_pred, 'lag6_pred.csv')
save.image("results_forecasts_Final.RData")

#####################################
### Boruta Algorithm for RF & XGB ###
####################################################
### Ranking Variables using the Boruta Algorithm ###
####################################################
library(Boruta)
library(MCS)
##### one month ahead #####
Y1 = Y  ## Using the whole sample 
lag = 1 ## Practice one-month ahead
aux = embed(Y1,4+lag)
y=aux[,1]
X=aux[,-c(1:(ncol(Y1)*lag))]
set.seed(42)
boruta_1 <- Boruta(X, y, maxRuns = 100)
boruta_1
plot = plot(boruta_1)
plot
# 14 important; 374 unimportant; 20 tentative.
attstats_1 = attStats(boruta_1)
attstats_1

write.csv(attstats_1,"Boruta_1.csv",sep=";",row.names = FALSE, col.names = FALSE)

order_1 = order(attstats_1$meanImp, decreasing = T)
order_1
## Cross Validation for Optimal Number of Variables ## up to 14 variables for one-month ahead

Errors = rep(NA,14)          

for (i in 2:14){
  
  selected_1 = order_1[1:i]
  
  model=randomForest(X[,selected_1], y, importance=TRUE)
  
  pred = model$predicted     
  error = mean((pred-y)^2)
  
  Errors[i] <- error
}

plot(c(1:14), Errors, xlab="# of Variables", ylab="Fitted Squared Error", type = "l") # x- # of using variables

Errors_1 = Errors

# Rolling Window with Selected Variables
varOrder_1 = order(attstats_1$meanImp, decreasing = T)
varOrder_1
which.min(Errors_1) # until 12th variables
selected_1 = varOrder_1[1:which.min(Errors_1)] # The Set of Optimal Number of Variables:12




##### 3 months ahead #####
Y3 = Y  ## Using the whole sample 
lag = 3 ## Practice one-month ahead
aux = embed(Y3,4+lag)
y=aux[,1]
X=aux[,-c(1:(ncol(Y3)*lag))]
set.seed(42)
boruta_3 <- Boruta(X, y, maxRuns = 100)
boruta_3
plot = plot(boruta_3)
plot
# 14 important; 385 unimportant; 9 tentative.
attstats_3 = attStats(boruta_3)
attstats_3

write.csv(attstats_3,"Boruta_3.csv",sep=";",row.names = FALSE, col.names = FALSE)

order_3 = order(attstats_3$meanImp, decreasing = T)
order_3
## Cross Validation for Optimal Number of Variables ## up to 14 variables for one-month ahead

Errors = rep(NA,14)          

for (i in 2:14){
  
  selected_3 = order_3[1:i]
  
  model=randomForest(X[,selected_3], y, importance=TRUE)
  
  pred = model$predicted     
  error = mean((pred-y)^2)
  
  Errors[i] <- error
}

plot(c(1:14), Errors, xlab="# of Variables", ylab="Fitted Squared Error", type = "l") # x- # of using variables

Errors_3 = Errors

# Rolling Window with Selected Variables
varOrder_3 = order(attstats_3$meanImp, decreasing = T)
varOrder_3
which.min(Errors_3) # 8 variables
selected_3 = varOrder_3[1:which.min(Errors_3)] 


##### six months ahead #####
Y6 = Y  ## Using the whole sample 
lag = 6 ## Practice one-month ahead
aux = embed(Y6,4+lag)
y=aux[,1]
X=aux[,-c(1:(ncol(Y6)*lag))]
set.seed(42)
boruta_6 <- Boruta(X, y, maxRuns = 100)
boruta_6
plot = plot(boruta_6)
plot
# 16 important; 369 unimportant; 23 tentative.
attstats_6 = attStats(boruta_6)
attstats_6

write.csv(attstats_6,"Boruta_6.csv",sep=";",row.names = FALSE, col.names = FALSE)

order_6 = order(attstats_6$meanImp, decreasing = T)
order_6
## Cross Validation for Optimal Number of Variables
## up to 16 variables for six-month ahead

Errors = rep(NA,16)         

for (i in 2:16){
  
  selected_6 = order_6[1:i]
  
  model=randomForest(X[,selected_6], y, importance=TRUE)
  
  pred = model$predicted     
  error = mean((pred-y)^2)
  
  Errors[i] <- error
}

plot(c(1:16), Errors, xlab="# of Variables", ylab="Fitted Squared Error", type = "l") # 12 variables.

Errors_6 = Errors

# Rolling Window with Selected Variables


varOrder_6 = order(attstats_6$meanImp, decreasing = T)
which.min(Errors_6) # 12 variables
selected_6 = varOrder_6[1:which.min(Errors_6)] 

source("main/functions/func-rf_selected2022.R")
source("main/functions/func-xgb_selected2022.R")
#lag_1: Rf, xgb
rf1_selected = rf.rolling.window(Y1,nprev=30,1,1,selected_1)
xgb1_selected=xgb.rolling.window(Y1,nprev=30,1,1,selected=selected_1)

#lag_3: Rf, xgb
rf3_selected = rf.rolling.window(Y3,nprev=30,1,3,selected_3)
xgb3_selected=xgb.rolling.window(Y3,nprev=30,indice=1,lag=3,selected_3)

#lag_6: Rf, xgb
rf6_selected = rf.rolling.window(Y6,nprev=30,1,6,selected_6)
xgb6_selected=xgb.rolling.window(Y6,nprev=30,indice=1,lag=6,selected_6)

lag1_error=rbind(rw1$errors,ar1$errors,lasso1$errors,adalasso1$errors,elasticnet1$errors,adaelasticnet1$errors,ridge1$errors,tfact1$errors,rf1$errors,rf1_selected$errors,xgb1$errors,xgb1_selected$errors)
rownames(lag1_error) <- c('rw', 'ar', 'lasso', 'adalasso', 'elasticnet', 'adaelasticnet','ridge','tfact', 'rf','rf_selected' ,'xgb', 'xgb_selected')

lag3_error=rbind(rw3$errors,ar3$errors,lasso3$errors,adalasso3$errors,elasticnet3$errors,adaelasticnet3$errors,ridge3$errors,tfact3$errors,rf3$errors,rf3_selected$errors,xgb3$errors,xgb3_selected$errors)
rownames(lag3_error) <- c('rw', 'ar', 'lasso', 'adalasso', 'elasticnet', 'adaelasticnet','ridge','tfact', 'rf','rf_selected' ,'xgb', 'xgb_selected')

lag6_error=rbind(rw6$errors,ar6$errors,lasso6$errors,adalasso6$errors,elasticnet6$errors,adaelasticnet6$errors,ridge6$errors,tfact6$errors,rf6$errors,rf6_selected$errors,xgb6$errors,xgb6_selected$errors)
rownames(lag6_error) <- c('rw', 'ar', 'lasso', 'adalasso', 'elasticnet', 'adaelasticnet','ridge','tfact', 'rf','rf_selected' ,'xgb', 'xgb_selected')

lag1_pred=cbind(rw1$pred,ar1$pred,lasso1$pred,adalasso1$pred,elasticnet1$pred,adaelasticnet1$pred,ridge1$pred,tfact1$pred,rf1$pred,rf1_selected$pred,xgb1$pred,xgb1_selected$pred)
colnames(lag1_pred) <- c('rw', 'ar', 'lasso', 'adalasso', 'elasticnet', 'adaelasticnet','ridge','tfact', 'rf','rf_selected' ,'xgb', 'xgb_selected')

lag3_pred=cbind(rw3$pred,ar3$pred,lasso3$pred,adalasso3$pred,elasticnet3$pred,adaelasticnet3$pred,ridge3$pred,tfact3$pred,rf3$pred,rf3_selected$pred,xgb3$pred,xgb3_selected$pred)
colnames(lag3_pred) <- c('rw', 'ar', 'lasso', 'adalasso', 'elasticnet', 'adaelasticnet','ridge','tfact', 'rf','rf_selected' ,'xgb', 'xgb_selected')

lag6_pred=cbind(rw6$pred,ar6$pred,lasso6$pred,adalasso6$pred,elasticnet6$pred,adaelasticnet6$pred,ridge6$pred,tfact6$pred,rf6$pred,rf6_selected$pred,xgb6$pred,xgb6_selected$pred)
colnames(lag6_pred) <- c('rw', 'ar', 'lasso', 'adalasso', 'elasticnet', 'adaelasticnet','ridge','tfact', 'rf','rf_selected' ,'xgb', 'xgb_selected')


write.csv(lag1_error, 'lag1_error_selected.csv')
write.csv(lag1_pred, 'lag1_pred_selected.csv')
write.csv(lag3_error, 'lag3_error_selected.csv')
write.csv(lag3_pred, 'lag3_pred_selected.csv')
write.csv(lag6_error, 'lag6_error_selected.csv')
write.csv(lag6_pred, 'lag6_pred_selected.csv')

save.image("results_forecasts_Final.RData")
#========================================================================
#Giacomini White Test
#========================================================================
source("main/gwtest.R")
library(sandwich)
real=tail(Y[,1],30) # 30:number of predictions

# lag=1, RF_selected, Xgb_selected vs other models
# lag=3, RF_selected, Xgb_selected vs other models
# lag=6, RF_selected, Xgb_selected vs other models

gwtest_rf_lag1 = matrix(NA,10,1)
gwtest_rf_lag3 = matrix(NA,10,1)
gwtest_rf_lag6 = matrix(NA,10,1)
gwpvalue_rf_lag1 = matrix(NA,10,1)
gwpvalue_rf_lag3 = matrix(NA,10,1)
gwpvalue_rf_lag6 = matrix(NA,10,1)

gwtest_xgb_lag1 = matrix(NA,10,1)
gwtest_xgb_lag3 = matrix(NA,10,1)
gwtest_xgb_lag6 = matrix(NA,10,1)
gwpvalue_xgb_lag1 = matrix(NA,10,1)
gwpvalue_xgb_lag3 = matrix(NA,10,1)
gwpvalue_xgb_lag6 = matrix(NA,10,1)


#lag1_rf_selected
for(i in 1:10){
  
  gw = gw.test(lag1_pred[,i], lag1_pred[,11], real, tau=1, T=30, method="NeweyWest") #tau가 forecast horizon
  
  gwtest_rf_lag1[i,1] <- gw$statistic
  gwpvalue_rf_lag1[i,1] <- gw$p.value
}

#lag3_rf_selected
for(i in 1:10){
  
  gw = gw.test(lag3_pred[,i], lag3_pred[,11], real, tau=3, T=30, method="NeweyWest") #tau가 forecast horizon
  
  gwtest_rf_lag3[i,1] <- gw$statistic
  gwpvalue_rf_lag3[i,1] <- gw$p.value
}

#lag6_rf_selected
for(i in 1:10){
  
  gw = gw.test(lag6_pred[,i], lag6_pred[,11], real, tau=6, T=30, method="NeweyWest") #tau가 forecast horizon
  
  gwtest_rf_lag6[i,1] <- gw$statistic
  gwpvalue_rf_lag6[i,1] <- gw$p.value
}

#lag1_xgb_selected
for(i in 1:10){
  
  gw = gw.test(lag1_pred[,i], lag1_pred[,12], real, tau=1, T=30, method="NeweyWest") #tau가 forecast horizon
  
  gwtest_xgb_lag1[i,1] <- gw$statistic
  gwpvalue_xgb_lag1[i,1] <- gw$p.value
}

#lag3_xgb_selected
for(i in 1:10){
  
  gw = gw.test(lag3_pred[,i], lag3_pred[,12], real, tau=3, T=30, method="NeweyWest") #tau가 forecast horizon
  
  gwtest_xgb_lag3[i,1] <- gw$statistic
  gwpvalue_xgb_lag3[i,1] <- gw$p.value
}

#lag6_xgb_selected
for(i in 1:10){
  
  gw = gw.test(lag6_pred[,i], lag6_pred[,12], real, tau=6, T=30, method="NeweyWest") #tau가 forecast horizon
  
  gwtest_xgb_lag6[i,1] <- gw$statistic
  gwpvalue_xgb_lag6[i,1] <- gw$p.value
}

gwtest_rf = cbind(gwtest_rf_lag1,gwtest_rf_lag3,gwtest_rf_lag6)
gwpvalue_rf = cbind(gwpvalue_rf_lag1, gwpvalue_rf_lag3, gwpvalue_rf_lag6)

gwtest_xgb = cbind(gwtest_xgb_lag1, gwtest_xgb_lag3, gwtest_xgb_lag6)
gwpvalue_xgb = cbind(gwpvalue_xgb_lag1, gwpvalue_xgb_lag3, gwpvalue_xgb_lag6)


gwpvalue_rf < 0.05
gwpvalue_rf < 0.10

gwpvalue_xgb < 0.05
gwpvalue_xgb < 0.10

write.csv(gwtest_rf, 'gwtest_rf.csv')
write.csv(gwpvalue_rf, 'gwpvalue_rf.csv')

write.csv(gwtest_xgb, 'gwtest_xgb.csv')
write.csv(gwpvalue_xgb, 'gwpvalue_xgb.csv')

# GW test
# The null hypothesis states that the predictive accuracy of the two models is equal. 
# Therefore, if the p-value is less than 0.05, it indicates that the model with lower forecast loss has statistically significantly better predictive performance.
#========================================================================
#Model Confidence Set (MCS) Test
#========================================================================
library(MCS)

LOSS_lag1 = lag1_pred - real
sqLOSS_lag1 = LOSS_lag1^2
SSM_1 <- MCSprocedure(sqLOSS_lag1, alpha=0.5, B=5000, statistic="Tmax")

LOSS_lag3 = lag3_pred - real
sqLOSS_lag3 = LOSS_lag3^2
SSM_3 <- MCSprocedure(sqLOSS_lag3, alpha=0.5, B=5000, statistic="Tmax")

LOSS_lag6 = lag6_pred - real
sqLOSS_lag6 = LOSS_lag6^2
SSM_6 <- MCSprocedure(sqLOSS_lag6, alpha=0.5, B=5000, statistic="Tmax")

SSM_1
SSM_3
SSM_6
save.image("results_forecasts_Final.RData")
#========================================================================
# Predicting 2022.09, 2022.11, 2023.2 
#========================================================================
source("main/functions/func-rf-2_selected2022.r")
source("main/functions/func-xgb-2_selected2022.r")
rf_real1=rf2.rolling.window(Y1,nprev=1,1,1,selected_1)
rf_real2=rf2.rolling.window(Y1,nprev=1,1,2,selected_1)
rf_real3=rf2.rolling.window(Y1,nprev=1,1,3,selected_3)
rf_real6=rf2.rolling.window(Y1,nprev=1,1,6,selected_6)
xgb_real1=xgb2.rolling.window(Y,nprev=1,indice=1,lag=1,selected_1)
xgb_real2=xgb2.rolling.window(Y,nprev=1,indice=1,lag=2,selected_1)
xgb_real3=xgb2.rolling.window(Y,nprev=1,indice=1,lag=3,selected_3)
xgb_real6=xgb2.rolling.window(Y,nprev=1,indice=1,lag=6,selected_6)

rf_real_pred = rbind(rf_real1$pred,rf_real2$pred,rf_real3$pred,rf_real6$pred)
xgb_real_pred = rbind(xgb_real1$pred,xgb_real2$pred,xgb_real3$pred,xgb_real6$pred)
rf_real_pred
xgb_real_pred
real_pred = cbind(rf_real_pred,xgb_real_pred)
write.csv(real_pred, 'real_pred.csv')
save.image("results_forecasts_Final.RData")
