######################## Xgboost ##########################
# Import the Boosting function
source('functions/func-xgb.R')
library(HDeconometrics)
library(xgboost)

xgb1=xgb.rolling.window(Y,npred,1,1)
xgb3=xgb.rolling.window(Y,npred,1,3)
xgb6=xgb.rolling.window(Y,npred,1,6)
xgb9=xgb.rolling.window(Y,npred,1,9)
xgb12=xgb.rolling.window(Y,npred,1,12)

# Get the error
xgb.rmse = cbind(xgb1$errors[1], xgb3$errors[1], xgb6$errors[1], xgb9$errors[1], xgb12$errors[1])
xgb.mae = cbind(xgb1$errors[2], xgb3$errors[2], xgb6$errors[2], xgb9$errors[2], xgb12$errors[2])

# Get the prediction
xgb.pred = cbind(xgb1$pred, xgb3$pred, xgb6$pred, xgb9$pred, xgb12$pred)


######################## Neural Networks(Deep Learning) ##########################
source("functions/func-nn.R")

library(dplyr)
library(keras)

library(h2o)
h2o.init()

nn1=nn.rolling.window(Y,npred,1,1)
nn3=nn.rolling.window(Y,npred,1,3)
nn6=nn.rolling.window(Y,npred,1,6)
nn9=nn.rolling.window(Y,npred,1,9)
nn12=nn.rolling.window(Y,npred,1,12)

xgb3=xgb.rolling.window(Y,npred,1,3)
xgb6=xgb.rolling.window(Y,npred,1,6)
xgb9=xgb.rolling.window(Y,npred,1,9)
xgb12=xgb.rolling.window(Y,npred,1,12)

# Get the error
nn.rmse = cbind(nn1$errors[1], nn3$errors[1], nn6$errors[1], nn9$errors[1], nn12$errors[1])
nn.mae = cbind(nn1$errors[2], nn3$errors[2], nn6$errors[2], nn9$errors[2], nn12$errors[2])

# Get the prediction
nn.pred = cbind(nn1$pred, xnn3$pred, nn6$pred, nnb9$pred, nn12$pred)
