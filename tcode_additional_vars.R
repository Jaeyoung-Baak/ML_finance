setwd("/Users/jaeyoungbaak/Desktop/skku/2022-2/finance/Final_report/data")
library("readxl")
library(fbi)
library(glmnet)
library(HDeconometrics)
library(fbi)
library(urca)
library(psych)

data = read.csv("DataKorea.csv", header = TRUE)
data = data[-(1:1),-(2:97)] # delete tcode
data = data[-(153:155),]

tdata = data[-(1:2),]

Test = matrix(NA, 4, 6)
Check = matrix(NA, 4, 6)
plot(data[,7], type = "l") # COFIX_new # tcode - 2
plot(diff(data[,7]), type = "l")

for (i in 7){
  tdata[,i] <- diff(data[-1,i])
  
  Test[1,i-1] <- ur.df(tdata[,i], type='drift')@teststat[,1]
  Test[2,i-1] <- ur.df(tdata[,i], type='trend')@teststat[,1]
  Test[3,i-1] <- ur.kpss(tdata[,i], type='tau', lags='long')@teststat
  Test[4,i-1] <- ur.kpss(tdata[,i], type='mu', lags='long')@teststat
  
  Check[1,i-1] <- Test[1,i-1] < ur.df(tdata[,i], type='drift')@cval[1,1]
  Check[2,i-1] <- Test[2,i-1] < ur.df(tdata[,i], type='trend')@cval[1,1]
  Check[3,i-1] <- Test[3,i-1] < ur.kpss(tdata[,i], type='tau', lags='long')@cval[1,1]
  Check[4,i-1] <- Test[4,i-1] < ur.kpss(tdata[,i], type='mu', lags='long')@cval[1,1]
}
Test
describe(data[,7])
describe(tdata[,7])
plot(tdata[,7], type = "l") # COFIX_new


plot(data[,2], type = "l") # MPU (monetary) # tcode 4
plot(data[,3], type = "l") # FPU (fiscal) # tcode 4 - w/trend
plot(data[,4], type = "l") # TPU (trade) # tcode 4 - w/drift
plot(data[,5], type = "l") # FxPU # tcode 4 - w/drift
plot(data[,6], type = "l") # overall EPU # tcode 4 - w/trend

########## 
for (i in 2:6){
  tdata[,i] <- log(data[-(1:2),i])
  
  Test[1,i-1] <- ur.df(tdata[,i], type='drift')@teststat[,1]
  Test[2,i-1] <- ur.df(tdata[,i], type='trend')@teststat[,1]
  Test[3,i-1] <- ur.kpss(tdata[,i], type='tau', lags='long')@teststat
  Test[4,i-1] <- ur.kpss(tdata[,i], type='mu', lags='long')@teststat
  
  Check[1,i-1] <- Test[1,i-1] < ur.df(tdata[,i], type='drift')@cval[1,1]
  Check[2,i-1] <- Test[2,i-1] < ur.df(tdata[,i], type='trend')@cval[1,1]
  Check[3,i-1] <- Test[3,i-1] < ur.kpss(tdata[,i], type='tau', lags='long')@cval[1,1]
  Check[4,i-1] <- Test[4,i-1] < ur.kpss(tdata[,i], type='mu', lags='long')@cval[1,1]
}
plot(tdata[,2], type = "l") # MPU (monetary) # tcode 4
plot(tdata[,3], type = "l") # FPU (fiscal) # tcode 4 - w/trend
plot(tdata[,4], type = "l") # TPU (trade) # tcode 4 - w/drift
plot(tdata[,5], type = "l") # FxPU # tcode 4 - w/drift
plot(tdata[,6], type = "l") # overall EPU # tcode 4 - w/trend



# Reference
for (i in 2:ncol(data)){
  
  if(tcode[i] == 1){
    tdata[,i] <- data[-(1:2),i]
  } # no transformation  
  
  if(tcode[i] == 2){
    tdata[,i] <- diff(data[-1,i])
  } # 1차 차분
  
  if(tcode[i] == 3){
    tdata[,i] <- diff(diff(data[,i]))
  } # tcode ==3(2차 차분)에 해당하는 데이터는 없음
  
  if(tcode[i] == 4){
    tdata[,i] <- log(data[-(1:2),i])
  } # log
  
  if(tcode[i] == 5){
    tdata[,i] <- diff(log(data[-1,i]))
  } # log differencing
  
  if(tcode[i] == 6){
    tdata[,i] <- diff(diff(log(data[,i])))
  } # log취한 뒤 2차 차분
  
  if(tcode[i] == 7){
    tdata[,i] <- diff(data[-1,i]/data[1:(nrow(data)-1),i])
  } # 증가율의 1차 차분
}