###############################################################################
### Forecasting Inflation in a Data Rich Environment (Medeiros et al, 2019) ###

### = Replicating TABLE S.12 - Forecasting Errors for CPI (1990-2000) = 

### Boruta Algorithm

###############################################################################


setwd("/Users/jaeyoungbaak/Desktop/skku/2022-2/finance/HW3/main")    


load("results_forecasts.RData")  #forecasts from Inflation_1990-2000_Forecasting.R 
#real=tail(Y[,1],132)


########################################################
### = Ranking Variables using the Boruta Algorithm = ###
########################################################
#install.packages("Boruta")
library(Boruta)

Y2 = Y                  ## Using the Whole Sample 
# Y2 = Y[1:359,]        ## Using only the First Window

# Example for lag = 12 setting
lag = 12
  
aux = embed(Y2,4+lag)
y=aux[,1]
X=aux[,-c(1:(ncol(Y2)*lag))]


# load("results_Boruta12.RData")  #아래 명령은 시간이 오래 걸림
boruta_12 <- Boruta(X, y, maxRuns = 100)
  
plot = plot(boruta_12)
plot
# 71 important; 350 unimportant; 67 tentative.
attstats = attStats(boruta_12)
attstats

write.csv(attstats,"Boruta_12.csv",sep=";",row.names = FALSE, col.names = FALSE)

order = order(attstats$meanImp, decreasing = T)

order
## Cross Validation for Optimal Number of Variables # (Up to 70 Variables)

Errors = rep(NA,70)          

#install.packages("MCS")
library(MCS)
#load("Boruta_result.RData")  #아래 loop 시간이 오래 걸림
for (i in 2:70){
  
  selected = order[1:i]
  
  model=randomForest(X[,selected], y, importance=TRUE)
  
  pred = model$predicted     
  error = mean((pred-y)^2)
  
  Errors[i] <- error
}

plot(c(1:70), Errors, xlab="# of Variables", ylab="Fitted Squared Error") # x- # of using variables

Errors1 = Errors

# Rolling Window with Selected Variables


varOrder = order(attstats$meanImp, decreasing = T)   # Ordering of Variables
which.min(Errors1)                                    # Optimal Number of Variables : 70 but, plot - 최초로 꺾이는 부분을 사용하는게 좋음.
selected = varOrder[1:which.min(Errors1)]             # The Set of Optimal Number of Variables
#selected = varOrder[1:16]                 # 꺾이는 부분. 16개까지만 사용 


source("func-rf_selected2022.R")

rf1_selected = rf.rolling.window(Y2,npred,1,1,selected)

#library(foreach)
#install.packages("doSNOW")
#library(doSNOW)
#registerDoSNOW(makeCluster(3, type="SOCK")) # For parallel computing, core 3개인 경우임.

# parallel computing이 된다면, func-rf_selected2022.R 파일의 15번째 줄인 
# model=randomForeast(X[,selected],y,importance=TRUE) 대신
# model <- foreach(ntree = rep(167, 3), .combine=randomForest::combine, .multicombine=TRUE, .packages = "randomForest") %dopar% randomForest(X[,selected],y,ntree=ntree, importance=TRUE) 로 수정해서 돌릴 것.  


######  
# install.packages("Rccp")  # 문제가 있을 때 추가 패키지
