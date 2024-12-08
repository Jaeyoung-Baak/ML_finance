# LSTM Function  

# ==================================================================
# Install packages and Recall Library
setwd("/Users/jaeyoungbaak/Desktop/skku/2022-2/finance/HW3/main")
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
#library(reshape2)
library(dplyr)
#install.packages("tensorflow")  #관리자 권한으로 실행 
library(tensorflow)
#install_tensorflow()

#reticulate::py_discover_config()

#install.packages("keras")  #관리자 권한으로 실행
library(keras) 
install_keras()

tf$constant("hello")
tf$version

# use_condaenv("r-tensorflow") if error loading tensorflow


# ==================================================================
# Normalization (Necessary)

normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

# Inverse Normalization 
denormalize <- function(x, minval, maxval) {
  x*(maxval-minval) + minval
}

# ==================================================================
# Load Data Set

getwd()
dir()

load("rawdata.rda")

Y=dados

# ==================================================================
# US inflation forecasting example explanation

nprev=132
indice = 1
lag = 1

comp = princomp(scale(Y,scale=FALSE))
Y2 = cbind(Y,comp$scores[,1:4]) %>% as.data.frame()
Y3 = lapply(Y2, normalize) %>% as.data.frame() %>% as.matrix()   # normalizing Y2

Y2 %>% dim() # 491 126 : 126 variables
Y2 %>% colnames() # Can see CPI Included with 4 Factor Components
Y3 %>% dim() # 491 126 : Same with Y2 since it's only normalized 

aux = embed(Y3, 4+lag)
aux %>% dim() # 487 630 : Note 630 - 126 = 504 
# Check aux

Y3 %>% head() # CPI
Y3[,1] %>% head() # CPI
aux[,505] %>% head()
aux[,379] %>% head()
aux[,253] %>% head()
aux[,127] %>% head()
aux[,1] %>% head()

# 126 features with 504 lagged features

y=aux[,indice] # This is our target (label) with total 487 observation
X=aux[,-c(1:(ncol(Y3)*lag))]  # ncol(Y3) = 126
# This is our feature with total 487 obs & 504 variables (including lagged)

ncol(Y3)*lag # 126 meaning aux에서 칼럼1 부터 칼럼 126까지 삭제

# 칼럼 127부터는 t-1 값의 X

# Check aux & X

Y3[,1] %>% head()
X[,1] %>% head()
aux[,127] %>% head()


# aux %>% head()
aux %>% dim()

X %>% dim() # 487 504
y %>% length() # 487

X[,1] %>% head()
c(1:(ncol(Y3)*(lag-1)))

nrow(X) # 487 : number of observation

tail(aux,1)[1:ncol(X)]

#embed(x2,3)
#tail(embed(x2,3),1)

tail(aux,1) %>% length() # 630
tail(aux,1)[1:ncol(X)] %>% length() # 504


ncol(aux)

if(lag==1){
  X.out=tail(aux,1)[1:ncol(X)]  
}else{
  X.out=aux[,-c(1:(ncol(Y3)*(lag-1)))]
  X.out=tail(X.out,1)[1:ncol(X)]
}

X.out %>% length() # 504 with is the test set

X %>% dim() # 504
X.out %>% length() # 504
X.out %>% head()

y %>% head() # 0.2918722 : t 시점부터
X[,1] %>% head() # 0.2774948 0.2918722 : t-1 시점부터
X[,127] %>% head() # 0.4224629 0.2774948 0.2918722 : t-2 시점부터
X[,253] %>% head() # 0.2341573 0.4224629 0.2774948 0.2918722 : t-3 시점부터
X[,379] %>% head() # 0.2922650 0.2341573 0.4224629 0.2774948 0.2918722 : t-4 시점부터

X[,126] %>% head()
X[,252] %>% head()
X[,378] %>% head()
X[,504] %>% head()

# ============================================================================
# Change the order of columns to match the 3D-array object

X %>% str()

X2 <- X %>% replace(!0, 0) 
X2 %>% dim()  

for(i in 0:125){
  
  # X2[,1] <- X[,1]
  # X2[,2] <- X[,127]
  # X2[,3] <- X[,253]
  # X2[,4] <- X[,379]
  # X2[,5] <- X[,2]
  # X2[,6] <- X[,128]
  
  X2[,(4*i+1)] <- X[,(i+1)]
  X2[,(4*i+2)] <- X[,(i+127)]
  X2[,(4*i+3)] <- X[,(i+253)]
  X2[,(4*i+4)] <- X[,(i+379)]
  
}

X2[,1] %>% head() == X[,1] %>% head()   # TRUE
X2[,2] %>% head() == X[,127] %>% head() # TRUE
X2[,3] %>% head() == X[,253] %>% head() # TRUE
X2[,4] %>% head() == X[,379] %>% head() # TRUE

# Same as X.out which is our test set

X.out2 <- X.out %>% replace(!0, 0)

for(i in 0:125){
  
  X.out2[(4*i+1)] <- X.out[(i+1)]
  X.out2[(4*i+2)] <- X.out[(i+127)]
  X.out2[(4*i+3)] <- X.out[(i+253)]
  X.out2[(4*i+4)] <- X.out[(i+379)]
  
}

X.out2 %>% head() # t CPI, t-1 CPI, t-2 CPI, t-3 CPI, / t PCE, t-1 PCE ...
Y3 %>% tail() # Compare


# ================================================
# LSTM 3D array Conversion

X.arr = array(
  data = as.numeric(unlist(X2)),
  dim = c(nrow(X), 4, 126)) # 126 variabes with 4 lagged (time-steps)

X.out.arr = array(
  data = as.numeric(unlist(X.out2)),
  dim = c(1, 4, 126)) # 126 variables with 4 lagged (time-steps)

y %>% head()

X.arr[,,1] %>% head()
Y3[,1] %>% head() # LAG 고려 OK
X.arr[,,2] %>% head()
Y3[,2] %>% head() # LAG 고려 OK

X.arr[,,1] %>% tail()
y %>% tail(1) # 3320807
Y3[,1] %>% tail() # OK

X.out.arr[,,1] # 0.3320807 / 0.3077476 / 0.3078746 / 0.4560760 OK~

# FINAL CHECK COMPLETE

# ==================================================================
# ==================================================================
# ==================================================================
# Multivariate LSTM Model 

run_multi_lstm=function(Y,indice,lag){
  
  comp=princomp(scale(Y,scale=FALSE))
  Y2 = cbind(Y, comp$scores[,1:4]) %>% as.data.frame()
  Y3 = lapply(Y2, normalize) %>% as.data.frame() %>% as.matrix()
  aux=embed(Y3,4+lag)
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y3)*lag))]  
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]  
  }else{
    X.out=aux[,-c(1:(ncol(Y3)*(lag-1)))]
    X.out=tail(X.out,1)[1:ncol(X)]
  }

  ###
  X2 <- X %>% replace(!0, 0) 
  
  for(i in 0:125){
    X2[,(4*i+1)] <- X[,(i+1)]
    X2[,(4*i+2)] <- X[,(i+127)]
    X2[,(4*i+3)] <- X[,(i+253)]
    X2[,(4*i+4)] <- X[,(i+379)]
  }

  X.out2 <- X.out %>% replace(!0, 0)
  
  for(i in 0:125){
    X.out2[(4*i+1)] <- X.out[(i+1)]
    X.out2[(4*i+2)] <- X.out[(i+127)]
    X.out2[(4*i+3)] <- X.out[(i+253)]
    X.out2[(4*i+4)] <- X.out[(i+379)]
  }

  ###  
  X.arr = array(
    data = as.numeric(unlist(X2)),
    dim = c(nrow(X), 4, 126))
  
  X.out.arr = array(
    data = as.numeric(unlist(X.out2)),
    dim = c(1, 4, 126))
  
  # =============================================================
  # Hyper-Parameters Adjustment

  batch_size = 25  # 25 또는 32 한 번에 입력하는 데이터 크기 
  feature = 126  # 설명변수 수 
  epochs = 100  # 학습 횟수, 100
 
  model = keras_model_sequential()
  
  # 1-layer model 실행
  
  model %>% layer_lstm(units = 32, 
                       input_shape = c(4, feature),
                       stateful = FALSE) %>%
    layer_dense(units = 1) 

    
  # 2-layer model with drop out (rate = 0.3)  아래 부분 실행 
  
  # model %>% layer_lstm(units = 32,
  #                      input_shape = c(4, feature),
  #                      stateful = FALSE,
  #                      return_sequences = TRUE) %>% 
  #   layer_dropout(rate = 0.3) %>% 
  #   layer_lstm(units = 16) %>% 
  #   layer_dropout(rate = 0.3) %>% 
  #   layer_dense(units = 1)
  
  model %>% compile(loss = 'mse', optimizer = 'adam')
  
  model %>% summary()
  
  history = model %>% fit(X.arr, y, epochs = epochs,
                          batch_size = batch_size, shuffle = FALSE, verbose = 2)
  
  # =============================================================
  
  pred = model(X.out.arr) %>% denormalize(min(Y2[,indice]),max(Y2[,indice])) # De-normalization
  
  return(list("model"=model,"pred"=pred))
}


lstm = run_multi_lstm(Y,1,1)   # 확인용 
# ============================================================================




# ============================================================================
# Multivariate Rolling 1 Step ahead LSTM Forecast

mul.lstm.rolling.window=function(Y,nprev,indice=1,lag=1){
  
  save.pred=matrix(NA,nprev,1)
  
  for(i in nprev:1){
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),] %>% as.data.frame()
    lstm=run_multi_lstm(Y.window,indice,lag)
    save.pred[(1+nprev-i),]=as.numeric(lstm$pred) # Note as.numeric()
    cat("iteration",(1+nprev-i),"\n")
  }
  
  real=Y[,indice]
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red")
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2))
  mae=mean(abs(tail(real,nprev)-save.pred))
  errors=c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=save.pred,"errors"=errors))
}


multi_lstm_rolling_ex <- mul.lstm.rolling.window(Y,nprev,1,1)  # forecasting horizon 설정 

multi_lstm_rolling_ex$errors 

# 0.001883298 RMSE
# 0.001514088 MAE

# ============================================================================
# Graphing Test Sets

date_test = seq(as.Date("1990-01-01"),as.Date("2000-12-01"), "months")

real_value = Y[,indice] %>% tail(nprev)

pred_value = multi_lstm_rolling_ex$pred

eval = data.frame(date_test, real_value, pred_value) %>% 
  set_names(c("Date","Actual","Predicted")) %>% 
  reshape2::melt(id="Date")

ggplot(data = eval, aes(x=Date, y=value, colour=variable, group=variable)) +
  geom_line(size=0.5) +
  xlab("") + ylab("") + labs(color = "") +
  scale_x_date(date_breaks ="2 year", date_labels = "%Y-%m") + 
  ggtitle("Multivariate LSTM : Actual vs Predicted") 
