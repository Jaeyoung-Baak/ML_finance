setwd("/Users/jaeyoungbaak/Desktop/skku/2022-2/finance/main/main")    #주의 D:\2Teaching\BOK이 아니라 D:/2Teaching/BOK
dir()


#install.packages("glmnet")
#library(glmnet)

#install.packages("remotes")
#remotes::install_github("gabrielrvsc/HDeconometrics")
library(HDeconometrics)

####

source("functions/func-lasso.R")

load("rawdata.rda")
Y=dados
#1960년 2월부터 2000년 12월까지 데이터

nprev=132  
#number of prediction, 1990년 1월부터 2000년 12월까지 132개월 예측

alpha=1
#The elastic net mixing parameter, 0<=alpha<=1. 
#glmnet package는 다음과 같이 설정. alpha=1 is the lasso penalty and alpha=0 is the ridge penalty


#########################
## lasso.rolling.window 설명 
#########################

# lasso1c=lasso.rolling.window(Y,nprev,1,1,alpha,type="lasso")  설명 

# lasso.rolling.window=function(Y,nprev,indice=1,lag=1,alpha=1,type="lasso") 

# indice=1은 CPI, indice=2는 PCE를 나타냄
# lag=1 means that forecasting horizon is one. 즉 1-step ahead forecasting 

#########################
## lag=1인 경우 설명  
#########################
indice=1
lag=1
type="lasso"


#lasso.rolling.window=function(Y,nprev,indice=1,lag=1,alpha=1,type="lasso"){
  
  save.coef=matrix(NA,nprev,21+ncol(Y[,-1])*4)
  # 132+505 (21+121*4=505)  (dim(X)=126(과거값 122개+factor 4개)*4 + 1(intercept)=504+1)
  save.pred=matrix(NA,nprev,1)

  i = nprev  #아래 for loop를 막고, i=nprev를 try해 본 후 loop를 돌려볼 것   
  
  for(i in nprev:1){
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    # i=nprev일 때, window는 1989년 12월까지
    lasso=runlasso(Y.window,indice,lag,alpha,type)
    # runlasso 아래 설명 참조 
    save.coef[(1+nprev-i),]=lasso$model$coef
    save.pred[(1+nprev-i),]=lasso$pred
    cat("iteration",(1+nprev-i),"\n")
  }
  
  real=Y[,indice]
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red")
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2))
  mae=mean(abs(tail(real,nprev)-save.pred))
  errors=c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors))
#}


#########################
#설명 runlasso=function(Y,indice,lag,alpha=1,type="lasso")
#########################
  
  
Y = Y.window  
#  runlasso=function(Y,indice,lag,alpha=1,type="lasso"){
    comp=princomp(scale(Y,scale=FALSE))
  
?scale
?princomp  
Z=scale(Y,scale=FALSE) #centering is done, but scaling is not  
cbind(mean(Y[,1]),mean(Y[,2]),mean(Z[,1]),mean(Z[,2]))
cbind(sd(Y[,1]),sd(Y[,2]),sd(Z[,1]),sd(Z[,2]))

Z2=scale(Y,center=TRUE,scale=FALSE)  # scale(Y,scale=FALSE)와 동일함 
aa=Z-Z2
head(aa)
tail(aa)

    Y2=cbind(Y,comp$scores[,1:4])
    #PCA를 통해 추출한 factor 포함
    dim(Y2)
    aux=embed(Y2,4+lag)
    dim(aux)
    #aux = [Y2_t, Y2_(t-1), Y2_(t-2), Y2_(t-3), Y2_(t-4)]
    y=aux[,indice]
    X=aux[,-c(1:(ncol(Y2)*lag))]  
    dim(X)
    #X = [Y2_(t-1), Y2_(t-2), Y2_(t-3), Y2_(t-4)]

    if(lag==1){
      X.out=tail(aux,1)[1:ncol(X)]  
    }else{
      X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))]
      X.out=tail(X.out,1)[1:ncol(X)]
    }
    
    
    model=ic.glmnet(X,y,alpha = alpha)
  
?ic.glmnet
?glmnet  
# see alpha, alpha=1은 lasso, alpha=0은 ridge, 0<alpha<는 elastic net  
    
    coef=model$coef
    coef[-1]
    #lambda =model$lambda
    #nvar = model$nvar
    if(type=="adalasso"){
      penalty=(abs(coef[-1])+1/sqrt(length(y)))^(-1)
      #논문 8페이지 23줄, weight
      model=ic.glmnet(X,y,penalty.factor = penalty,alpha=alpha)
    }
    
    # penalty에서 tau값을 위와 같이 1로 설정하는 대신 낮은 bic를 주는 tau를 찾는 부분 (무시해도됨)
    if(type=="fal"){
      taus=c(seq(0.1,1,0.1),1.25,1.5,2,3,4,5,7,10)
      alphas=seq(0,1,0.1)
      bb=Inf
      for(alpha in alphas){
        m0=ic.glmnet(X,y,alpha = alpha)
        coef=m0$coef
        for(tau in taus){
          penalty=(abs(coef[-1])+1/sqrt(length(y)))^(-tau)
          m=ic.glmnet(X,y,penalty.factor = penalty)
          crit=m$bic
          if(crit<bb){
            model=m
            bb=crit
          }
        }
      }
    }
    pred=predict(model,X.out)
    
    return(list("model"=model,"pred"=pred))
#  }  


#########################


lasso1c=lasso.rolling.window(Y,nprev,1,1,alpha,type="lasso")
lasso1p=lasso.rolling.window(Y,nprev,2,1,alpha,type="lasso")
lasso2c=lasso.rolling.window(Y,nprev,1,2,alpha,type="lasso")
lasso2p=lasso.rolling.window(Y,nprev,2,2,alpha,type="lasso")
lasso3c=lasso.rolling.window(Y,nprev,1,3,alpha,type="lasso")
lasso3p=lasso.rolling.window(Y,nprev,2,3,alpha,type="lasso")
lasso4c=lasso.rolling.window(Y,nprev,1,4,alpha,type="lasso")
lasso4p=lasso.rolling.window(Y,nprev,2,4,alpha,type="lasso")
lasso5c=lasso.rolling.window(Y,nprev,1,5,alpha,type="lasso")
lasso5p=lasso.rolling.window(Y,nprev,2,5,alpha,type="lasso")
lasso6c=lasso.rolling.window(Y,nprev,1,6,alpha,type="lasso")
lasso6p=lasso.rolling.window(Y,nprev,2,6,alpha,type="lasso")
lasso7c=lasso.rolling.window(Y,nprev,1,7,alpha,type="lasso")
lasso7p=lasso.rolling.window(Y,nprev,2,7,alpha,type="lasso")
lasso8c=lasso.rolling.window(Y,nprev,1,8,alpha,type="lasso")
lasso8p=lasso.rolling.window(Y,nprev,2,8,alpha,type="lasso")
lasso9c=lasso.rolling.window(Y,nprev,1,9,alpha,type="lasso")
lasso9p=lasso.rolling.window(Y,nprev,2,9,alpha,type="lasso")
lasso10c=lasso.rolling.window(Y,nprev,1,10,alpha,type="lasso")
lasso10p=lasso.rolling.window(Y,nprev,2,10,alpha,type="lasso")
lasso11c=lasso.rolling.window(Y,nprev,1,11,alpha,type="lasso")
lasso11p=lasso.rolling.window(Y,nprev,2,11,alpha,type="lasso")
lasso12c=lasso.rolling.window(Y,nprev,1,12,alpha,type="lasso")
lasso12p=lasso.rolling.window(Y,nprev,2,12,alpha,type="lasso")

# pols #

# OLS with selected variables by lasso
# lasso 결과 계수가 0이 아닌 변수들만으로 OLS 추정 및 예측,  

pols.lasso1c=pols.rolling.window(Y,nprev,1,1,lasso1c$coef)
pols.lasso1p=pols.rolling.window(Y,nprev,2,1,lasso1p$coef)
pols.lasso2c=pols.rolling.window(Y,nprev,1,2,lasso2c$coef)
pols.lasso2p=pols.rolling.window(Y,nprev,2,2,lasso2p$coef)
pols.lasso3c=pols.rolling.window(Y,nprev,1,3,lasso3c$coef)
pols.lasso3p=pols.rolling.window(Y,nprev,2,3,lasso3p$coef)
pols.lasso4c=pols.rolling.window(Y,nprev,1,4,lasso4c$coef)
pols.lasso4p=pols.rolling.window(Y,nprev,2,4,lasso4p$coef)
pols.lasso5c=pols.rolling.window(Y,nprev,1,5,lasso5c$coef)
pols.lasso5p=pols.rolling.window(Y,nprev,2,5,lasso5p$coef)
pols.lasso6c=pols.rolling.window(Y,nprev,1,6,lasso6c$coef)
pols.lasso6p=pols.rolling.window(Y,nprev,2,6,lasso6p$coef)
pols.lasso7c=pols.rolling.window(Y,nprev,1,7,lasso7c$coef)
pols.lasso7p=pols.rolling.window(Y,nprev,2,7,lasso7p$coef)
pols.lasso8c=pols.rolling.window(Y,nprev,1,8,lasso8c$coef)
pols.lasso8p=pols.rolling.window(Y,nprev,2,8,lasso8p$coef)
pols.lasso9c=pols.rolling.window(Y,nprev,1,9,lasso9c$coef)
pols.lasso9p=pols.rolling.window(Y,nprev,2,9,lasso9p$coef)
pols.lasso10c=pols.rolling.window(Y,nprev,1,10,lasso10c$coef)
pols.lasso10p=pols.rolling.window(Y,nprev,2,10,lasso10p$coef)
pols.lasso11c=pols.rolling.window(Y,nprev,1,11,lasso11c$coef)
pols.lasso11p=pols.rolling.window(Y,nprev,2,11,lasso11p$coef)
pols.lasso12c=pols.rolling.window(Y,nprev,1,12,lasso12c$coef)
pols.lasso12p=pols.rolling.window(Y,nprev,2,12,lasso12p$coef)


### == juntando tudo ==  ###

cpi=cbind(lasso1c$pred,lasso2c$pred,lasso3c$pred,lasso4c$pred,
          lasso5c$pred,lasso6c$pred,lasso7c$pred,lasso8c$pred,
          lasso9c$pred,lasso10c$pred,lasso11c$pred,lasso12c$pred)

pce=cbind(lasso1p$pred,lasso2p$pred,lasso3p$pred,lasso4p$pred,
          lasso5p$pred,lasso6p$pred,lasso7p$pred,lasso8p$pred,
          lasso9p$pred,lasso10p$pred,lasso11p$pred,lasso12p$pred)


pols.cpi=cbind(pols.lasso1c$pred,pols.lasso2c$pred,pols.lasso3c$pred,pols.lasso4c$pred,
               pols.lasso5c$pred,pols.lasso6c$pred,pols.lasso7c$pred,pols.lasso8c$pred,
               pols.lasso9c$pred,pols.lasso10c$pred,pols.lasso11c$pred,pols.lasso12c$pred)

pols.pce=cbind(pols.lasso1p$pred,pols.lasso2p$pred,pols.lasso3p$pred,pols.lasso4p$pred,
               pols.lasso5p$pred,pols.lasso6p$pred,pols.lasso7p$pred,pols.lasso8p$pred,
               pols.lasso9p$pred,pols.lasso10p$pred,pols.lasso11p$pred,pols.lasso12p$pred)



##
write.table(cpi,"forecasts/passado2000/lasso-cpi.csv",sep=";",row.names = FALSE, col.names = FALSE)
write.table(pce,"forecasts/passado2000/lasso-pce.csv",sep=";",row.names = FALSE, col.names = FALSE)

write.table(pols.cpi,"forecasts/passado2000/pols-lasso-cpi.csv",sep=";",row.names = FALSE, col.names = FALSE)
write.table(pols.pce,"forecasts/passado2000/pols-lasso-pce.csv",sep=";",row.names = FALSE, col.names = FALSE)
