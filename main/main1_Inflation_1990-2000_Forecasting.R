###############################################################################
### Forecasting Inflation in a Data Rich Environment (Medeiros et al, 2019) ###
###############################################################################

### Codes for Replicating TABLE S.12 - Forecasting Errors for CPI (1990-2000), 

####################
### Forecasting Part
#################### 


setwd("/Users/jaeyoungbaak/Desktop/skku/2022-2/finance/HW3/main")
dir()

#install.packages("devtools")  #github의 package들을 인스톨할 수 있게 함 
#install.packages("randomForest")

# Installing 'HDeconometrics' Package from Github
#library(devtools)  
#install_github("gabrielrvsc/HDeconometrics")
#install_github("gabrielrvsc/HDeconometricsbeta")
library(HDeconometrics)


# Loading Data
load("rawdata.rda")
Y = dados


# Number of Forecasts, prediction
npred=132
#주의: nprev 대신 npred를 사용하고 있음 


## Random Walk Model ##
source("functions/func-rw.R")

rw1=rw.rolling.window(Y,npred,1,1)
rw2=rw.rolling.window(Y,npred,1,2)
rw3=rw.rolling.window(Y,npred,1,3)
rw4=rw.rolling.window(Y,npred,1,4)
rw5=rw.rolling.window(Y,npred,1,5)
rw6=rw.rolling.window(Y,npred,1,6)
rw7=rw.rolling.window(Y,npred,1,7)
rw8=rw.rolling.window(Y,npred,1,8)
rw9=rw.rolling.window(Y,npred,1,9)
rw10=rw.rolling.window(Y,npred,1,10)
rw11=rw.rolling.window(Y,npred,1,11)
rw12=rw.rolling.window(Y,npred,1,12)


rw_pred = cbind(rw1$pred,rw2$pred,rw3$pred,rw4$pred,rw5$pred,rw6$pred,
                rw7$pred,rw8$pred,rw9$pred,rw10$pred,rw11$pred,rw12$pred)


rw = cbind(rw1$errors[1],rw2$errors[1],rw3$errors[1],rw4$errors[1],rw5$errors[1],rw6$errors[1],
           rw7$errors[1],rw8$errors[1],rw9$errors[1],rw10$errors[1],rw11$errors[1],rw12$errors[1])

# erros[1]: RMSE, errors[2]: MAE


## AR(4) Model ##
source("functions/func-ar.R")

ar1=ar.rolling.window(Y,npred,1,1,type="fixed")
ar2=ar.rolling.window(Y,npred,1,2,type="fixed")
ar3=ar.rolling.window(Y,npred,1,3,type="fixed")
ar4=ar.rolling.window(Y,npred,1,4,type="fixed")
ar5=ar.rolling.window(Y,npred,1,5,type="fixed")
ar6=ar.rolling.window(Y,npred,1,6,type="fixed")
ar7=ar.rolling.window(Y,npred,1,7,type="fixed")
ar8=ar.rolling.window(Y,npred,1,8,type="fixed")
ar9=ar.rolling.window(Y,npred,1,9,type="fixed")
ar10=ar.rolling.window(Y,npred,1,10,type="fixed")
ar11=ar.rolling.window(Y,npred,1,11,type="fixed")
ar12=ar.rolling.window(Y,npred,1,12,type="fixed")


ar_pred = cbind(ar1$pred,ar2$pred,ar3$pred,ar4$pred,ar5$pred,ar6$pred,
                ar7$pred,ar8$pred,ar9$pred,ar10$pred,ar11$pred,ar12$pred)


ar = cbind(ar1$errors[1],ar2$errors[1],ar3$errors[1],ar4$errors[1],ar5$errors[1],ar6$errors[1],
           ar7$errors[1],ar8$errors[1],ar9$errors[1],ar10$errors[1],ar11$errors[1],ar12$errors[1])


## LASSO ##
source("functions/func-lasso.R")
alpha=1

lasso1=lasso.rolling.window(Y,npred,1,1,alpha,type="lasso")
lasso2=lasso.rolling.window(Y,npred,1,2,alpha,type="lasso")
lasso3=lasso.rolling.window(Y,npred,1,3,alpha,type="lasso")
lasso4=lasso.rolling.window(Y,npred,1,4,alpha,type="lasso")
lasso5=lasso.rolling.window(Y,npred,1,5,alpha,type="lasso")
lasso6=lasso.rolling.window(Y,npred,1,6,alpha,type="lasso")
lasso7=lasso.rolling.window(Y,npred,1,7,alpha,type="lasso")
lasso8=lasso.rolling.window(Y,npred,1,8,alpha,type="lasso")
lasso9=lasso.rolling.window(Y,npred,1,9,alpha,type="lasso")
lasso10=lasso.rolling.window(Y,npred,1,10,alpha,type="lasso")
lasso11=lasso.rolling.window(Y,npred,1,11,alpha,type="lasso")
lasso12=lasso.rolling.window(Y,npred,1,12,alpha,type="lasso")


lasso_pred = cbind(lasso1$pred,lasso2$pred,lasso3$pred,lasso4$pred,lasso5$pred,lasso6$pred,
                   lasso7$pred,lasso8$pred,lasso9$pred,lasso10$pred,lasso11$pred,lasso12$pred)


lasso = cbind(lasso1$errors[1],lasso2$errors[1],lasso3$errors[1],lasso4$errors[1],lasso5$errors[1],lasso6$errors[1],lasso7$errors[1],lasso8$errors[1],lasso9$errors[1],lasso10$errors[1],lasso11$errors[1],lasso12$errors[1])



## Adaptive LASSO ##

adalasso1=lasso.rolling.window(Y,npred,1,1,alpha,type="adalasso")
adalasso2=lasso.rolling.window(Y,npred,1,2,alpha,type="adalasso")
adalasso3=lasso.rolling.window(Y,npred,1,3,alpha,type="adalasso")
adalasso4=lasso.rolling.window(Y,npred,1,4,alpha,type="adalasso")
adalasso5=lasso.rolling.window(Y,npred,1,5,alpha,type="adalasso")
adalasso6=lasso.rolling.window(Y,npred,1,6,alpha,type="adalasso")
adalasso7=lasso.rolling.window(Y,npred,1,7,alpha,type="adalasso")
adalasso8=lasso.rolling.window(Y,npred,1,8,alpha,type="adalasso")
adalasso9=lasso.rolling.window(Y,npred,1,9,alpha,type="adalasso")
adalasso10=lasso.rolling.window(Y,npred,1,10,alpha,type="adalasso")
adalasso11=lasso.rolling.window(Y,npred,1,11,alpha,type="adalasso")
adalasso12=lasso.rolling.window(Y,npred,1,12,alpha,type="adalasso")


adalasso_pred = cbind(adalasso1$pred,adalasso2$pred,adalasso3$pred,adalasso4$pred,
                      adalasso5$pred,adalasso6$pred,adalasso7$pred,adalasso8$pred,
                      adalasso9$pred,adalasso10$pred,adalasso11$pred,adalasso12$pred)

adalasso = cbind(adalasso1$errors[1],adalasso2$errors[1],adalasso3$errors[1],adalasso4$errors[1],
                 adalasso5$errors[1],adalasso6$errors[1],adalasso7$errors[1],adalasso8$errors[1],
                 adalasso9$errors[1],adalasso10$errors[1],adalasso11$errors[1],adalasso12$errors[1])



## Elastic Net ##

alpha=0.5

elasticnet1=lasso.rolling.window(Y,npred,1,1,alpha,type="lasso")
elasticnet2=lasso.rolling.window(Y,npred,1,2,alpha,type="lasso")
elasticnet3=lasso.rolling.window(Y,npred,1,3,alpha,type="lasso")
elasticnet4=lasso.rolling.window(Y,npred,1,4,alpha,type="lasso")
elasticnet5=lasso.rolling.window(Y,npred,1,5,alpha,type="lasso")
elasticnet6=lasso.rolling.window(Y,npred,1,6,alpha,type="lasso")
elasticnet7=lasso.rolling.window(Y,npred,1,7,alpha,type="lasso")
elasticnet8=lasso.rolling.window(Y,npred,1,8,alpha,type="lasso")
elasticnet9=lasso.rolling.window(Y,npred,1,9,alpha,type="lasso")
elasticnet10=lasso.rolling.window(Y,npred,1,10,alpha,type="lasso")
elasticnet11=lasso.rolling.window(Y,npred,1,11,alpha,type="lasso")
elasticnet12=lasso.rolling.window(Y,npred,1,12,alpha,type="lasso")


elasticnet_pred = cbind(elasticnet1$pred,elasticnet2$pred,elasticnet3$pred,elasticnet4$pred,
                        elasticnet5$pred,elasticnet6$pred,elasticnet7$pred,elasticnet8$pred,
                        elasticnet9$pred,elasticnet10$pred,elasticnet11$pred,elasticnet12$pred)



elasticnet = cbind(elasticnet1$errors[1],elasticnet2$errors[1],elasticnet3$errors[1],elasticnet4$errors[1],
                   elasticnet5$errors[1],elasticnet6$errors[1],elasticnet7$errors[1],elasticnet8$errors[1],
                   elasticnet9$errors[1],elasticnet10$errors[1],elasticnet11$errors[1],elasticnet12$errors[1])



## Adaptive Elastic Net ##

adaelasticnet1=lasso.rolling.window(Y,npred,1,1,alpha,type="adalasso")
adaelasticnet2=lasso.rolling.window(Y,npred,1,2,alpha,type="adalasso")
adaelasticnet3=lasso.rolling.window(Y,npred,1,3,alpha,type="adalasso")
adaelasticnet4=lasso.rolling.window(Y,npred,1,4,alpha,type="adalasso")
adaelasticnet5=lasso.rolling.window(Y,npred,1,5,alpha,type="adalasso")
adaelasticnet6=lasso.rolling.window(Y,npred,1,6,alpha,type="adalasso")
adaelasticnet7=lasso.rolling.window(Y,npred,1,7,alpha,type="adalasso")
adaelasticnet8=lasso.rolling.window(Y,npred,1,8,alpha,type="adalasso")
adaelasticnet9=lasso.rolling.window(Y,npred,1,9,alpha,type="adalasso")
adaelasticnet10=lasso.rolling.window(Y,npred,1,10,alpha,type="adalasso")
adaelasticnet11=lasso.rolling.window(Y,npred,1,11,alpha,type="adalasso")
adaelasticnet12=lasso.rolling.window(Y,npred,1,12,alpha,type="adalasso")


adaelasticnet_pred = cbind(adaelasticnet1$pred,adaelasticnet2$pred,adaelasticnet3$pred,adaelasticnet4$pred,
                           adaelasticnet5$pred,adaelasticnet6$pred,adaelasticnet7$pred,adaelasticnet8$pred,
                           adaelasticnet9$pred,adaelasticnet10$pred,adaelasticnet11$pred,adaelasticnet12$pred)

adaelasticnet = cbind(adaelasticnet1$errors[1],adaelasticnet2$errors[1],adaelasticnet3$errors[1],adaelasticnet4$errors[1],
                      adaelasticnet5$errors[1],adaelasticnet6$errors[1],adaelasticnet7$errors[1],adaelasticnet8$errors[1],
                      adaelasticnet9$errors[1],adaelasticnet10$errors[1],adaelasticnet11$errors[1],adaelasticnet12$errors[1])



## Ridge Regression ##

alpha=0

ridge1=lasso.rolling.window(Y,npred,1,1,alpha,type="lasso")
ridge2=lasso.rolling.window(Y,npred,1,2,alpha,type="lasso")
ridge3=lasso.rolling.window(Y,npred,1,3,alpha,type="lasso")
ridge4=lasso.rolling.window(Y,npred,1,4,alpha,type="lasso")
ridge5=lasso.rolling.window(Y,npred,1,5,alpha,type="lasso")
ridge6=lasso.rolling.window(Y,npred,1,6,alpha,type="lasso")
ridge7=lasso.rolling.window(Y,npred,1,7,alpha,type="lasso")
ridge8=lasso.rolling.window(Y,npred,1,8,alpha,type="lasso")
ridge9=lasso.rolling.window(Y,npred,1,9,alpha,type="lasso")
ridge10=lasso.rolling.window(Y,npred,1,10,alpha,type="lasso")
ridge11=lasso.rolling.window(Y,npred,1,11,alpha,type="lasso")
ridge12=lasso.rolling.window(Y,npred,1,12,alpha,type="lasso")



ridge_pred = cbind(ridge1$pred,ridge2$pred,ridge3$pred,ridge4$pred,ridge5$pred,ridge6$pred,
                   ridge7$pred,ridge8$pred,ridge9$pred,ridge10$pred,ridge11$pred,ridge12$pred)

ridge = cbind(ridge1$errors[1],ridge2$errors[1],ridge3$errors[1],ridge4$errors[1],ridge5$errors[1],ridge6$errors[1],
              ridge7$errors[1],ridge8$errors[1],ridge9$errors[1],ridge10$errors[1],ridge11$errors[1],ridge12$errors[1])



## Complete Subset Regression (CSR) ##
source("functions/func-csr.R")

csr1=csr.rolling.window(Y,npred,1,1)
csr2=csr.rolling.window(Y,npred,1,2)
csr3=csr.rolling.window(Y,npred,1,3)
csr4=csr.rolling.window(Y,npred,1,4)
csr5=csr.rolling.window(Y,npred,1,5)
csr6=csr.rolling.window(Y,npred,1,6)
csr7=csr.rolling.window(Y,npred,1,7)
csr8=csr.rolling.window(Y,npred,1,8)
csr9=csr.rolling.window(Y,npred,1,9)
csr10=csr.rolling.window(Y,npred,1,10)
csr11=csr.rolling.window(Y,npred,1,11)
csr12=csr.rolling.window(Y,npred,1,12)


csr_pred = cbind(csr1$pred,csr2$pred,csr3$pred,csr4$pred,csr5$pred,csr6$pred,
                 csr7$pred,csr8$pred,csr9$pred,csr10$pred,csr11$pred,csr12$pred)


csr = cbind(csr1$errors[1],csr2$errors[1],csr3$errors[1],csr4$errors[1],csr5$errors[1],csr6$errors[1],
            csr7$errors[1],csr8$errors[1],csr9$errors[1],csr10$errors[1],csr11$errors[1],csr12$errors[1])



## Target Factors ##
source("functions/func-fact.R")
source("functions/func-tfact.R")
source("functions/func-baggit.R")

tfact1=tfact.rolling.window(Y,npred,1,1)
tfact2=tfact.rolling.window(Y,npred,1,2)
tfact3=tfact.rolling.window(Y,npred,1,3)
tfact4=tfact.rolling.window(Y,npred,1,4)
tfact5=tfact.rolling.window(Y,npred,1,5)
tfact6=tfact.rolling.window(Y,npred,1,6)
tfact7=tfact.rolling.window(Y,npred,1,7)
tfact8=tfact.rolling.window(Y,npred,1,8)
tfact9=tfact.rolling.window(Y,npred,1,9)
tfact10=tfact.rolling.window(Y,npred,1,10)
tfact11=tfact.rolling.window(Y,npred,1,11)
tfact12=tfact.rolling.window(Y,npred,1,12)


tfact_pred = cbind(tfact1$pred,tfact2$pred,tfact3$pred,tfact4$pred,tfact5$pred,tfact6$pred,
                   tfact7$pred,tfact8$pred,tfact9$pred,tfact10$pred,tfact11$pred,tfact12$pred)


tfact = cbind(tfact1$errors[1],tfact2$errors[1],tfact3$errors[1],tfact4$errors[1],tfact5$errors[1],tfact6$errors[1],
              tfact7$errors[1],tfact8$errors[1],tfact9$errors[1],tfact10$errors[1],tfact11$errors[1],tfact12$errors[1])



## Random Forest (RF) ##
source("functions/func-rf.R")
library(randomForest)

rf1=rf.rolling.window(Y,npred,1,1)
rf2=rf.rolling.window(Y,npred,1,2)
rf3=rf.rolling.window(Y,npred,1,3)
rf4=rf.rolling.window(Y,npred,1,4)
rf5=rf.rolling.window(Y,npred,1,5)
rf6=rf.rolling.window(Y,npred,1,6)
rf7=rf.rolling.window(Y,npred,1,7)
rf8=rf.rolling.window(Y,npred,1,8)
rf9=rf.rolling.window(Y,npred,1,9)
rf10=rf.rolling.window(Y,npred,1,10)
rf11=rf.rolling.window(Y,npred,1,11)
rf12=rf.rolling.window(Y,npred,1,12)



rf_pred = cbind(rf1$pred,rf2$pred,rf3$pred,rf4$pred,rf5$pred,rf6$pred,
                rf7$pred,rf8$pred,rf9$pred,rf10$pred,rf11$pred,rf12$pred)


rf = cbind(rf1$errors[1],rf2$errors[1],rf3$errors[1],rf4$errors[1],rf5$errors[1],rf6$errors[1],
           rf7$errors[1],rf8$errors[1],rf9$errors[1],rf10$errors[1],rf11$errors[1],rf12$errors[1])



## RF/OLS ##
source("functions/func-rfols.R")

rfols1=rfols.rolling.window(Y,npred,1,1)
rfols2=rfols.rolling.window(Y,npred,1,2)
rfols3=rfols.rolling.window(Y,npred,1,3)
rfols4=rfols.rolling.window(Y,npred,1,4)
rfols5=rfols.rolling.window(Y,npred,1,5)
rfols6=rfols.rolling.window(Y,npred,1,6)
rfols7=rfols.rolling.window(Y,npred,1,7)
rfols8=rfols.rolling.window(Y,npred,1,8)
rfols9=rfols.rolling.window(Y,npred,1,9)
rfols10=rfols.rolling.window(Y,npred,1,10)
rfols11=rfols.rolling.window(Y,npred,1,11)
rfols12=rfols.rolling.window(Y,npred,1,12)



rfols_pred = cbind(rfols1$pred,rfols2$pred,rfols3$pred,rfols4$pred,rfols5$pred,rfols6$pred,
                   rfols7$pred,rfols8$pred,rfols9$pred,rfols10$pred,rfols11$pred,rfols12$pred)

rfols = cbind(rfols1$errors[1],rfols2$errors[1],rfols3$errors[1],rfols4$errors[1],rfols5$errors[1],rfols6$errors[1],
              rfols7$errors[1],rfols8$errors[1],rfols9$errors[1],rfols10$errors[1],rfols11$errors[1],rfols12$errors[1])



## adaLASSO/RF ##
source("functions/func-adalassorf.R")

lassorf1=lasso.rolling.window(Y,npred,1,1,type="adalasso")
lassorf2=lasso.rolling.window(Y,npred,1,2,type="adalasso")
lassorf3=lasso.rolling.window(Y,npred,1,3,type="adalasso")
lassorf4=lasso.rolling.window(Y,npred,1,4,type="adalasso")
lassorf5=lasso.rolling.window(Y,npred,1,5,type="adalasso")
lassorf6=lasso.rolling.window(Y,npred,1,6,type="adalasso")
lassorf7=lasso.rolling.window(Y,npred,1,7,type="adalasso")
lassorf8=lasso.rolling.window(Y,npred,1,8,type="adalasso")
lassorf9=lasso.rolling.window(Y,npred,1,9,type="adalasso")
lassorf10=lasso.rolling.window(Y,npred,1,10,type="adalasso")
lassorf11=lasso.rolling.window(Y,npred,1,11,type="adalasso")
lassorf12=lasso.rolling.window(Y,npred,1,12,type="adalasso")


lassorf_pred = cbind(lassorf1$pred,lassorf2$pred,lassorf3$pred,lassorf4$pred,
                     lassorf5$pred,lassorf6$pred,lassorf7$pred,lassorf8$pred,
                     lassorf9$pred,lassorf10$pred,lassorf11$pred,lassorf12$pred)


lassorf = cbind(lassorf1$errors[1],lassorf2$errors[1],lassorf3$errors[1],lassorf4$errors[1],
                lassorf5$errors[1],lassorf6$errors[1],lassorf7$errors[1],lassorf8$errors[1],
                lassorf9$errors[1],lassorf10$errors[1],lassorf11$errors[1],lassorf12$errors[1])




### Errors and their Relative Ratio to RW Model ###

ERRORS = rbind(rw, ar, lasso, adalasso, elasticnet, adaelasticnet, ridge, csr, tfact, rf, rfols, lassorf)

RATIO = ERRORS / rep(ERRORS[1,], each=nrow(ERRORS))


# saving entire worksapce
# save.image("results_forecasts.RData")    
