setwd("/Users/jaeyoungbaak/Desktop/skku/2022-2/finance/main/main")
dir()

#install.packages("devtools")  #github의 package들을 인스톨할 수 있게 함 

# Installing 'fbi' Package from Github
library(devtools)  
#install_github("cykbennie/fbi")

library(fbi)

data("fredmd_description")


## Transformation하지 않은 FRED-MD loading
data = fredmd(file = "2016-01.csv", date_start = NULL, date_end = NULL, transform = FALSE)


#fredmd(file = "2016-01.csv", date_start = NULL, date_end = NULL, transform = TRUE)
#tdata = fredmd(file = "2016-01.csv", date_start = NULL, date_end = NULL, transform = True)  # 문제가 있음, 따라서 아래 transformation을 직접 코딩 

?fredmd
#rm_outliers.fredmd()



### Transformation 

tcode = fredmd_description[,2]

tcode

tdata = data[-(1:2),]   #log후 2차 차분도 있기 때문에 맞추려고 

for (i in 2:136){
  
  if(tcode[i-1] == 1){
    tdata[,i] <- data[-(1:2),i]
  } # no transformation  

  if(tcode[i-1] == 2){
    tdata[,i] <- diff(data[-1,i])
  } # 1차 차분
  
  # tdoce ==3(2차 차분)에 해당하는 데이터는 없음

  if(tcode[i-1] == 4){
    tdata[,i] <- log(data[-(1:2),i])
  } # log

  if(tcode[i-1] == 5){
    tdata[,i] <- diff(log(data[-1,i]))
  } # log differencing

  if(tcode[i-1] == 6){
    tdata[,i] <- diff(diff(log(data[,i])))
  } # log취한 뒤 2차 차분

  if(tcode[i-1] == 7){
    tdata[,i] <- diff(data[-1,i]/data[1:(nrow(data)-1),i])
  } # 증가율의 1차 차분
}



# 저자들의 데이터와 확인, CPI와 PCE 이후는 동일, 저자들은 NA가 있는 변수들은 생략   
load("rawdata.rda")
Y=dados
