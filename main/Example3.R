###################
# Neural Networks #
###################

setwd("D:/2Teaching/BOK/ML2021/Rcodes/main")  
dir()

# Training & Test datasets
bmd.data=read.table("Bone Mineral Density Data.txt",sep="\t",header=T)
bmd.data[1:5,]

attach(bmd.data)

sex = ifelse(gender=='male',1,0)
bmd.data$gender = as.numeric(sex)

# Training 
train=bmd.data[training,1:3]

# Test variables
test=bmd.data[!training,1:3]

detach(bmd.data)

nrow(train)
nrow(test)

# Scaling
maxs = apply(train, 2, max)
mins = apply(train, 2, min)
train.s = as.data.frame(scale(train, center=mins, scale=maxs-mins))
test.s = as.data.frame(scale(test, center=mins, scale=maxs-mins))

# Neural networks
install.packages('neuralnet')
library(neuralnet)

nn = neuralnet(spnbmd ~ age+gender,data=train.s,hidden=c(5,3),linear.output=T)
# linear.output=T: Regression; =F: Classification.
# hidden=c(5,3): 첫번째 hidden layer unit이 5, 두번째 hidden layer unit이 3
# linear.output=T: regression


# Result
plot(nn)

# Weights
nn$weights

# Activation function
nn$act.fct

?neuralnet

# Prediction
pr.nn = compute(nn,test.s[,1:2])
spnbmdhat = pr.nn$net.result*(maxs[3]-mins[3])+mins[3]
# scale된 것을 원래로 변환 

# Test error
sum((test.s$spnbmd - spnbmdhat)^2)/nrow(test.s)

# Single-hidden-layer Neural Networks-----------------


attach(bmd.data)

# Training variables
tr.age=age[training]
tr.gender=gender[training]
tr.spnbmd=spnbmd[training]

# Test variables
te.age=age[!training]
te.gender=gender[!training]
te.spnbmd=spnbmd[!training]

detach(bmd.data)

length(tr.age)
length(te.age)


install.packages('nnet')
library(nnet)
?nnet

nnet.bmd=nnet(tr.spnbmd~tr.age + tr.gender,size=10,linout=TRUE,maxit=1000,trace=FALSE)
# size=10: single hidden layer with 10 units
# if output is continuous, linout=TRUE.
# decay: regularization (weight decay)


summary(nnet.bmd)

nnet.y.hat=predict(nnet.bmd,newdata=data.frame(tr.age=te.age,tr.gender=te.gender))
nnet.y.hat[1:4]
te.spnbmd[1:4]

test.error = mean((nnet.y.hat-te.spnbmd)^2)
test.error


