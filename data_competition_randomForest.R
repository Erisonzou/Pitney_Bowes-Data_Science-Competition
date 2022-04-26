library(ggplot2)
library(cowplot)
library(randomForest)

data = read.csv(file.choose())
data = data[complete.cases(data), ]
str(data)

data[data$fail_7==0,]$fail_7<-"False"
data[data$fail_7==1,]$fail_7<-"TRUE"
data$fail_7=as.factor(data$fail_7)

Rows=sample(length(data$fail_7),10000)
TEST=data[Rows,]
TRAIN=data[-Rows,]


set.seed(some number)
>>imputed.data = rfImpute(fail_7~.,data=data,iter=10)

Model = randomForest(fail_7~.,data=TRAIN,subset=train,mtry=3,importance=TRUE)

Model_err.rate_df=as.data.frame(Model$err.rate)
Model_err.rate_df$Tree=seq(1,500)

plot(Model_err.rate_df$Trees,Model_err.rate_df$nofail,ylim=c(0,1),type="l",col="green")
lines(Model_err.rate_df$Trees,Model_err.rate_df$fail,col="red")
lines(Model_err.rate_df$Trees,Model_err.rate_df$OOB)

TForest = predict (RandomF, TEST)
mean((TForest-TEST$fail_7)^2)

importance(Treez)

varImpPlot(Treez)