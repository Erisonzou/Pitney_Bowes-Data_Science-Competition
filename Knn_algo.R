data = read.csv(file.choose())
data = data[,c("charge_cycle_time_below_12","discharging_rate_lag3","avg_volt_change_charging","avg_volt_change_discharging","max_voltage_day","charging_rate_lag3","avg_time_charging","avg_time_discharging","chargecycles","cycle_time","dischargecycles","avg_time_discharging_lag2","fail_7")]
data = data[complete.cases(data), ]
data[data$fail_7==0,]$fail_7<-"False"
data[data$fail_7==1,]$fail_7<-"True"
data$fail_7=as.factor(data$fail_7)

library(class)


Rows=sample(length(data$fail_7),10000)
TEST=data[Rows,]
TRAIN=data[-Rows,]

str(TRAIN)

Train_x = TRAIN[,-13]
head(Train_x)

Train_y = TRAIN[,13]
Test_x = TEST[,-13]
Test_y = TEST[,13]

knn.pred=knn(Train_x,Test_x,Train_y,k=13)
tabl = table(knn.pred,Test_y)
tabl

(tabl[1,1]+tabl[2,2])/sum(tabl)

kcvSearch =
 function (X, Y, split=100, d=25, m=10) 
 {
  if(class(Y)!= "factor") stop('Y is not factor type')
  if(class(X)!= "data.frame") stop('X is not data.frame type')
  rows = nrow(X)
  Hold = rep(NA,d)
  
  for(k in 1:d) {
  Store = rep(NA, m)
  
 for(j in 1:m) {
  Shuffle=sample(rows,rows)
  InSample=Shuffle[1:split]
  OutSample=Shuffle[(split+1):rows]
  TrainX = X[InSample,]
  TrainY = Y[InSample]
  TestX = X[OutSample,]
  TestY = Y[OutSample]
  knn.pred = knn(TrainX, TestX, TrainY, k)
  table.out = table(knn.pred,TestY)
  Store[j] = (table.out[1,1]+table.out[2,2])/sum(table.out)
  }
  Hold[k] = mean(Store)
  }
  return(Hold)
 }

k_graph = kcvSearch(Train_x,Train_y,1200,20,30)