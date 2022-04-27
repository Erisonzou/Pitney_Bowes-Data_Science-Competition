# import the csv file
data = read.csv(file.choose())


# grabbing the important features 
## important features were chosen through the Randomtree algorithm 

data = data[,c("charge_cycle_time_below_12","discharging_rate_lag3","avg_volt_change_charging","avg_volt_change_discharging","max_voltage_day","charging_rate_lag3","avg_time_charging","avg_time_discharging","chargecycles","cycle_time","dischargecycles","avg_time_discharging_lag2","fail_7")]
data = data[complete.cases(data), ]

# changing the fail_7 column which is the prediction into False or True in order to make it easier to classify the data
data[data$fail_7==0,]$fail_7<-"False"
data[data$fail_7==1,]$fail_7<-"True"
data$fail_7=as.factor(data$fail_7)

# class library is needed for the Knn algorithm 
library(class)

# splitting the data into Training and Testing Data
Rows=sample(length(data$fail_7),10000)
TEST=data[Rows,]
TRAIN=data[-Rows,]

# Structure of the Training data
str(TRAIN)

# The Train_X data will contain all the features that will be used to predict the fail_7 column 
Train_x = TRAIN[,-13]
head(Train_x)

# Train_y will contain the prediction value which is fail_7
Train_y = TRAIN[,13]

# Train_Y and Train_X will be necessary to Train the Algorithm 
# After the Algorithm has been Train it is going to be necessary to Test the Algorithm 
# Test_x and Test_y are going to the testing data
Test_x = TEST[,-13]
Test_y = TEST[,13]

# Knn algorithm will train itself using the Train_X and the Train_y using a k value of 13 
# After the algorithm has train itself it will predict the values in Test_y using Test_x
# knn.pred will contain the predicted values of Test_y
knn.pred=knn(Train_x,Test_x,Train_y,k=13)

# the table function will creating the Confusion Matrix for the predicted value vs the actual value
tabl = table(knn.pred,Test_y)
tabl

# this calculation will return the accuracy rate of the Confusion Matrix 
# the higher the accuracy rate the more accurate the predicted value was to the actual value 
(tabl[1,1]+tabl[2,2])/sum(tabl)

# in order to tune the Knn algorithm it is going to be necessary to understand at what K value will it work best for the Knn algorithm 
# in order to do that the Function below will check the accuracy rate at each k value 
# X are the features that is used to predict the value
# Y is the feature being predicted
# "split" will determine the size of the training data 
# d is the maximum k value
# m is the number of random splits being generated

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

k_graph = kcvSearch(Train_x,Train_y,5000,50,30)
plot(k_graph, type="l")


