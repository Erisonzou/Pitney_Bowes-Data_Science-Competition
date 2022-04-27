# randomForest library is necessary to use the Random forest algorithm
library(randomForest)

#imput the data
data = read.csv(file.choose())

# removing rows with NA
data = data[complete.cases(data), ]

#structure of the data
str(data)

#changing the target variable which is fail_7 into False and True in order to make the algorithm Classification instead of regression
data[data$fail_7==0,]$fail_7<-"False"
data[data$fail_7==1,]$fail_7<-"True"
data$fail_7=as.factor(data$fail_7)

#creating Testing and Training data
Rows=sample(length(data$fail_7),10000)
TEST=data[Rows,]
TRAIN=data[-Rows,]


set.seed(some number)

#creating the randomForest Model with fail_7 as the variable we want to predict for 
Model = randomForest(fail_7~.,data=TRAIN,mtry=3,importance=TRUE)

# call Model to check the information about the Model
Model

# importance method to find the most important feature when predicting fail_7
importance(Model)

#plotting the importance of the features to understand which features are the most important
varImpPlot(Model)

#creating a importance data frame to grab the top 20 most important features 
importance_df <- as.data.frame(importance(Model))
importance_df <- importance_df[order(importance_df$MeanDecreaseAccuracy, decreasing = TRUE), ]
head(importance_df,20)

# new_df contains the most important features 
new_df = data[,c("charge_cycle_time_below_12","discharging_rate_lag3","avg_volt_change_charging","avg_volt_change_discharging","max_voltage_day","charging_rate_lag3","avg_time_charging","avg_time_discharging","chargecycles","cycle_time","dischargecycles","avg_time_discharging_lag2")]

#creating a new randomForest Model based on the new_df
new_Model = randomForest(fail_7~.,data=new_df,mtry=3,importance=TRUE)

new_Model

#create a dataframe from the Model error rate
Model_err.rate_df=as.data.frame(new_Model$err.rate)
Model_err.rate_df$Tree=seq(1,500)
colnames(Model_err.rate_df)<-c("OOB","nofail","fail","Trees")

#plot the Model error rate in order to determine the number of trees needed for the highest effiency 
plot(Model_err.rate_df$Trees,Model_err.rate_df$nofail,ylim=c(0,1),type="l",col="green")
lines(Model_err.rate_df$Trees,Model_err.rate_df$fail,col="red")
lines(Model_err.rate_df$Trees,Model_err.rate_df$OOB)


#finding the optimal number of variables 

#importing the necessary libraries
library(ggplot2)
library(mlbench)
library(caret)

#setting up the default model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
mtry <- sqrt(ncol(new_df))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(fail_7~., data=new_df, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
rf_default

#setting up the random model 
control = trainControl(method="repeatedcv", number=10, repeats=3, search="random")
rf_random = train(fail_7~., data=new_df, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
rf_random
plot(rf_random)


