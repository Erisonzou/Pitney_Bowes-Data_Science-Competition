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
Model = randomForest(fail_7~.,data=TRAIN,subset=train,mtry=3,importance=TRUE)

# call Model to check the information about the Model
Model

#create a dataframe from the Model error rate
Model_err.rate_df=as.data.frame(Model$err.rate)
Model_err.rate_df$Tree=seq(1,500)
colnames(Model_err.rate_df)<-c("OOB","nofail","fail","Trees")

#plot the Model error rate in order to determine the number of trees needed for the highest effiency 
plot(Model_err.rate_df$Trees,Model_err.rate_df$nofail,ylim=c(0,1),type="l",col="green")
lines(Model_err.rate_df$Trees,Model_err.rate_df$fail,col="red")
lines(Model_err.rate_df$Trees,Model_err.rate_df$OOB)

# importance method to find the most important feature when predicting fail_7
importance(Model)

#plotting the importance of the features to understand which features are the most important
varImpPlot(Model)

#creating a importance data frame to grab the top 20 most important features 
importance_df <- as.data.frame(importance(Model))
importance_df <- importance_df[order(importance_df$MeanDecreaseAccuracy, decreasing = TRUE), ]
head(importance_df,20)
