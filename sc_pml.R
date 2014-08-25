library(caret)
library(psych)
library(ggplot2)
library(descr)
library(randomForest)
library(corrplot)
set.seed(3433)

training_data <- read.csv("pml-training.csv", header=T, row.names=1)
testing_data <- read.csv("pml-testing.csv", header=T, row.names=1)

#lets look into the data
summary(training_data)
describe(training_data)
sapply(training_data, class)

#a lot of columns contain large share of blank data and #DIV/0!, lets turn em into NA
training_data <- read.csv("pml-training.csv", header=T, row.names=1, na.strings = c("NA", "","#DIV/0!"))
testing_data <- read.csv("pml-testing.csv", header=T, row.names=1, na.strings = c("NA", "","#DIV/0!"))

#lets get rid of columns with NA's (NA's form ~ 98% of values in columns where they occure)
training_data <-training_data[,colSums(is.na(training_data)) == 0] 
testing_data <-testing_data[,colSums(is.na(testing_data)) == 0] 

#lets remove analysis-meaningless columns like timestamp, username, window etc. 
training_data <- training_data[-(1:6)]
testing_data <- testing_data[-(1:6)]

#Now lets split the training dataset foe actual training and validation purposess
inTrain<-createDataPartition(training_data$classe, p=3/4, list=FALSE)
training<-training_data[inTrain,]
validating<-training_data[-inTrain,]

#Now I will check the correlation among the variables 
CorMx<-abs(cor(training[,-53]))
corrplot(CorMx, method = "color", tl.cex = 0.8)
#image 1

#the graph shows strong corr, specifically among in belt, accel_arm/magnet_arm,gyros and dumbbell variables groups
#lets move to random forest (which takes pca analysis and repeated cross-validation)
preProc <- preProcess(training[, -53], method = "pca", thresh = 0.99)
trainingPC <- predict(preProc, training[, -53])
validatingPC <- predict(preProc, validating[,-53])

modelFit <- train(training$classe ~ ., 
                  method = "rf", 
                  data = trainingPC,
                  trControl = trainControl(method = "repeatedcv", number=5, repeats=5))
                  
#cross valdation results show pretty high accuracy ~ 0.98, whle sensitivity and specificity is also rather good, 
#lowest sensitivity is for class C, still it is 0.94 which is pretty high
C1 <- confusionMatrix(validating$classe, predict(modelFit, validatingPC))
print(C1)

#lets do pca on testing set and run model across the result
testingPC <- predict(preProc, testing_data[, -53])
stats<- predict(modelFit, testingPC) 


#lets write the results to files
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(stats)
