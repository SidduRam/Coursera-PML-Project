#Load the library
library(ggplot2);
library(caret);
library(randomForest)

# Project Processing / Result code
#-----------------------------------------------

# The Raw Data - # Download the file if does not exist in local system
if (!file.exists("./Data/pml-training.csv")){
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                "./Data/pml-training.csv")
}

if (!file.exists("./Data/pml-testing.csv")){
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                "./Data/pml-testing.csv")
}

# Load the training data
#-------------------------
trainingdata = read.csv("./Data/pml-training.csv", na.strings = c("NA", ""))
dim(trainingdata)
summary(trainingdata$classe)

testingdata = read.csv("./Data/pml-testing.csv", na.strings = c("NA", ""))

# Removing nerar Zero covariates
nzv <- nearZeroVar(trainingdata,saveMetrics=TRUE)
trainingdata <- trainingdata[,nzv$nzv==FALSE]

nzv <- nearZeroVar(testingdata,saveMetrics=TRUE)
testingdata <- testingdata[,nzv$nzv==FALSE]
                   

# Partioning the training set into two
inTrain <- createDataPartition(y=trainingdata$classe, p=0.6, list=FALSE)
projTraining <- trainingdata[inTrain, ]; projTesting <- trainingdata[-inTrain, ]
dim(projTraining); dim(projTesting)



# Killing first column of Dataset - ID Removing first ID variable so that it does not interfer with ML Algorithms:
projTraining <- projTraining[c(-1)]

# Remove the columns / Variables has too many NAs (keep only the variable > 60% threshold of NA's)

subprojTraining <- projTraining #creating another subset to iterate in loop
for(i in 1:length(projTraining)) { #for every column in the training dataset
  if( sum( is.na( projTraining[, i] ) ) /nrow(projTraining) >= .6 ) { #if n?? NAs > 60% of total observations
    for(j in 1:length(subprojTraining)) {
      if( length( grep(names(projTraining[i]), names(subprojTraining)[j]) ) ==1)  { #if the columns are the same:
        subprojTraining <- subprojTraining[ , -j] #Remove that column
      }   
    } 
  }
}

#To check the new N?? of observations
dim(subprojTraining)
str(subprojTraining)

clean1 <- colnames(subprojTraining)
clean2 <- colnames(subprojTraining[, -58]) #already with classe column removed
projTesting <- projTesting[clean1]
projTraining <- subprojTraining

testing <- testingdata[clean2]

#To check the new N?? of observations
dim(projTesting)
dim(testing)

# Model Builinding ~ Using ML algorithms for prediction: Random Forests

# Train model with random forest due to its highly accuracy rate. 
modFitB1 <- randomForest(classe ~. , data=subprojTraining)

# Predicting in-sample error:
predictionsB1 <- predict(modFitB1, projTesting, type = "class")

#(Moment of truth) Using confusion Matrix to test results:
confMatrix <- confusionMatrix(predictionsB1, projTesting$classe)
confMatrix 

# Let's have a look at the accuracy

confMatrix$overall[1]
##  Accuracy 
## 0.9980882

#It looks very good - it is more then 99.85%.

# Random Forests yielded better Results, as expected!

#RESULTS - Predictions on the real testing set

predictionsB2 <- predict(modFitB1, testing)
predict(modFitB1, testing[, -length(names(subprojTraining))])

mean(predict(model, testing) == testing$classe) * 100