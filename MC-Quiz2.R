# MC_Quiz2
------------------

# Question 1:
------------------
library(AppliedPredictiveModeling);
library(caret);
data(AlzheimerDisease);
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

# Question 2:
----------------------------
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
# Option 1
histogram(x=training$Superplasticizer, data=training, xlab="SuperPlasticizer")
# Option 2:
ggplot(training, aes(x=as.factor(Age), fill=Superplasticizer)) + 
  geom_histogram(binwidth=.5,alpha=.5)

# Question 3:
-----------------------------------------
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
modelFit <- train(training$diagnosis~., method="glm",preProcess="pca",data=training)
confusionMatrix(testing$diagnosis, predict(modelFit,testing))

# answer to question
IL_str <- grep("^IL", colnames(training), value = TRUE)
preProc <- preProcess(training[, IL_str], method = "pca", thresh = 0.8)
preProc  # 9 components
preProc$rotation


# Question: 4
---------------
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(3433)
## grep the predictors starting with 'IL'
IL_str <- grep("^IL", colnames(training), value = TRUE)
## make a subset of these predictors
predictors_IL <- predictors[, IL_str]
df <- data.frame(diagnosis, predictors_IL)
inTrain = createDataPartition(df$diagnosis, p = 3/4)[[1]]
training = df[inTrain, ]
testing = df[-inTrain, ]
## train the data using the first method
modelFit <- train(diagnosis ~ ., method = "glm", data = training)

predictions <- predict(modelFit, newdata = testing)
## get the confustion matrix for the first method
C1 <- confusionMatrix(predictions, testing$diagnosis)
print(C1)


## do similar steps with the caret package
modelFit <- train(training$diagnosis ~ ., method = "glm", preProcess = "pca", 
                  data = training, trControl = trainControl(preProcOptions = list(thresh = 0.8)))
C2 <- confusionMatrix(testing$diagnosis, predict(modelFit, testing))
print(C2)