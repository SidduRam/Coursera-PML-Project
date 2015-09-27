library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rpart)
install.packages(rattle)

set.seed(125)

##Question 1:
-------------------------

# 1. Subset the data to a training set and testing set based on the Case variable in the data set.

inTrain <- createDataPartition(y = segmentationOriginal$Case, list = FALSE)
train <- subset(segmentationOriginal, Case == "Train")
test <- subset(segmentationOriginal, Case == "Test")

# 2. Set the seed to 125 and fit a CART model with the rpart method using all 
# predictor variables and default caret settings. 

modFit <- train(Class ~ ., data = train, method = "rpart")
modFit$finalModel

# 3. In the final model what would be the final model prediction for cases with the following variable values:

# Look at the output
# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2       PS
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100       WS
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100        PS  
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2              Not possible to predict


plot(modFit$finalModel, uniform = TRUE, main = "Classification Tree")
text(modFit$finalModel, use.n = TRUE, all = TRUE, cex = .8)

fancyRpartPlot(modFit$finalModel)
fancyRpartPlot(modFit)

plot(modFit$finalModel, uniform=T)
text(modFit$finalModel, cex=0.8)

predict(modFit, newdata = train)

-----------------------------------
  Question 3:
  --------------------
library(caret)
library(pgmm)
data(olive)
olive = olive[,-1]
library(randomForest)

#Fit a classification tree where Area is the outcome variable. 
# Then predict the value of area for the following data frame using the tree command with all defaults

model <- train(Area ~ ., data = olive, method = "rpart2")

newdata = as.data.frame(t(colMeans(olive)))

predict(model, newdata = newdata)

# 2.875. It is strange because Area should be a qualitative variable - but tree 
# is reporting the average value of Area as a numeric variable in the leaf predicted for newdata


# Problem 4.
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train <- sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA <- SAheart[train,]
testSA <- SAheart[-train,]
set.seed(13234)
logitModel <- train(chd ~ age + alcohol + obesity + tobacco + 
                      typea + ldl, data=trainSA, method="glm", 
                    family="binomial")
logitModel
missClass <- function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
predictTrain <- predict(logitModel, trainSA)
predictTest <- predict(logitModel, testSA)
# Training Set Misclassification rate
missClass(trainSA$chd, predictTrain) # 0.2727273
# Test Set Misclassification rate
missClass(testSA$chd, predictTest) # 0.3116883

# Problem 5.
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
head(vowel.train)
head(vowel.test)
dim(vowel.train) # 528  11
dim(vowel.test) # 462  11
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
modelRf <- randomForest(y ~ ., data = vowel.train, importance = FALSE)
order(varImp(modelRf), decreasing=T)
# The order of the variables is:
#  x.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7,x.10
