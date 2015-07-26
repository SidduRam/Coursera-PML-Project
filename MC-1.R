# Machine Learning - Code / try...samples

library(caret);
library(kernlab);
install.packages('e1071', dependencies=TRUE)
data(spam)


# splitting the data into two training /test
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
test <- spam[-inTrain,]
dim(training)
dim(test)

# Fit a model
set.seed(32343)
modelFit <- train(type~., data=training, method="glm")
modelFit

# Including the preProcessig
modelFit1 <- train(type~., data=training, preProcess=c("center","Scale"), method="glm")
modelFit1
