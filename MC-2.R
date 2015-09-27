# To work Tableau software
install.packages("Rserve")
library(Rserve)
Rserve()


# MC - Wages Datset - ISLR
#-------------------------------

#1 _ Load the dataset
library(ISLR);
library(ggplot2);
library(caret);
data(Wage);
Wage <- subset(Wage, select=-c(logwage))

# 2: Get the Training / Test dataset
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,];
test <- Wage[-inTrain,];
dim(training);
dim(test);

# 3: Plot the values and predict the key features
featurePlot(x=training[, c("age", "education", "jobclass")], y=training$wage, plot="pairs")

# 4: Plot age vs Wage value
qplot(age, wage, data=training)

# 5: Plot age vs Wage colour by Jobclass
qplot(age, wage, color=jobclass, data=training)

# 6: Plot age vs Wage colour by Education
qplot(age, wage, color=education, data=training)

# 7: Fit a Linder Model
modelFit <-train(wage ~ age + jobclass + education, method="lm", data=training)
modelFit
finModel <- modelFit$finalModel
print(finModel)

# 8: Diagnostics
plot(finModel, 1, pch=19, cex=0.5, col="#00000010")

# 9: Predicted versus truth in the test set
pred <-predict(finModel, test)
qplot(wage, pred, colour=year, data=test)