# MC - Project
----------------
  
# Read the Training and Test dataset
  
training <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"), header = FALSE) 

testing <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"), header = FALSE) 

head(training)
summary(training)

#1 _ Load the dataset
library(ggplot2);
library(caret);
qplot(V160,V5, colour=V2,data=training)

# Removing nerar Zero covariates
nsv <- nearZeroVar(training, saveMetrics=TRUE)
nsv

# Correlated predictors
M <- abs(cor(training[,-2]))
diag(M) <- 0
which(M > 0.8, arr.ind=T)

#1Load the library
library(ggplot2);
library(caret);

# Removing nerar Zero covariates
nsv <- nearZeroVar(training, saveMetrics=TRUE)
nsv