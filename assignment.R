# Practical Machine Learning - Course Project
# Derek Brown

setwd("/Users/Derek/Desktop/Coursera/8 - Practical Machine Learning/Assignment/")

library(caret)
library(randomForest)

# Load the training and testing datasets
training <- read.csv("pml-training.csv", na.strings=c("","#DIV/0!","NA"))
testing <- read.csv("pml-testing.csv", na.strings=c("","#DIV/0!","NA"))

plot(training$classe)

# Retain columns that are less than 90% missing (NA)
goodData <- training[colSums(is.na(training)) / nrow(training) < .9]
rm(training)

# Convert appropriate features to numeric values
predictors <- names(goodData)[grep("arm|belt|dumbbell", names(goodData))]
goodData[,predictors] <- sapply(goodData[,predictors],as.numeric)
testing[,predictors] <- sapply(testing[,predictors],as.numeric)

# Only retain the outcome (classe) and predictors and remove columns pertaining to data gathering (time, X)
goodData <- data.frame(goodData$classe, goodData[,predictors])
names(goodData)[1] <- "classe"
testing <- data.frame(testing$problem_id, testing[,predictors])
names(testing)[1] <- "problem_id"

# Subset the data into a training (75%) and a validation (25%) set
inTrain <- createDataPartition(goodData$classe, p=.75, list=F)
training <- goodData[inTrain,]
validation <- goodData[-inTrain,]

# Fit a random forest model using the training data
fit <- randomForest(classe ~ ., data=training)

# Look at the most important features
imp <- varImp(fit)[order(-varImp(fit)$Overall),,drop=F]
plot(training$roll_belt, training$yaw_belt, col=training$classe)

# Estimate the out-of-bag accuracy using the validation data
test <- predict(fit, newdata=validation)
confusionMatrix(test, validation$classe)

# predict the classe of the test data
answers <- predict(fit, newdata=testing)

# Write answers out to individual files for submission to Coursera
# pml_write_files = function(x){
#     n = length(x)
#     for(i in 1:n){
#         filename = paste0("problem_id_",i,".txt")
#         write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
#     }
# }
# 
# pml_write_files(answers)