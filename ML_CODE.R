library(ggplot2)
library(dplyr)
library(statsr)
library(caret)
library(gridExtra)
load("movies.Rdata")
set.seed(123)
movies <- read.table("D:/Project/DATASET/movies.Rdata")
View(movies)
load("D:/Project/DATASET/movies.Rdata")
set.seed(123)
testingIndex <- sample(nrow(movies),floor(nrow(movies) * 0.2),replace = FALSE)
# get test and training data
testing  <- movies[testingIndex,]
training <- movies[-testingIndex,]
# response variables of interest:
# imdb_rating, critics_rating, critics_score, audience_rating, audience_score
# remove response cols, title, studio, actors and url's columns
responseCols <- c(13,15,16,17,18)
noInterestVars <- c(1,6,25:32)
trainingPredictors <- training[,-c(responseCols,noInterestVars)]
trainingResponse   <- training[, responseCols]
testingPredictors <- testing[,-c(responseCols,noInterestVars)]
testingResponse   <- testing[, responseCols]
# model 1: remove rows with NA?
# function to return which row has at least a NA
anyNA <- function(row){ any(is.na(row))}
anyNA()
# apply the function to each row
rowsWithNA <- apply(training, 1, anyNA)
# get which row return TRUE
rowsWithNAIndexTRUE <- which(rowsWithNA == TRUE)
# few rows with NA's, might be a good way of removing NA
length(rowsWithNAIndexTRUE)/nrow(training)
# In this report we will NOT use this model, as I have already compared it with the Median imputation model
# create new model with the NA rows removed
naRMTrainingData <- training[-rowsWithNAIndexTRUE,]
# Model 2: remove columns that have a lot of NA's?
# function to return which row has at least a NA
colsWithNA <- apply(trainingPredictors, 2, function(col) { any(is.na(col))})
# get cols index that have at least one NA
colsWithNAIndexTRUE <- which(colsWithNA == TRUE)
# low percentage of NA's. No need to exclude columns, take other actions to imput then
apply(trainingPredictors[,colsWithNAIndexTRUE], 2, function(cols) {sum(is.na(cols))/length(cols)})
# model 3, 4 and 5: IMPUTATIONS
kNNImput <- preProcess(training, method='knnImpute')
bagImput <- preProcess(training, method='bagImpute', na.remove = TRUE)
medianImput <- preProcess(training, method='medianImpute')
knnImputTrainingData <- predict(kNNImput, training)
bagImputTrainingData <- predict(bagImput, training)
medianImputTrainingData <- predict(medianImput, training)
medianImputTrainingPredictors <- cbind(medianImputTrainingPredictors,medianImputTrainingData[,responseCols[1]])
medianImputTrainingPredictors <- medianImputTrainingData[,-c(responseCols,noInterestVars)]
medianImputTrainingPredictors <- cbind(medianImputTrainingPredictors,medianImputTrainingData[,responseCols[1]])
# first raw model with what we got
model.raw <- lm(imdb_rating ~ .,
                data = medianImputTrainingPredictors)
summary(model.raw)
nzv <- nearZeroVar(training, saveMetrics= TRUE)
nzvIndices <- which(nzv$nzv == TRUE)
nzv[nzvIndices, ]
