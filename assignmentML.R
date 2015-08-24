
library(caret);  library(readr); library(mlbench);  library(randomForest); 

set.seed(0)
raw_dataset <- read_csv("pml-training.csv")
test <- read_csv("pml-testing.csv")

isAnyValueMissing <- sapply(test, function (x) any(is.na(x) | x == ""))

isCandidatePredictor <- !isAnyValueMissing & grepl("belt|[^(fore)]arm|dumbbell|forearm", names(isAnyValueMissing))

candidatePredictors <- names(isAnyValueMissing)[isCandidatePredictor]

dataset <- raw_dataset[, candidatePredictors]
dataset$classe <- as.factor(raw_dataset$classe)

inTrain <- createDataPartition(y = dataset$classe, p = .7,list = FALSE)
training <- dataset[ inTrain,]
testing <- dataset[-inTrain,]


rf <- randomForest(training[,-53], training$classe,  ntree=300, do.trace=100)
classesPrediction_randomForest <- predict(rf, newdata = testing[,-53])
confusionMatrix(data = classesPrediction_randomForest, testing$classe)


rf_caret <- train(x=training[,-53],y=training$classe, method = "rf", ntree=300);
classesPrediction_rf_caret <- predict(RFFit, newdata = testing[,-53])
confusionMatrix(data = classesPrediction_rf_caret, testing$classe)