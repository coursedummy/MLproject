## Predicting classe variable



The goal of of this analysis was to predict the classe variable using data from https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv.  The actual meaning of the variables was ignored for this analysis.

## Step 1: choosing predictor variables



First, timestamps and username columns were removed.  Then, columns with factor variables were ignored simply to reduce the amount of work needed to create dummy variables and to reduce to overall number of possible variables.  Then, columns with all NA values in the test data set were excluded from the training data set.  Finally, a random subset (1000 rows) of the trimmed data was selected as a training set for the model.


```r
train <- read.csv("~/class/practicalML/data/pml-training.csv")
test <- read.csv("~/class/practicalML/data/pml-testing.csv")

## Ignore factor columns
ccs <- which(sapply(names(train), function(i) class(train[, i])) != "factor")
classe <- train$classe
train <- train[, ccs]
test <- test[, ccs]

## Remove some NA columns and timestamps
sapply(1:ncol(train), function(i) nrow(train[is.na(train[, i]), ]))
sapply(1:ncol(test), function(i) nrow(test[is.na(test[, i]), ]))
nacols <- which(sapply(1:ncol(test), function(i) all(is.na(test[, i]))))
test <- test[, -c(1:4, nacols)]
train <- train[, -c(1:4, nacols)]

## Use subset of train for fitting
set.seed(1000)
samp <- sample(1:nrow(train), tsize)
trn <- train[samp, ]
cs <- classe[samp]
```

The resulting training set used 52 variables as predictors.

## The model
The train() function for the caret package was used as a wrapper to fit a random forest model from the R package 'randomForest'.  The random forest model was fit using default settings.


```r
require(caret)
mod <- train(cs ~ ., method = "rf", data = trn, verbose = FALSE)
pred <- predict(mod, test)
```


## Cross-validation
A subset of the training data set (3000 rows) that was not used for the actual training of the model was selected for cross-validation.


```r
cvsamp <- sample(1:nrow(train)[-samp], cvsize)
cv <- train[cvsamp, ]
cvpred <- predict(mod, cv)
confMat <- confusionMatrix(classe[cvsamp], cvpred)
acc <- confMat$overall[["Accuracy"]]
lwr <- confMat$overall[["AccuracyLower"]]
upr <- confMat$overall[["AccuracyUpper"]]
```


## Conclusion
So, using a training set of size 1000 and a cross-validation set of size 3000 a random forest model achieved an accuracy of 0.9113 (95% CI: (0.9006, 0.9213)).
