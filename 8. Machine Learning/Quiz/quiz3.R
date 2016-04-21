#quizz 3
#question 5
library(randomForest)
library(caret)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

set.seed(33833)

# Fit a random forest predictor relating the factor variable y to the remaining variables.
a <- randomForest(y ~ ., data = vowel.train, importance = FALSE)
b <- varImp(a)
order(b)
