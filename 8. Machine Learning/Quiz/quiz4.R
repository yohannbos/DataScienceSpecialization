
#question 1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
head(vowel.train)

vowel.train$y<-factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)

suppressMessages(library(caret))
set.seed(33833)

rfmodel<-supressMessages(train(y~., data=vowel.train, method = "rf"))
gbmmodel<-supressMessages(train(y~., data=vowel.train, method = "gbm"))

rfresult<-predict(rfmodel, vowel.test)
gbmresult<-predict(gbmmodel, vowel.test)

confusionMatrix(vowel.test$y, rfresult)$overall['Accuracy']
confusionMatrix(vowel.test$y, gbmresult)$overall['Accuracy']

predDF<- data.frame(rfresult, gbmresult, diagnosis=testing$diagnosis)
combModFit<- train(diagnosis~., data = predDF, method = "gbm")
combPred<- predict(combModFit)
confusionMatrix(testing$diagnosis, combPred)$overall['Accuracy']

#question 2
suppressMessages(library(caret))
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)

data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
rfmodel2 = suppressMessages(train(diagnosis~., data = training, method = "rf"))
gbmmodel2 = suppressMessages(train(diagnosis~., data = training, method = "gbm"))
ldamodel2 = suppressMessages(train(diagnosis~., data = training, method = "lda"))

rfresult2<-predict(rfmodel2, testing)
gbmresult2<-predict(gbmmodel2, testing)
ldaresult2<-predict(ldamodel2, testing)

confusionMatrix(testing$diagnosis, rfresult2)$overall['Accuracy']
confusionMatrix(testing$diagnosis, gbmresult2)$overall['Accuracy']
confusionMatrix(testing$diagnosis, ldaresult2)$overall['Accuracy']

predDF<- data.frame(rfresult2, gbmresult2, ldaresult2, diagnosis=testing$diagnosis)
combModFit<- suppressMessages(train(diagnosis~., data = predDF, method = "rf"))
combPred<- predict(combModFit, testing$diagnosis)
confusionMatrix(testing$diagnosis, combPred)$overall['Accuracy']

#question 3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
lassomodel<-suppressMessages(train(CompressiveStrength~., data = training, method = "lasso") )
plot.enet(lasso.model$finalModel, xvar="penalty", use.color=TRUE)

#question 4


library(lubridate) # For year() function below

d<-getwd()
url<-"https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
download.file(url,file.path(d, "gaData.csv"), mode="wb")
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

# Fit a model using the bats() function in the forecast package to the training
# time series. Then forecast this model for the remaining time points. For how
# many of the testing points is the true value within the 95% prediction
# interval bounds?

library(forecast)
library(quantmod)

# fit a model
fit <- bats(tstrain)

# check how long the test set is, so you can predict beyond trainign
h <- dim(testing)[1]

# forecast the model for remaining time points
fcast <- forecast(fit, level = 95, h = h)

# get the accuracy
accuracy(fcast, testing$visitsTumblr)


# check what percentage of times that the actual number of visitors was within
# 95% confidence interval

result <- c()
l <- length(fcast$lower)

for (i in 1:l){
  x <- testing$visitsTumblr[i]
  a <- fcast$lower[i] < x & x < fcast$upper[i]
  result <- c(result, a)
}

sum(result)/l * 100

#question 5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

# Set the seed to 325 and fit a support vector machine using the e1071 package
# to predict Compressive Strength using the default settings. Predict on the
# testing set. What is the RMSE?

set.seed(325)

library(e1071)
library(caret)

fit <- train(CompressiveStrength ~ ., data = training, method = "svmRadial")

prediction <- predict(fit, testing)

RMSE(prediction, testing$CompressiveStrength)








