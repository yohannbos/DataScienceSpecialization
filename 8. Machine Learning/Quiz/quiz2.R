#Question 1

library(caret)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

#Question 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
head(training)
library(Hmisc)

#solution 1
names <- colnames(concrete)
names <- names[-length(names)]
featurePlot(x = training[, names], y = training$CompressiveStrength, plot = "pairs")
index <- seq_along(1:nrow(training))
ggplot(data = training, aes(x = index, y = CompressiveStrength)) + geom_point() +
  theme_bw()
cutCS <- cut2(training$CompressiveStrength, g = 4)
summary(cutCS)
featurePlot(x = training[, names], y = cutCS, plot = "box")

#solution 2
library(Hmisc)
mixtures$cementf<-cut2(mixtures$Cement)
plot(mixtures$CompressiveStrength, col=mixtures$cementf)

mixtures$BlastFurnaceSlagf<-cut2(mixtures$BlastFurnaceSlag)
plot(mixtures$CompressiveStrength, col=mixtures$BlastFurnaceSlagf)

mixtures$Agef<-cut2(mixtures$Age)
plot(mixtures$CompressiveStrength, col=mixtures$Agef)

mixtures$FlyAshf<-cut2(mixtures$FlyAsh)
plot(mixtures$CompressiveStrength, col=mixtures$FlyAshf)


#Question 3
hist(log(mixtures$Superplasticizer+1))

#Question 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
IL_str <- grep("^IL", colnames(training), value = TRUE)
preProc <- preProcess(training[, IL_str], method = "pca", thresh = 0.8)
preProc$rotation


#question 5
set.seed(3433)
## grep the predictors starting with 'IL'
IL_str <- grep("^IL", colnames(training), value = TRUE)
## make a subset of these predictors
predictors_IL <- predictors[, IL_str]
df <- data.frame(diagnosis, predictors_IL)
inTrain = createDataPartition(df$diagnosis, p = 3/4)[[1]]
training = df[inTrain, ]
testing = df[-inTrain, ]
## train the data using the first method
modelFit <- train(diagnosis ~ ., method = "glm", data = training)

predictions <- predict(modelFit, newdata = testing)
## get the confustion matrix for the first method
C1 <- confusionMatrix(predictions, testing$diagnosis)
print(C1)

modelFit <- train(training$diagnosis ~ ., method = "glm", preProcess = "pca",
                  data = training, trControl = trainControl(preProcOptions = list(thresh = 0.8)))
C2 <- confusionMatrix(testing$diagnosis, predict(modelFit, testing))
print(C2)

