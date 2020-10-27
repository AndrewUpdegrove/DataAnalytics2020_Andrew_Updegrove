library(ISLR)
library(MASS)
library(boot)
set.seed(1)

help("sample")

#--------------------------------------------------------------
# Simple Validation
#--------------------------------------------------------------

train = sample(392,196)
#create learning regression model using training data
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# error is 23.5501


#create quadratic regression model using training data
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
# error is 18.72

#create cubic regression model ussing training data
lm.fit3 <- lm(mpg~poly(horsepower, 3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# error is 18.79

#Copied from slide for later reference:
# we find that the validation set error rates for the models with linear, quadratic,
# and cubic terms are 23.29, 18.90 and 19.25 respectively.
# The model that predict mpg using a quadratic function of horsepower performs better,
# than a models that only involves only a linear function of horsepower, and there is a,
# little evidence in favor of a model that uses a cubic function of horsepower.



#--------------------------------------------------------------
# K-Fold Validation and Leave one out validation
#--------------------------------------------------------------

# Leave one out Cross Validation
# use cv.glm() function using the parameter family = "binomial"

# K-fold Cross Validation
# use cv.glm() function using the parameter K = integer

set.seed(17)
cv.error.10 = rep(0,10)
for(i in 1:10) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit, K=10)$delta[1]
}
cv.error.10



#--------------------------------------------------------------
# Random Forest
#--------------------------------------------------------------
# Dataset can be found at https://archive.ics.uci.edu/ml/machine-learning-databases/car/
library(randomForest)
#load car.data dataset
data1 <- read.csv(file.choose(), header = TRUE)
data1[] <- lapply(data1, factor)
head(data1)

colnames(data1) <- c("BuyingPrice", "Maintenance", "NumDoors", "NumPersons", "BootSpace", "Safety", "Condition")
head(data1)
str(data1)
levels(data1$Condition)

set.seed(100)
train <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
summary(TrainSet)
summary(ValidSet)


help("randomForest")
model1 <- randomForest(Condition ~ ., data = TrainSet, importance = TRUE)
model1
#default number of trees in 500 and number of variables tried at each split is 2

# mtry = Number of variables randomly sample as candidates at each split
# Classification (sqrt(p) where p is number of variables in x) and regression (p/3)
model2 <- randomForest(Condition ~ ., data= TrainSet, ntree = 500, mtry = 6, importance = TRUE)
model2

predTrain <- predict(model2, TrainSet, type = "class")
table(predTrain, TrainSet$Condition)
predValid <- predict(model2, ValidSet, type = "class")
table(predValid, ValidSet$Condition)


#We can also use importance() function to check important variables
# The below function show the drop in mean accuracy for each of the variables
# to check the important variables
importance(model2)
varImpPlot(model2)

a = c()
for (i in 3:8) {
  model3 <- randomForest(Condition ~ ., data = TrainSet, ntree=500, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$Condition)
}
a
plot(3:8, a)


#comparing forest to decision tree
library(rpart)
library(caret)
library(e1071)
model_dt <- train(Condition ~ ., data= TrainSet, method = "rpart")
model_dt_1 = predict(model_dt, data = TrainSet)
table(model_dt_1, TrainSet$Condition)
mean(model_dt_1 == TrainSet$Condition)
table(model_dt_1, TrainSet$Condition)
mean(model_dt_1 == TrainSet$Condition)
# In-sample error is 79.4%


model_dt_vs = predict(model_dt, newdata = ValidSet)
table(model_dt_vs, ValidSet$Condition)
table(model_dt_vs == ValidSet$Condition)
