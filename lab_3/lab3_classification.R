library(rpart)
library(rpart.plot)
library(ggplot2)
data("msleep")
str(msleep)
help("msleep")


mSleepDF1 <- msleep[,c(3,6,10,11)]

help("rpart")

sleepModel_1 <- rpart(sleep_total~., data=mSleepDF1, method="anova")
sleepModel_1

rpart.plot(sleepModel_1, type=3, fallen.leaves = TRUE)
#type - 3, draw separate split labels for the left and right directions

#fallen.leaves = TRUE, default TRUE to position the leaf nodes at the bottom of the graph
#use FALSE if graph is too crowded

rpart.plot(sleepModel_1, type = 3, digits = 3, fallen.leaves = TRUE)
rpart.plot(sleepModel_1, type = 3, digits = 4, fallen.leaves = TRUE)


require(C50)
data("iris")
head(iris)
set.seed(69420)
#generate random numbers
grn <- runif(nrow(iris))

irisrand <- iris[order(grn),]
classificationmodel1 <- C5.0(irisrand[1:100,-5],irisrand[1:100,5])
summary(classificationmodel1)


prediction1 <- predict(classificationmodel1, irisrand[101:150,])
table(irisrand[101:150,5],Predicted = prediction1)

plot(classificationmodel1)
