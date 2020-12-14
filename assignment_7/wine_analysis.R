require(ggplot2)
require(e1071)
require(class)
require(dplyr)
# white and red together are one dataset
whitewine.raw <-  read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep = ";", header = T)
redwine.raw <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep = ";", header = T)
whitewine.raw$wine_type <- "white"
redwine.raw$wine_type <- "red"


set.seed(11232020)

wine.raw <- rbind(whitewine.raw, redwine.raw)
# changed quality column to factor
wine.process <- wine.raw
wine.process$quality <- as.factor(wine.process$quality)

per80 <- ceiling(nrow(wine.process) * .8)

train_indices <- sample(1:nrow(wine.process), per80)
wine.train <- wine.process[train_indices,]
wine.test <- wine.process[-train_indices,]

ggplot(data = wine.train, mapping = aes(x = fixed.acidity)) + geom_histogram() + labs(title = "Distribution of fixed acidity measurement", x= "fixed acidity")
ggplot(data = wine.train, mapping = aes(x = pH)) + geom_histogram() + labs(title = "Distibution of pH measurement")

# ----------------------------------------------------------------------------
# Support Vector Machine
# ----------------------------------------------------------------------------


ggplot(data = wine.train, mapping = aes(x = citric.acid, y = pH, color = wine_type)) + geom_point() + labs(title = "Citric Acid vs pH for UCI Wine Data set ")

sum(wine.train[which(wine.train$wine_type == "white"),]$pH)/length(which(wine.train$wine_type == "white")) # average pH value for white wine
sum(wine.train[which(wine.train$wine_type == "red"),]$pH)/length(which(wine.train$wine_type == "red")) # average pH value for red wine

sum(wine.train[which(wine.train$wine_type == "white"),]$citric.acid)/length(which(wine.train$wine_type == "white")) # average citric.acid value for white wine
sum(wine.train[which(wine.train$wine_type == "red"),]$citric.acid)/length(which(wine.train$wine_type == "red")) # average citric acid value for red wine
  

svm.data <- wine.train[,c(3,9,13)]
svm.data$wine_type <- as.factor(svm.data$wine_type)
svmfit <- svm(wine_type ~ ., data = svm.data, kernel = "linear", cost = 1, scale = FALSE)
svmfit
plot(svmfit, svm.data)


length(which(wine.test$wine_type == "red"))/nrow(wine.test)


# ----------------------------------------------------------------------------
# kNN
# ----------------------------------------------------------------------------
k_lim <- floor(sqrt(nrow(wine.train)))
k_vals <- seq(1, k_lim, by=2) # only considering odd numbered neighbors so less chances of ties
e_cv_set <- c()
wine.train.knn <- wine.train
# in analysis don't consider wine_type and alcohol

#normalizing all variables
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}
for(i in 1:10){
  wine.train.knn[i] <- normalize(wine.train.knn[i])
}

#use leave one out cross validation error
for(k in k_vals) {
  print(paste("Running nearest neighbor with k = ", k))
  sum = 0
  for(i in 1:nrow(wine.test)){
    temp_score <- knn(wine.train.knn[-i,1:10], wine.train.knn[i,1:10],wine.train.knn[-i,12], k)
    if(temp_score != wine.train.knn[i,12]){
      sum = sum + 1
    }
  }
  e_cv_set <- c(e_cv_set, sum/nrow(wine.train.knn))
}
ggplot(data = NULL, mapping = aes(x = k_vals, y = e_cv_set)) + geom_line() + labs(title = "Cross Validation Error Performed for various number of nearest neighbors", x = "k", y = "Cross Validation Error")


# 
wine.test.knn <- knn(wine.train.knn[,1:10], wine.test[,1:10], wine.train.knn[,12], 4)
wine.test.out_err <- sum(wine.test.knn != wine.test[,12])/nrow(wine.test)


# ------------------------------------------------------------------------------
# Random Forest
# ------------------------------------------------------------------------------
require(randomForest)

wine.less <- wine.train
wine.less[,c(11,13)] <- NULL

wine.ran_forest = randomForest(quality ~ ., data = wine.less, mtry = sqrt(10), importance = TRUE)
print(wine.ran_forest)

wine.forest_pred <- predict(wine.ran_forest, wine.test[,c(-11,-12,-13)])
table(wine.test[,12], wine.forest_pred)
ran_forest_accuracy <- mean(wine.test[,12] == wine.forest_pred)

# visualization code obtained from https://towardsdatascience.com/implement-random-forest-in-r-b00b69eb8501
importance = importance(wine.ran_forest)
varImportance = data.frame(Variables = row.names(importance),
                           Importance =round(importance[, "MeanDecreaseAccuracy"],2))
rankImportance=varImportance%>%mutate(Rank=paste('#',dense_rank(desc(Importance))))
ggplot(rankImportance,aes(x=reorder(Variables,Importance), y=Importance,fill=Importance)) + 
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust=0, vjust=0.55, size = 4, colour = 'white') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic() +
  theme(axis.text.y = element_text(size=13, face = "bold"))

varImportance.gini = data.frame(Variables = row.names(importance),
                           Importance =round(importance[, "MeanDecreaseGini"],2))
rankImportance.gini=varImportance.gini%>%mutate(Rank=paste('#',dense_rank(desc(Importance))))
ggplot(rankImportance.gini,aes(x=reorder(Variables,Importance), y=Importance,fill=Importance)) + 
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust=0, vjust=0.55, size = 4, colour = 'white') +
  labs(x = 'Variables') +
  coord_flip() +
  theme_classic() + 
  theme(axis.text.y = element_text(size=13, face = "bold"))

