require(dplyr)
require(ggplot2)
require(randomForest)
require(e1071)
abs.raw <- read.csv(file.choose(), header = T, sep = ";")
abs.data <- abs.raw

ggplot(data = abs.raw, aes(x= Age, fill=Education)) + geom_bar()

sort(unique(abs.data$ID)) # data is for 36 employees. Actually 37 employees because the ID 29 was used twice
which(abs.data$ID == 29)
which(abs.data$ID == 29 & abs.data$Age == 28)
which(abs.data$ID == 29 & abs.data$Age == 41)

# fixing the double ID 29 issue
abs.data[which(abs.data$ID == 29 & abs.data$Age == 41),]$ID <- 37

# split the data set into two, one for information about unique people
# and another data set of the actual absentee information
abs.personnel <- abs.data %>% select(ID, Transportation.expense, Distance.from.Residence.to.Work, Service.time, Age, Education, Son, Social.drinker, Social.smoker, Pet, Weight, Height, Body.mass.index) %>% distinct()
abs.instances <- abs.data %>% select(ID, Reason.for.absence, Month.of.absence, Day.of.the.week, Seasons, Work.load.Average.day, Hit.target, Disciplinary.failure,Absenteeism.time.in.hours, )

#this data set has lots of discrete values

# I like this graph, it's pretty
abs.personnel %>% mutate(Education = as.factor(Education), Age = as.factor(Age)) %>% ggplot(mapping = aes(x = Age, fill = Education)) + geom_bar() + scale_fill_hue(labels = c("High school", "Graduate", "Post-graduate", "Master and Doctor"))

table(abs.instances$ID)
table(abs.instances[which(abs.instances$ID == 3),]$Reason.for.absence)

# ------------------------------------------------------------------------------
# Linear Regression of weight vs average time gone
# ------------------------------------------------------------------------------

# adding average time gone as variable in personnel table
abs.personnel$avg_abs_time <- NA
for (i in abs.personnel$ID){
  abs.personnel[which(abs.personnel$ID == i),"avg_abs_time"] <- abs.instances[which(abs.instances$ID == i),"Absenteeism.time.in.hours"] %>% sapply(as.numeric) %>% mean()
}

bmi_lm <- lm(avg_abs_time ~ Body.mass.index, data = abs.personnel)
plot(avg_abs_time ~ Body.mass.index, data = abs.personnel, xlab = "Body Mass Index", ylab="Average Absenteeism Time")
abline(bmi_lm)
summary(bmi_lm) # no correlation

# ------------------------------------------------------------------------------
# SVM to predict if an absence is a disciplinary failure
# ------------------------------------------------------------------------------
# will be removing ID 3 because it appears as though something bad happened to them
svm.full <- abs.data[which(abs.data$ID != 3), ] %>% select(ID,Reason.for.absence, Month.of.absence,Day.of.the.week,Age,Education,Social.drinker,Disciplinary.failure)
svm.full$Disciplinary.failure <- as.factor(svm.full$Disciplinary.failure)
#split into train and test 80-20
samp_size <- ceiling(.8 * nrow(svm.full))
samp_indices <- sample(nrow(svm.full), samp_size)
svm.train <- svm.full[samp_indices,]
svm.test <- svm.full[-samp_indices,]

# performing 10 fold cross validation to find best cost
svm.model <- svm(Disciplinary.failure ~ ., data = svm.train, kernel = "radial")

svm.tune <- tune(svm, Disciplinary.failure ~ ., data = svm.train, kernel = "radial", ranges = list(cost = c(.1,.5,1,2,3,4,5,8,10)))
summary(svm.tune)
ggplot(data = NULL, mapping = aes(x=svm.tune$performances$cost, y=svm.tune$performances$error)) + geom_line() + labs(title = "10 Fold Cross Validation on Absenteeism Radial SVM", x = "Cost", y = "Erorr")
svm.best <- svm.tune$best.model

svm.pred <- predict(svm.best, svm.test[,names(svm.test) != "Disciplinary.failure"])
table(svm.test$Disciplinary.failure,svm.pred)
mean(svm.pred == svm.test$Disciplinary.failure)


# -----------------------------------------------------------------------------
# Random forest on predicting reason for absence
# -----------------------------------------------------------------------------
#let's use all the variables fuckers
forest.full <- abs.data[which(abs.data$ID != 3), ] %>% select(Reason.for.absence, Month.of.absence, Day.of.the.week, Age, Disciplinary.failure, Education, Son, Social.drinker, Social.smoker, Pet, Body.mass.index, Absenteeism.time.in.hours)
forest.full$Reason.for.absence <- as.factor(forest.full$Reason.for.absence)

#only use 0, 1, 6, 7, 10, 11, 12,13, 14, 18, 19, 22, 23, 25, 26, 27, 28
forest.partial <- forest.full[forest.full$Reason.for.absence %in% c(0,1,6,7,10,11,12,13,14,18,19,22,23,25,26,27,28),]
forest.partial$Reason.for.absence <- as.factor(as.numeric(forest.partial$Reason.for.absence))


forest.samp_size <- ceiling(.8 * nrow(forest.partial))
forest.samp_indices <- sample(nrow(forest.partial), forest.samp_size)
forest.train <- forest.partial[forest.samp_indices,]
forest.test <- forest.partial[-forest.samp_indices,]


forest.model = randomForest(Reason.for.absence ~ ., data = forest.train, mtry = 3, importance = TRUE)
forest.predict <- predict(forest.model, forest.test[,names(forest.test) != "Reason.for.absence"])
table(forest.test$Reason.for.absence, forest.predict)
ran_forest_accuracy <- mean(forest.test$Reason.for.absence == forest.predict) #it all went wrong and that's okay <3 cate
