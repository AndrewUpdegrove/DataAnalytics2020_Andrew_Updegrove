require(rpart)
require(titanic)

part <- rpart(Survived ~ Pclass + Sex + Age + SibSp, data = titanic_train)
plot(part)
text(part)

