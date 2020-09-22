multivariate <- read.csv(file.choose(), header=T)
attach(multivariate)
names(multivariate)
multivariate


#Scatterplots
plot(Income,Immigrant, main="Scatterplot")
plot(Immigrant,Homeowners)

#fitting Linear Models
mm = lm(Homeowners ~ Immigrant)
mm
plot(Immigrant, Homeowners)
abline(mm)
abline(mm, col="Green", lwd=3)

summary(mm)
attributes(mm)
mm$coefficients
