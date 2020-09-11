data1 <- read.csv(file.choose(), skip=1, header=T)
data_clean<-data1[rowSums(is.na(data1)) < "100",]

summary(data_clean$EPI)
fivenum(data_clean$EPI, na.rm=TRUE)
stem(data_clean$EPI)

#commands used to create histogram
hist(data_clean$EPI)
hist(data_clean$EPI, seq(30., 95., 1.0), prob=TRUE, xlab="EPI",main="Histogram of UN Countries' EPI's")
lines(density(data_clean$EPI,na.rm=TRUE,bw=1.))  #maybe bw="SJ"
rug(data_clean$EPI)

#Commands used to create ECDF Plot
plot(ecdf(data_clean$EPI), do.points=FALSE, verticals=TRUE, main="Cumulative Density Function for EPI")

#commands used to create Normal Q-Q Plot
par(pty="s")
qqnorm(data_clean$EPI); qqline(data_clean$EPI)

#Q-Q plot for t distribution
x = seq(30,95,1)
qqplot(qt(ppoints(250), df=5), x, main="Q-Q plot for t dsn", xlab="Theoretical values")
qqline(x)

# Exploration of water_H variable
fivenum(data_clean$WATER_H,na.rm=TRUE)
hist(data_clean$WATER_H,seq(0,100,5),prob=TRUE,xlab="Water Health", main="Histogram of UN Countries' Water Health")
lines(density(data_clean$WATER_H,na.rm=TRUE,bw=1))
rug(data_clean$WATER_H)

plot(ecdf(data_clean$WATER_H),do.points=FALSE, verticals=TRUE, main="Cumulative Density Function for Water Health")

qqnorm(data_clean$WATER_H, main="Normal Q-Q Plot for Water Health"); qqline(data_clean$WATER_H)

#exploration of AIR_H variable
fivenum(data_clean$AIR_H)
hist(data_clean$AIR_H,seq(0,100,5),prob=TRUE,xlab="Air Quality", main="Histogram of UN Countries' Air Quality")
lines(density(data_clean$AIR_H,na.rm=TRUE,bw=1))
rug(data_clean$AIR_H)

plot(ecdf(data_clean$AIR_H), do.points=FALSE, verticals=TRUE, main="Cumulative Density Function for Air Quality")

qqnorm(data_clean$AIR_H, main="Normal Q-Q Plot for Air Quality"); qqline(data_clean$AIR_H)


#Comparison Data

boxplot(data_clean$EPI, data_clean$WATER_H, main="Box Plot Comparison of EPI and Water Health")

qqplot(data_clean$EPI, data_clean$AIR_H, main="Q-Q Plot for EPI vs Air Health",xlab="EPI",ylab="Air Quality")
