#Using ggplot2
plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature,pressure$pressure, type="l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col="red")
points(pressure$temperature, pressure$pressure/2, col="blue")
qplot(temperature,pressure, data=pressure, geom="line")
qplot(temperature, pressure, data=pressure, geom="line")
ggplot(pressure, aes(x=temperature, y=pressure))+geom_line() + geom_point()
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()



#Creating Bar Graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) #generate a table of counts
qplot(mtcars$cyl) # cyl is continous here
qplot(factor(mtcars$cyl))
# Bar graph of counts
qplot(factor(cyl), data = mtcars)
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()


#creating historgrams
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data=mtcars, binwidth=4)
ggplot(mtcars,aes(x=mpg)) + geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 5)


#Box plots using GG plot
plot(ToothGrowth$supp, ToothGrowth$len)
boxplot(len ~ supp, data = ToothGrowth)
boxplot(len ~ supp + dose, data = ToothGrowth)


qplot(ToothGrowth$supp, ToothGrowth$len, geom="boxplot")
qplot(supp, len, data = ToothGrowth, geom="boxplot") #alternate syntax if datasets are in same dataframe
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot() #equivalent for ggplot
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom="boxplot")
qplot(interaction(supp, dose), len, data= ToothGrowth, geom="boxplot")

ggplot(ToothGrowth, aes(x=interaction(supp,dose), y=len)) + geom_boxplot()
