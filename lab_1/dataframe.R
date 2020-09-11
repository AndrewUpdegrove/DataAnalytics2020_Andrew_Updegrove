days <- c('Mon', 'Tue', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun')
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed <- c('T', 'T', 'F', 'F', 'T', 'T', 'F')

RPI_WEATHER_WEEK <- data.frame(days,temp,snowed)

head(RPI_WEATHER_WEEK)

str(RPI_WEATHER_WEEK)

summary(RPI_WEATHER_WEEK)

RPI_WEATHER_WEEK[1,] #show the 1st row and all columns
RPI_WEATHER_WEEK[,1] #showt the 1st column and all rows

RPI_WEATHER_WEEK[,'snowed']
RPI_WEATHER_WEEK[,'days']
RPI_WEATHER_WEEK[,'tem']
RPI_WEATHER_WEEK[1:5,c("days","temp")]