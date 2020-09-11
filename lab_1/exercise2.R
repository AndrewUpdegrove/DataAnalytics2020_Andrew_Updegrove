data1 <- read.csv(file.choose(), skip=1, header=T)
data_clean<-data1[rowSums(is.na(data1)) < "100",]

EPILand = data_clean$EPI[!data_clean$Landlock]
Eland = EPILand[!is.na(EPILand)]
hist(Eland, seq(30,95,1.0),prob=TRUE)


par(mfrow=c(2,1))
hist(data_clean$EPI[data_clean$EPI_regions == "Europe"], seq(0,100,5), prob=TRUE, xlab="EPI", main="EPI's for European Countries")
hist(data_clean$EPI[data_clean$EPI_regions == "Middle East and North Africa"], seq(0,100,5), prob=TRUE, xlab="EPI", main = "EPI's for African Countries")
