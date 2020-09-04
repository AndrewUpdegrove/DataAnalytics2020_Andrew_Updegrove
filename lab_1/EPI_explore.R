data1 <- read.csv(file.choose(), skip=1, header=T)
data_clean<-data1[rowSums(is.na(data1)) < "100",]
summary(data_clean$EPI)
data_clean[which.max(data_clean$CLIMATE), "Country"]