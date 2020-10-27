library(ggplot2)
library(gridExtra)
library(dplyr)
#dataset 1 (nyt4)
ds1 = read.csv(file.choose(), header=T)
name1 = "NYT4"
#dataset 2 (nyt6)
ds2 = read.csv(file.choose(), header=T)
name2 = "NYT6"
#dataset 3 (nyt9)
ds3 = read.csv(file.choose(), header=T)
name3 = "NYT9"
#dataset 4 (nyt20)
ds4 = read.csv(file.choose(), header=T)
name4 = "NYT20"
#dataset 5 (nyt31)
ds5 = read.csv(file.choose(), header=T)
name5 = "NYT31"

#looking at age and impressions
var1 = "Age"
var2 = "Impressions"
list_names = c(name1,name2,name3,name4,name5)
label1 = paste(name1, " (N = ",toString(nrow(ds1)), ")", sep = "")
label2 = paste(name2, " (N = ",toString(nrow(ds2)), ")", sep = "")
label3 = paste(name3, " (N = ",toString(nrow(ds3)), ")", sep = "")
label4 = paste(name4, " (N = ",toString(nrow(ds4)), ")", sep = "")
label5 = paste(name5, " (N = ",toString(nrow(ds5)), ")", sep = "")

ds1_reg <- ds1 %>% filter(Signed_In == 1)
ds2_reg <- ds2 %>% filter(Signed_In == 1)
ds3_reg <- ds3 %>% filter(Signed_In == 1)
ds4_reg <- ds4 %>% filter(Signed_In == 1)
ds5_reg <- ds5 %>% filter(Signed_In == 1)


#boxplots for age, question 1 and 2
bpa1 <- boxplot(ds1[,1],ds2[,1],ds3[,1],ds4[,1],ds5[,1], names = list_names, ylab=var1, main = "Boxplot comparison of Age for Data Sets")
bpa2 <- boxplot(ds1_reg[,1], ds2_reg[,1], ds3_reg[,1], ds4_reg[,1], ds5_reg[,1], names = list_names, ylab=var1, main = "Boxplot comparison of Age for Data Sets (Only signed in users)")

#boxplots for impressions, question 1 and 2
bpi1 <- boxplot(ds1[,3],ds2[,3],ds3[,3],ds4[,3],ds5[,3], names = list_names, ylab=var2, main = "Boxplot comparison of Impressions for Data sets")
bpi2 <- boxplot(ds1[which(ds1[,1] != 0),3],ds2[which(ds2[,1] != 0),3],ds3[which(ds3[,1] != 0),3],ds4[which(ds4[,1] != 0),3],ds5[which(ds5[,1] != 0),3], names = list_names, ylab=var2, main = "Boxplot comparison of Impressions for Data Sets (only signed in users)")


#Histograms for age question 1
ha1 <- ggplot(ds1,aes(x=Age)) + geom_histogram(binwidth = 10, color="black", fill="lightgreen") + labs(y=name1)
ha2 <- ggplot(ds2,aes(x=Age)) + geom_histogram(binwidth = 10, color="black", fill="lightblue")+ labs(y=name2)
ha3 <- ggplot(ds3,aes(x=Age)) + geom_histogram(binwidth = 10, color="black", fill="tomato")+ labs(y=name3)
ha4 <- ggplot(ds4,aes(x=Age)) + geom_histogram(binwidth = 10, color="black", fill="orchid1")+ labs(y=name4)
ha5 <- ggplot(ds5,aes(x=Age)) + geom_histogram(binwidth = 10, color="black", fill="darkorange")+ labs(y=name5)
grid.arrange(ha1,ha2,ha3,ha4,ha5,nrow = 5)

#Histograms for age question2
har1 <- ggplot(ds1_reg,aes(x=Age)) + geom_histogram(binwidth = 10, color="black", fill="lightgreen") + labs(y=name1)
har2 <- ggplot(ds2_reg,aes(x=Age)) + geom_histogram(binwidth = 10, color="black", fill="lightblue") + labs(y=name2)
har3 <- ggplot(ds3_reg,aes(x=Age)) + geom_histogram(binwidth = 10, color="black", fill="tomato") + labs(y=name3)
har4 <- ggplot(ds4_reg,aes(x=Age)) + geom_histogram(binwidth = 10, color="black", fill="orchid1") + labs(y=name4)
har5 <- ggplot(ds5_reg,aes(x=Age)) + geom_histogram(binwidth = 10, color="black", fill="darkorange") + labs(y=name5)
grid.arrange(har1,har2,har3,har4,har5,nrow = 5)


#Histrograms for impressions question 1
hi1 <- ggplot(ds1,aes(x=Impressions)) + geom_histogram(binwidth = 1, color="black", fill="lightgreen") + labs(y=name1)
hi2 <- ggplot(ds2,aes(x=Impressions)) + geom_histogram(binwidth = 1, color="black", fill="lightblue") + labs(y=name2)
hi3 <- ggplot(ds3,aes(x=Impressions)) + geom_histogram(binwidth = 1, color="black", fill="tomato") + labs(y=name3)
hi4 <- ggplot(ds4,aes(x=Impressions)) + geom_histogram(binwidth = 1, color="black", fill="orchid1") + labs(y=name4)
hi5 <- ggplot(ds5,aes(x=Impressions)) + geom_histogram(binwidth = 1, color="black", fill="darkorange") + labs(y=name5)
grid.arrange(hi1,hi2,hi3,hi4,hi5,nrow=5)

#Histograms for impressions question 2
hir1 <- ggplot(ds1_reg,aes(x=Impressions)) + geom_histogram(binwidth = 1, color="black", fill="lightgreen") + labs(y=name1)
hir2 <- ggplot(ds2_reg,aes(x=Impressions)) + geom_histogram(binwidth = 1, color="black", fill="lightblue") + labs(y=name2)
hir3 <- ggplot(ds3_reg,aes(x=Impressions)) + geom_histogram(binwidth = 1, color="black", fill="tomato") + labs(y=name3)
hir4 <- ggplot(ds4_reg,aes(x=Impressions)) + geom_histogram(binwidth = 1, color="black", fill="orchid1") + labs(y=name4)
hir5 <- ggplot(ds5_reg,aes(x=Impressions)) + geom_histogram(binwidth = 1, color="black", fill="darkorange") + labs(y=name5)
grid.arrange(hir1,hir2,hir3,hir4,hir5,nrow = 5)

#ECDFs and qq for age question 1
ea1 <- ggplot(ds1, aes(Age)) + stat_ecdf(geom="step", color="lightgreen",size=2) + labs(y=name1)
ea2 <- ggplot(ds2, aes(Age)) + stat_ecdf(geom="step", color="lightblue",size=2) + labs(y=name2)
ea3 <- ggplot(ds3, aes(Age)) + stat_ecdf(geom="step", color="tomato",size=2) + labs(y=name3)
ea4 <- ggplot(ds4, aes(Age)) + stat_ecdf(geom="step", color="orchid1",size=2) + labs(y=name4)
ea5 <- ggplot(ds5, aes(Age)) + stat_ecdf(geom="step", color="darkorange",size=2) + labs(y=name5)
grid.arrange(ea1,ea2,ea3,ea4,ea5,nrow = 5)

qa1 <- ggplot(ds1, aes(sample = Age)) + stat_qq(geom="path",color="lightgreen",size=2) + labs(y=name1)
qa2 <- ggplot(ds2, aes(sample = Age)) + stat_qq(geom="path",color="lightblue",size=2) + labs(y=name2)
qa3 <- ggplot(ds3, aes(sample = Age)) + stat_qq(geom="path",color="tomato",size=2) + labs(y=name3)
qa4 <- ggplot(ds4, aes(sample = Age)) + stat_qq(geom="path",color="orchid1",size=2) + labs(y=name4)
qa5 <- ggplot(ds5, aes(sample = Age)) + stat_qq(geom="path",color="darkorange",size=2) + labs(y=name5)
grid.arrange(qa1,qa2,qa3,qa4,qa5,nrow=5)


#ECDFs for impressions question 1
ei1 <- ggplot(ds1, aes(Impressions)) + stat_ecdf(geom="step", color="lightgreen",size=2) + labs(y=name1)
ei2 <- ggplot(ds2, aes(Impressions)) + stat_ecdf(geom="step", color="lightblue",size=2) + labs(y=name2)
ei3 <- ggplot(ds3, aes(Impressions)) + stat_ecdf(geom="step", color="tomato",size=2) + labs(y=name3)
ei4 <- ggplot(ds4, aes(Impressions)) + stat_ecdf(geom="step", color="orchid1",size=2) + labs(y=name4)
ei5 <- ggplot(ds5, aes(Impressions)) + stat_ecdf(geom="step", color="darkorange",size=2) + labs(y=name5)
grid.arrange(ei1,ei2,ei3,ei4,ei5,nrow = 5)

qi1 <- ggplot(ds1, aes(sample = Impressions)) + stat_qq(geom="path",color="lightgreen",size=2) + labs(y=name1)
qi2 <- ggplot(ds2, aes(sample = Impressions)) + stat_qq(geom="path",color="lightblue",size=2) + labs(y=name2)
qi3 <- ggplot(ds3, aes(sample = Impressions)) + stat_qq(geom="path",color="tomato",size=2) + labs(y=name3)
qi4 <- ggplot(ds4, aes(sample = Impressions)) + stat_qq(geom="path",color="orchid1",size=2) + labs(y=name4)
qi5 <- ggplot(ds5, aes(sample = Impressions)) + stat_qq(geom="path",color="darkorange",size=2) + labs(y=name5)
grid.arrange(qi1,qi2,qi3,qi4,qi5,nrow=5)


#ECDFs and qq for age question 2
ear1 <- ggplot(ds1_reg, aes(Age)) + stat_ecdf(geom="step", color="lightgreen",size=2) + labs(y=name1)
ear2 <- ggplot(ds2_reg, aes(Age)) + stat_ecdf(geom="step", color="lightblue",size=2) + labs(y=name2)
ear3 <- ggplot(ds3_reg, aes(Age)) + stat_ecdf(geom="step", color="tomato",size=2) + labs(y=name3)
ear4 <- ggplot(ds4_reg, aes(Age)) + stat_ecdf(geom="step", color="orchid1",size=2) + labs(y=name4)
ear5 <- ggplot(ds5_reg, aes(Age)) + stat_ecdf(geom="step", color="darkorange",size=2) + labs(y=name5)
grid.arrange(ear1,ear2,ear3,ear4,ear5,nrow = 5)

qar1 <- ggplot(ds1_reg, aes(sample = Age)) + stat_qq(geom="path",color="lightgreen",size=2) + labs(y=name1)
qar2 <- ggplot(ds2_reg, aes(sample = Age)) + stat_qq(geom="path",color="lightblue",size=2) + labs(y=name2)
qar3 <- ggplot(ds3_reg, aes(sample = Age)) + stat_qq(geom="path",color="tomato",size=2) + labs(y=name3)
qar4 <- ggplot(ds4_reg, aes(sample = Age)) + stat_qq(geom="path",color="orchid1",size=2) + labs(y=name4)
qar5 <- ggplot(ds5_reg, aes(sample = Age)) + stat_qq(geom="path",color="darkorange",size=2) + labs(y=name5)
grid.arrange(qar1,qar2,qar3,qar4,qar5,nrow=5)


#ECDFs for impressions question 2
eir1 <- ggplot(ds1_reg, aes(Impressions)) + stat_ecdf(geom="step", color="lightgreen",size=2) + labs(y=name1)
eir2 <- ggplot(ds2_reg, aes(Impressions)) + stat_ecdf(geom="step", color="lightblue",size=2) + labs(y=name2)
eir3 <- ggplot(ds3_reg, aes(Impressions)) + stat_ecdf(geom="step", color="tomato",size=2) + labs(y=name3)
eir4 <- ggplot(ds4_reg, aes(Impressions)) + stat_ecdf(geom="step", color="orchid1",size=2) + labs(y=name4)
eir5 <- ggplot(ds5_reg, aes(Impressions)) + stat_ecdf(geom="step", color="darkorange",size=2) + labs(y=name5)
grid.arrange(eir1,eir2,eir3,eir4,eir5,nrow = 5)

qir1 <- ggplot(ds1_reg, aes(sample = Impressions)) + stat_qq(geom="path",color="lightgreen",size=2) + labs(y=name1)
qir2 <- ggplot(ds2_reg, aes(sample = Impressions)) + stat_qq(geom="path",color="lightblue",size=2) + labs(y=name2)
qir3 <- ggplot(ds3_reg, aes(sample = Impressions)) + stat_qq(geom="path",color="tomato",size=2) + labs(y=name3)
qir4 <- ggplot(ds4_reg, aes(sample = Impressions)) + stat_qq(geom="path",color="orchid1",size=2) + labs(y=name4)
qir5 <- ggplot(ds5_reg, aes(sample = Impressions)) + stat_qq(geom="path",color="darkorange",size=2) + labs(y=name5)
grid.arrange(qir1,qir2,qir3,qir4,qir5,nrow=5)

set.seed(443392)
#Random sample of 5000 points from datasets for shapiro wilkes test
samp1 <- ceiling(runif(5000, min = 0, max = nrow(ds1)))
samp2 <- ceiling(runif(5000, min = 0, max = nrow(ds2)))
samp3 <- ceiling(runif(5000, min = 0, max = nrow(ds3)))
samp4 <- ceiling(runif(5000, min = 0, max = nrow(ds4)))
samp5 <- ceiling(runif(5000, min = 0, max = nrow(ds5)))

samp_reg1 <- ceiling(runif(5000, min = 0, max = nrow(ds1_reg)))
samp_reg2 <- ceiling(runif(5000, min = 0, max = nrow(ds2_reg)))
samp_reg3 <- ceiling(runif(5000, min = 0, max = nrow(ds3_reg)))
samp_reg4 <- ceiling(runif(5000, min = 0, max = nrow(ds4_reg)))
samp_reg5 <- ceiling(runif(5000, min = 0, max = nrow(ds5_reg)))

library(goft)
weibull_test(ds1[samp1,3])

#Shapiro Test for Age
shapiro.test(ds1[samp1,1])
shapiro.test(ds2[samp2,1])
shapiro.test(ds3[samp3,1])
shapiro.test(ds4[samp4,1])
shapiro.test(ds5[samp5,1])

#Shapiro Test for Impressions
shapiro.test(ds1[samp1,3])
shapiro.test(ds2[samp2,3])
shapiro.test(ds3[samp3,3])
shapiro.test(ds4[samp4,3])
shapiro.test(ds5[samp5,3])

#Shapiro Test for Age (without non-registered users)
shapiro.test(ds1_reg[samp_reg1,1])
shapiro.test(ds1_reg[samp_reg2,1])
shapiro.test(ds1_reg[samp_reg3,1])
shapiro.test(ds1_reg[samp_reg4,1])
shapiro.test(ds1_reg[samp_reg5,1])

#Shapiro Test for Impressions (without non-registered users)
shapiro.test(ds1_reg[samp_reg1,3])
shapiro.test(ds2_reg[samp_reg2,3])
shapiro.test(ds3_reg[samp_reg3,3])
shapiro.test(ds4_reg[samp_reg4,3])
shapiro.test(ds5_reg[samp_reg5,3])


