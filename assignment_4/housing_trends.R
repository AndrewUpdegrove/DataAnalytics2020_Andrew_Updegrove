require(readxl)
require(ggplot2)
require(dplyr)
require(GGally)
require(rpart)
require(rpart.plot)
require(C50)

data_raw = read_excel(file.choose(), skip = 4, col_names = TRUE)
unique(data_raw$NEIGHBORHOOD)
brooklyn_data = data_raw
brooklyn_data$BOROUGH <- NULL
brooklyn_data$`EASE-MENT` <- NULL
names(brooklyn_data)[18] <- "SALE PRICE"
names(brooklyn_data)[8] <- "APARTMENT NUMBER"


sale_sponged = filter(brooklyn_data, `GROSS SQUARE FEET` > 0 & `SALE PRICE` > 0 & `GROSS SQUARE FEET` < 2000000)
#zip_sponged = filter(brooklyn_data, `ZIP CODE` > 0 & `TAX CLASS AT PRESENT` > 0)
data_sponged = filter(brooklyn_data, `GROSS SQUARE FEET` > 0 & `SALE PRICE` > 0 & `ZIP CODE` > 0 & !is.na(`TAX CLASS AT PRESENT`))


#ggpairs(data=brooklyn_data, columns = c(9,14,16,18), title="Correlate")


#histogram for year built with 10 year bins
year_hist = ggplot(filter(brooklyn_data, `YEAR BUILT` > 0), aes(x = `YEAR BUILT`)) + geom_histogram(binwidth = 10) + labs(title = "Distribution of the years properties in Brookly were built")


#histogram for Gross Square Footage
gross_sqft_hist = ggplot(filter(brooklyn_data, `GROSS SQUARE FEET` > 0 & `GROSS SQUARE FEET` < 10000), aes(x = `GROSS SQUARE FEET`)) + geom_histogram() + labs(title = "Distribution of the Gross Square Footage of properties in Brooklyn", subtitle = "Not inlcuding properties over 10000 sqft")


lin_fit_sale <- lm(`SALE PRICE` ~ `GROSS SQUARE FEET`, data = sale_sponged)

ggplot(data=sale_sponged, aes(x = `GROSS SQUARE FEET`, y = `SALE PRICE`)) + geom_point(stat="identity") + geom_abline()


set.seed(6654)

grn <- runif(nrow(data_sponged))
zip_rand <- data_sponged[order(grn),]
zip_rand = as.data.frame(zip_rand)
train = floor(nrow(data_sponged)*.8)
zip_rand$`ZIP CODE` <- as.factor(zip_rand$`ZIP CODE`)
classmodel <- C5.0(x = zip_rand[1:train,c(14,18,16)], y = zip_rand[1:train,9])
summary(classmodel)

zip_predict <- predict(classmodel, zip_rand[train:nrow(zip_rand),c(9,14,18,16)])
summary(zip_predict)

table(zip_rand[train:nrow(zip_rand),9],Predicted= zip_predict)

accuracy = sum(diag(table(zip_rand[train:nrow(zip_rand),9],Predicted= zip_predict))) / (nrow(zip_rand)-train)

