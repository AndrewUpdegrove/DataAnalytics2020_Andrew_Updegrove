require(dplyr)
require(ggplot2)
require(broom)
require(tidyr)
require(randomForest)

setwd(choose.dir())

palm_oil1 <- read.csv("data/palm_oil_90-94.csv", header = T, sep = ",")
palm_oil2 <- read.csv("data/palm_oil_95-99.csv", header = T, sep = ",")
palm_oil3 <- read.csv("data/palm_oil_00-04.csv", header = T, sep = ",")
palm_oil4 <- read.csv("data/palm_oil_05-09.csv", header = T, sep = ",")
palm_oil5 <- read.csv("data/palm_oil_10-14.csv", header = T, sep = ",")
palm_oil6 <- read.csv("data/palm_oil_15-19.csv", header = T, sep = ",")
palm_oil <- rbind(palm_oil1,palm_oil2,palm_oil3,palm_oil4,palm_oil5,palm_oil6)

primary_forest <- read.csv("data/primary_forest.csv", header = T, sep = ",")
planted_forest <- read.csv("data/planted_forest.csv", header = T, sep = ",")
GDP <- read.csv("data/GDP_ppp.csv", header = T, sep = ",")
country_codes <- read.csv("data/country_codes.csv", header = T, sep = ",")


# Namibia's country code is NA which results in missing value
country_codes[which(country_codes$Name == "Namibia"),2] <- "NB"


# removing metadata from and cleaning primary_forest
#------------------------------------------------------------------------------
names(primary_forest)[1] <- "Country" # changing column name to consistent value for later
primary_forest <- filter(primary_forest, Value.Footnotes != 'A') # getting rid of regional data
primary_forest$Element <- NULL # forest is measured in area
primary_forest$Unit <- NULL # units are in 1000's of hectacres
primary_forest$Value.Footnotes <- NULL # footnotes are all manual estimation, expert sources, or official reporting
primary_forest$Code <- NA
for(sub in unique(primary_forest$Country)){
  index <- which(country_codes$Name == sub)
  if(length(index) == 0){
    print(paste("No matching country found for", sub))
    new_code <- readline(prompt = "What 2 letter code would you like to use?")
  } else if(length(index) == 1){
    new_code = country_codes[index,2]
  } else {
    print("More than one match found???")
    print(paste("Your choices are"), sub)
    new_code <- readline(prompt = "What 2 letter code would you like to use?")
  }
  primary_forest[which(primary_forest$Country == sub),]$Code <- new_code
}

primary_forest <- primary_forest[which(primary_forest$Code != "IGN"),]
primary_forest$Country <- NULL
names(primary_forest)[2] <- "primary"

# removing metadata from and cleaning planted_forest
#------------------------------------------------------------------------------
names(planted_forest)[1] <- "Country"
planted_forest <- filter(planted_forest, Value.Footnotes != 'A')
planted_forest$Element <- NULL # forest is measured in area
planted_forest$Unit <- NULL # units are in 1000's of hectacres
planted_forest$Value.Footnotes <- NULL # footnotes are all manual estimation, expert sources, or official reporting
planted_forest$Code <- NA
for(sub in unique(planted_forest$Country)){
  index <- which(country_codes$Name == sub)
  if(length(index) == 0){
    print(paste("No matching country found for", sub))
    new_code <- readline(prompt = "What 2 letter code would you like to use?")
  } else if(length(index) == 1){
    new_code = country_codes[index,2]
  } else {
    print("More than one match found???")
    print(paste("Your choices are"), sub)
    new_code <- readline(prompt = "What 2 letter code would you like to use?")
  }
  planted_forest[which(planted_forest$Country == sub),]$Code <- new_code
}

planted_forest <- planted_forest[which(planted_forest$Code != "IGN"),]
planted_forest$Country <- NULL
names(planted_forest)[2] <- "planted"

# removing metadata from and cleaning palm_oil
palm_oil[,c(-2, -8, -10,-30, -32, -35)] <- NULL
names(palm_oil)[3] <- c("Country")
palm_oil$Code <- NA
for(sub in unique(palm_oil$Country)){
  index <- which(country_codes$Name == sub)
  if(length(index) == 0){
    print(paste("No matching country found for", sub))
    new_code <- readline(prompt = "What 2 letter code would you like to use?")
  } else if(length(index) == 1){
    new_code = country_codes[index,2]
  } else {
    print("More than one match found???")
    print(paste("Your choices are"), sub)
    new_code <- readline(prompt = "What 2 letter code would you like to use?")
  }
  palm_oil[which(palm_oil$Country == sub),]$Code <- new_code
}

palm_oil <- palm_oil[which(palm_oil$Code != "IGN"),]
palm_oil$Country <- NULL #drop the country
palm_oil$Flag <- NULL

# removing metadata from and cleaning GDP
#------------------------------------------------------------------------------
names(GDP)[1] <- "Country"
GDP$Code <- NA
for(sub in unique(GDP$Country)){
  index <- which(country_codes$Name == sub)
  if(length(index) == 0){
    print(paste("No matching country found for", sub))
    new_code <- readline(prompt = "What 2 letter code would you like to use?")
  } else if(length(index) == 1){
    new_code = country_codes[index,2]
  } else {
    print("More than one match found???")
    print(paste("Your choices are"), sub)
    new_code <- readline(prompt = "What 2 letter code would you like to use?")
  }
  GDP[which(GDP$Country == sub),]$Code <- new_code
}
GDP <- GDP[which(GDP$Code != "IGN"),]
GDP$Country <- NULL
GDP$Value.Footnotes <- NULL
names(GDP)[2] <- "GDP"




# need to outer join data by country and year
combined1 <- merge(planted_forest, primary_forest, by = c("Code", "Year"), all = TRUE)
combined2 <- merge(GDP, palm_oil, by = c("Code", "Year"))
combined2$palm_oil_normalized <- combined2$Trade.Value..US.. / combined2$GDP
combined2$Year <- as.integer(combined2$Year)
final <- merge(combined1, combined2, by = c("Code", "Year"), all = TRUE)




# ------------------------------------------------------------------------------
# Visualizations
# ------------------------------------------------------------------------------

filter(palm_oil, Code == "US" & Trade.Flow == "Export") %>% ggplot(aes(x = Year, y = Trade.Value..US..)) + 
  geom_point() + 
  labs(x = "Year", y = "Trade Value in USD", title = "United States Palm Oil Exports")
filter(palm_oil, Code == "RS" & Trade.Flow == "Export") %>% ggplot(aes(x = Year, y = Trade.Value..US..)) + 
  geom_point() +
  labs(x = "Year", y = "Trade Value in USD", title = "Serbia Palm Oil Exports")

filter(combined2, Trade.Flow == "Export" & Year == 2016) %>% select(Trade.Value..US..) %>% ggplot(aes(x = Trade.Value..US..)) +
  geom_histogram() +
  labs(x = "Export Value in USD", y = "Frequency", title = "World Palm Oil Exports by country in 2016")


filter(regression_collection, slope < 10000 & slope > -10000) %>% ggplot(aes(x = slope, y = curvature)) + geom_point() 

test = lm(palm_oil_normalized ~ Year, data = filter(combined2, Trade.Flow == "Export" & Code == "AM"))
plot(Trade.Value..US.. ~ Year, data = filter(combined2, Trade.Flow == "Export" & Code == "US"))
abline(test)
filter(combined2, Trade.Flow == "Export" & Code == "CD") %>% ggplot(aes(x = Year, y = Trade.Value..US..)) + geom_point()

ggplot(plot.data, mapping = aes(x = Year, y = log10(Trade.Value..US..+1))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE, fill = NA, formula = y ~ poly(x, 2), colour = "green") +
  labs(y = "Trade Value in USD", title = " US Palm Oil Exports with Second Order Polynomial Fit")


filter(primary_forest, Code == "BO") %>% ggplot(mapping = aes(x = Year, y = primary)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE, fill = NA, formula = y ~ x, colour = "green") +
  labs(y = "Hectacres of Primary Forest Land", title = "Primary Forest Land from 1990 - 2017")

# ------------------------------------------------------------------------------
# Kmeans Clustering with transform before
# ------------------------------------------------------------------------------
set.seed(12142020)
log_regress <- data.frame()
for(country in unique(combined2$Code)){
  dat <- filter(combined2, Trade.Flow == "Export" & Code == country)
  if(nrow(dat) > 2){
    model = lm(log10(Trade.Value..US..) ~ poly(Year, 2), data = dat)
    log_regress <- rbind(log_regress, c(country, model$coefficients))
  }
}

names(log_regress) <- c("Code", "intercept", "slope", "curvature")
log_regress$intercept <- as.numeric(log_regress$intercept)
log_regress$slope <- as.numeric(log_regress$slope)
log_regress$curvature <- as.numeric(log_regress$curvature)

ggplot(data = log_regress, mapping = aes(x = slope, y = curvature)) + geom_point() +
  labs(title = "Second Order Polynomial Fit Plot for all Countries")

withinss <- c()
set <- 1:20
collection <- vector(mode = "list", length = length(set))
for(i in set){
  cutsie <- kmeans(log_regress[,3:4], i, iter.max = 20, algorithm = "Lloyd")
  withinss <- c(withinss, cutsie$tot.withinss)
  collection[[i]] = cutsie
}
ggplot(mapping = aes(x = set, y = withinss)) + geom_line() +
  labs(x = "Centers", y = "Total within-cluster sum of squares", title = "Elbow Method for Picking Number of Cluster Centers")

final <- collection[[5]] # value in double brackets is the best model
ggplot(data = log_regress, aes(x=slope, y = curvature)) + 
  geom_point(color = final$cluster) + 
  geom_point(data = as.data.frame(final$centers), aes(x=slope, y=curvature), color = "purple", pch = c("1","2", "3", "4", "5"), size = 4) +
  labs(y = "Squared Coefficient", x = "Linear Coefficient", title = "Clustering on Second Order Polynomial Fit", color = "group")


get_named_clusters <- log_regress
get_named_clusters$group <- final$cluster
yes <- merge(get_named_clusters, country_codes, by = "Code")
filter(yes, group == 1) %>% select(Name)
# group 1 is black, group 3 is green, group 2 is red, group 4 is darker blue, group 5 is light blue


# ------------------------------------------------------------------------------
# Kmeans Clustering with transform after
# ------------------------------------------------------------------------------
# linear version won't work because it always results in a line
regression_collection <- data.frame()
for(country in unique(combined2$Code)){
  dat <- filter(combined2, Trade.Flow == "Export" & Code == country)
  if(nrow(dat) > 2){
    model = lm(Trade.Value..US.. ~ poly(Year, 2), data = dat)
    regression_collection <- rbind(regression_collection, c(country, model$coefficients))
  }
}
names(regression_collection) <- c("Code", "intercept", "slope", "curvature")
regression_collection$intercept <- as.numeric(regression_collection$intercept)
regression_collection$slope <- as.numeric(regression_collection$slope)
regression_collection$curvature <- as.numeric(regression_collection$curvature)

transformed <- regression_collection
# ruining things
for(i in 1:nrow(transformed)){
  if(transformed[i,3] < 0){
    transformed[i,3] <- -1 * (1/transformed[i,3])
  }
  if(transformed[i,4] < 0){
    transformed[i,4] <- -1 * (1/transformed[i,4])
  }
  transformed[i,3:4] <- log10(transformed[i,3:4])
}

clustering <- kmeans(transformed[,3:4], 4, iter.max = 15, algorithm = "Lloyd")

ggplot(data = transformed, aes(x=slope, y = curvature)) + 
  geom_point(color = clustering$cluster) + 
  geom_point(data = as.data.frame(clustering$centers), aes(x=slope, y=curvature), color = "purple", shape = 18, size = 4) +
  labs(y = "Squared Coefficient", x = "Linear Coefficient", title = "Logarithmic Visualization of Second-Order Polynomial Fit of Palm Oil Exports")
transformed[which(clustering$cluster == 3),]

# ------------------------------------------------------------------------------
# Random Forest
# ------------------------------------------------------------------------------
# get the trends on whether the primary forest are shrinking in a country
primary_regression <- data.frame()
for(country in unique(primary_forest$Code)){
  dat <- filter(primary_forest, Code == country)
  if(nrow(dat) > 1){
    model = lm(primary ~ Year, data = dat)
    primary_regression <- rbind(primary_regression, c(country, model$coefficients))
  }
}
primary_regression$X.0. <- as.numeric(primary_regression$X.0.)
primary_regression$X.0..1 <- as.numeric(primary_regression$X.0..1)

# pivot the data to a long wise format so that each year of data is a parameter in randomForest
palm_oil_pivoted <- palm_oil %>% filter(Trade.Flow == "Export") %>% select(Year, Trade.Value..US.., Code) %>% pivot_wider(names_from = Year, values_from = Trade.Value..US..)
short.data <- palm_oil_pivoted[rowSums(is.na(palm_oil_pivoted[,15:31])) < 5,] %>% select(Code,"2000":"2016") %>% arrange(Code)

#  if the slope is significant (no slope gets recorded as -1e_15)
primary_classification <- data.frame()
for(i in 1:nrow(primary_regression)){
  if(primary_regression[i,3] < -.00001){
    holder = 1
  } else {
    holder = -1
  }
  primary_classification <- rbind(primary_classification, c(primary_regression[i,1], holder))
}
names(primary_classification) <- c("Code", "class") # relabel columns
primary_classification$class <- as.factor(primary_classification$class) # convert classification to a factor
short.class <- filter(primary_classification, Code %in% short.data$Code) %>% arrange(Code) # use only ones we have data and then put it alphabetical order


# separate into training and testing data
set.seed(1111292020)
rf.indices <- sample(nrow(short.data), ceiling(.8*nrow(short.data)))

rf.train.x <- short.data[rf.indices,-1]
rf.test.x <- short.data[-rf.indices,-1]

rf.train.y <- short.class[rf.indices,-1]
rf.test.y <- short.class[-rf.indices, -1]

rf.model <- randomForest(na.roughfix(rf.train.x), rf.train.y) # pass into random forest a repaired matrix

# use entire data set to repair the test set, and then predict on it
rf.predictions <- predict(rf.model, na.roughfix(short.data[,-1])[-rf.indices,])
table(rf.predictions, rf.test.y)

importance = importance(rf.model)
varImportance.gini = data.frame(Variables = row.names(importance),
                                Importance =round(importance[, "MeanDecreaseGini"],2))
rankImportance.gini=varImportance.gini%>%mutate(Rank=paste('#',dense_rank(desc(Importance))))
ggplot(rankImportance.gini,aes(x=reorder(Variables,Importance), y=Importance,fill=Importance)) + 
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust=0, vjust=0.55, size = 4, colour = 'white') +
  labs(x = 'Variables') +
  coord_flip() +
  theme_classic() + 
  theme(axis.text.y = element_text(size=13, face = "bold"))



# ------------------------------------------------------------------------------
# Random Forest but with planted forest
# ------------------------------------------------------------------------------
# get the trends on whether the primary forest are shrinking in a country
planted_regression <- data.frame()
for(country in unique(planted_forest$Code)){
  dat <- filter(planted_forest, Code == country)
  if(nrow(dat) > 1){
    model = lm(planted ~ Year, data = dat)
    planted_regression <- rbind(planted_regression, c(country, model$coefficients))
  }
}
planted_regression$X.0. <- as.numeric(planted_regression$X.0.)
planted_regression$X.0..1 <- as.numeric(planted_regression$X.0..1)


# if the slope is significant (no slope gets recorded as -1e_15)
# 1 means the forest area is decreasing, -1 means no change or increase 
planted_classification <- data.frame()
for(i in 1:nrow(planted_regression)){
  if(planted_regression[i,3] < -.00001){
    holder = 1
  } else {
    holder = -1
  }
  planted_classification <- rbind(planted_classification, c(planted_regression[i,1], holder))
}
names(planted_classification) <- c("Code", "class") # relabel columns
planted_classification$class <- as.factor(planted_classification$class) # convert classification to a factor
planted.class <- filter(planted_classification, Code %in% short.data$Code) %>% arrange(Code) # use only ones we have data and then put it alphabetical order


# separate into training and testing data
set.seed(1111292020)
rf2.indices <- sample(nrow(short.data), ceiling(.8*nrow(short.data)))

rf2.train.x <- short.data[rf2.indices,-1]
rf2.test.x <- short.data[-rf2.indices,-1]

rf2.train.y <- planted.class[rf2.indices,-1]
rf2.test.y <- planted.class[-rf2.indices, -1]

rf2.model <- randomForest(na.roughfix(rf2.train.x), rf2.train.y) # pass into random forest a repaired matrix

# use entire data set to repair the test set, and then predict on it
rf2.predictions <- predict(rf2.model, na.roughfix(short.data[,-1])[-rf2.indices,])
table(rf2.test.y,rf2.predictions)

importance = importance(rf2.model)
varImportance.gini = data.frame(Variables = row.names(importance),
                                Importance =round(importance[, "MeanDecreaseGini"],2))
rankImportance.gini=varImportance.gini%>%mutate(Rank=paste('#',dense_rank(desc(Importance))))
ggplot(rankImportance.gini,aes(x=reorder(Variables,Importance), y=Importance,fill=Importance)) + 
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust=0, vjust=0.55, size = 4, colour = 'white') +
  labs(x = 'Variables') +
  coord_flip() +
  theme_classic() + 
  theme(axis.text.y = element_text(size=13, face = "bold"))
