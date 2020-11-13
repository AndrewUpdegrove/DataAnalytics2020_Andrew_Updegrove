

wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
names(wine_data) <- c("Class", "Alcohol", "Malic_Acid", "Ash", "Alcalinity_of_Ash", "Magnesium", "Total_Phenols", "Flavanoids", "Nonflavanoid_Phenols", "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wines", "Proline")
head(wine_data)

#See correlation of values using heatmap
heatmap(cor(wine_data), Rowv = NA, Colv = NA)


cultivar_classes <- factor(wine_data$Class)
wine_data_PCA <- prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)
