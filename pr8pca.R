# Q1) PCA and Principal Component Regression (PCR) on iris dataset

# Load the iris dataset
data("iris")
head(iris)  # View the first few rows
summary(iris)  # Summary statistics

# Perform PCA on the numeric columns (removes Species)
mypr <- prcomp(iris[,-5], scale = TRUE)

# PCA summary
summary(mypr)

# Visualizing original vs scaled data
plot(iris$Sepal.Length, iris$Sepal.Width, main = "Raw Plot")
plot(scale(iris$Sepal.Length), scale(iris$Sepal.Width), main = "Scaled Plot")

# Scree plot (Elbow method)
plot(mypr, type = "l")

# Biplot for the first two principal components
biplot(mypr, scale = 0)

# Extract PCA scores
head(mypr$x)

# Combine original iris data with first two principal components
iris2 <- cbind(iris, mypr$x[, 1:2])
head(iris2)

# Correlation between original variables and first two PCs
cor(iris[,-5], iris2[, 6:7])

# Perform Principal Component Regression (PCR) to predict Sepal.Length
install.packages("pls")
library(pls)
pcmodel <- pcr(Sepal.Length ~ Species + Sepal.Width + Petal.Length + Petal.Width, ncomp = 3, data = iris, scale = TRUE)

# Predict Sepal.Length for all rows in the iris dataset
iris$pred <- predict(pcmodel, iris, ncomp = 2)
head(iris)

# Q2) PCA and Principal Component Regression (PCR) on mtcars dataset

# Load the mtcars dataset
data(mtcars)
head(mtcars)  # View the first few rows
summary(mtcars)  # Summary statistics

# Perform PCA on mtcars data
mypr_mtcars <- prcomp(mtcars, scale = TRUE)

# PCA summary
summary(mypr_mtcars)

# Scree plot (Elbow method)
plot(mypr_mtcars, type = "l")

# Biplot for the first two principal components
biplot(mypr_mtcars, scale = 0)

# Extract PCA scores
head(mypr_mtcars$x)

# Combine mtcars data with first two principal components
mtcars2 <- cbind(mtcars, mypr_mtcars$x[, 1:2])
head(mtcars2)

# Correlation between original variables and first two PCs
cor(mtcars[, -1], mtcars2[, 12:13])

# Perform Principal Component Regression (PCR) to predict mpg (fuel efficiency)
pcmodel_mtcars <- pcr(mpg ~ ., ncomp = 3, data = mtcars, scale = TRUE)

# Predict mpg for all rows in the mtcars dataset
mtcars$pred <- predict(pcmodel_mtcars, mtcars, ncomp = 2)
head(mtcars)

# Q3) PCA and Principal Component Regression (PCR) on USArrests dataset

# Load the USArrests dataset
data("USArrests")
head(USArrests)  # View the first few rows
summary(USArrests)  # Summary statistics

# Perform PCA on the USArrests data
mypr_usarrests <- prcomp(USArrests, scale = TRUE)

# PCA summary
summary(mypr_usarrests)

# Scree plot (Elbow method)
plot(mypr_usarrests, type = "l")

# Biplot for the first two principal components
biplot(mypr_usarrests, scale = 0)

# Extract PCA scores
head(mypr_usarrests$x)

# Combine original USArrests data with first two principal components
usarrests2 <- cbind(USArrests, mypr_usarrests$x[, 1:2])
head(usarrests2)

# Correlation between original variables and first two PCs
cor(USArrests, usarrests2[, 5:6])

# Perform Principal Component Regression (PCR) to predict Murder
pcmodel_usarrests <- pcr(Murder ~ ., ncomp = 3, data = USArrests, scale = TRUE)

# Predict Murder for all rows in the USArrests dataset
USArrests$pred <- predict(pcmodel_usarrests, USArrests, ncomp = 3)
head(USArrests)
