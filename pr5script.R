# Q1
height <- c(151, 174, 138, 186, 128, 136, 179, 163, 152)
weight <- c(63, 81, 56, 91, 47, 57, 76, 72, 62)
data <- data.frame(height, weight)
# Create linear regression model
model <- lm(weight ~ height, data = data)
summary(model)
# Predict weight for height = 140
new_height <- data.frame(height = 140)
predicted_weight <- predict(model, new_height)
print(paste("Predicted Weight for height 140 cm:", predicted_weight))
# Plot
plot(data$height, data$weight, main = "Height vs Weight", 
     xlab = "Height (cm)", ylab = "Weight (kg)", pch = 16, col = "blue")
abline(model, col = "red", lwd = 2)
boxplot(data)
qqnorm(model$residuals, main = "Q-Q Plot of Residuals")
qqline(model$residuals, col = "red")
plot(fitted(model), residuals(model), main = "Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals", pch = 16, col = "purple")
abline(h = 0, col = "grey", lwd = 2)

#Iris Ques
data(iris)
model <- lm(Sepal.Length ~ Sepal.Width, data = iris)
summary(model)
new_data <- data.frame(Sepal.Width = c(4.0))
predicted_sepal_length <- predict(model, new_data)
print(predicted_sepal_length)
plot(iris$Sepal.Width, iris$Sepal.Length, main = "Sepal Width vs Sepal Length",
     xlab = "Sepal Width", ylab = "Sepal Length", pch = 16, col = "blue")
abline(model, col = "red")
plot(model$fitted.values, model$residuals, main = "Residuals vs Fitted Model",
     xlab = "Fitted Values", ylab = "Residuals", pch = 16, col = "red")
abline(h = 0, col = "blue")
qqnorm(model$residuals, main = "Q-Q Plot")
qqline(model$residuals, col = "red")
boxplot(iris$Sepal.Length, iris$Sepal.Width, 
        names = c("Sepal Length", "Sepal Width"), 
        main = "Box Plot of Sepal Length and Sepal Width", 
        ylab = "Values", 
        col = c("lightblue", "lightgreen"))


#Last Ques
library(ggplot2)
heart_data <- read.csv(file.choose(), header = TRUE)

model <- lm(heart_disease ~ biking + smoking, data = heart_data)
summary(model)

# Predict heart disease
new_data <- data.frame(biking = 50, smoking = 10)
predicted_heart_disease <- predict(model, newdata = new_data)
cat("Predicted Heart Disease: ", predicted_heart_disease, "\n")

# Biking vs Heart Disease Plot
ggplot(heart_data, aes(x = biking, y = heart_disease)) +
  geom_point(aes(color = heart_disease), size = 3) +
  labs(title = "Scatter Plot: Biking vs Heart Disease", x = "Biking", y = "Heart Disease") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal()

# Smoking vs Heart Disease Plot
ggplot(heart_data, aes(x = smoking, y = heart_disease)) +
  geom_point(aes(color = heart_disease), size = 3) +
  labs(title = "Scatter Plot: Smoking vs Heart Disease", x = "Smoking", y = "Heart Disease") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal()

# Boxplot for Heart Disease by Biking Level
boxplot(
  heart_data$heart_disease ~ cut(heart_data$biking, breaks = 4), 
  main = "Box Plot: Heart Disease by Biking Levels",
  xlab = "Biking Levels",
  ylab = "Heart Disease",
  col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),
  names = c("Low", "Medium", "High", "Very High") 
)

# Boxplot for Heart Disease by Smoking Level
boxplot(
  heart_data$heart_disease ~ cut(heart_data$smoking, breaks = 4),
  main = "Box Plot: Heart Disease by Smoking Levels",
  xlab = "Smoking Levels",
  ylab = "Heart Disease",
  col = c("lightblue", "lightgreen", "lightpink", "lightyellow"), 
  names = c("Low", "Medium", "High", "Very High")
)

# Q-Q Plot for Residuals
qqnorm(model$residuals, main = "QQ Plot of Residuals")
qqline(model$residuals, col = "red")

# Residuals vs Fitted Plot
ggplot(data.frame(Fitted = model$fitted.values, Residuals = model$residuals), aes(x = Fitted, y = Residuals)) +
  geom_point(color = "darkgreen") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Q7: Linear Regression to predict performance index based on skills
index_data <- read.csv(file.choose(), header = TRUE)
model <- lm(index ~ written + language + tech + gk, data = index_data)
summary(model)

# Predict performance index
new_data <- data.frame(written = 48, language = 67, tech = 56, gk = 54)
predicted_index <- predict(model, newdata = new_data)
cat("Predicted Performance Index: ", predicted_index, "\n")

# Pairwise Scatter Plot Matrix
pairs(~index + written + language + tech + gk, data = index_data, main = "Pairwise Scatter Plot Matrix")

# Box Plots for Skills
ggplot(index_data, aes(x = factor(1), y = written)) +
  geom_boxplot() +
  labs(title = "Box Plot: Written Skills", y = "Written Skills") +
  theme_minimal()

ggplot(index_data, aes(x = factor(1), y = language)) +
  geom_boxplot() +
  labs(title = "Box Plot: Language Skills", y = "Language Skills") +
  theme_minimal()

ggplot(index_data, aes(x = factor(1), y = tech)) +
  geom_boxplot() +
  labs(title = "Box Plot: Technical Knowledge", y = "Technical Knowledge") +
  theme_minimal()

ggplot(index_data, aes(x = factor(1), y = gk)) +
  geom_boxplot() +
  labs(title = "Box Plot: General Knowledge", y = "General Knowledge") +
  theme_minimal()

# Q-Q Plot for Residuals
qqnorm(model$residuals, main = "QQ Plot of Residuals")
qqline(model$residuals, col = "red")

# Residuals vs Fitted Plot
ggplot(data.frame(Fitted = model$fitted.values, Residuals = model$residuals), aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
