# Load necessary libraries
install.packages(c("rpart", "tree", "party", "caret", "e1071"))
library(rpart)
library(tree)
library(party)
library(caret)
library(e1071)

# Load mtcars dataset and convert 'cyl' to a factor
mydata <- data.frame(mtcars)
mydata$cyl <- as.factor(mydata$cyl)  # Convert 'cyl' to categorical

# Split the data into training (70%) and testing (30%) sets
set.seed(123)
train_index <- createDataPartition(mydata$cyl, p = 0.7, list = FALSE)
train_data <- mydata[train_index, ]
test_data <- mydata[-train_index, ]

# 1. Build Decision Tree Models

## (A) Using rpart (CART)
model_rpart <- rpart(cyl ~ mpg + hp + wt + disp, data = train_data, method = "class")
plot(model_rpart)
text(model_rpart, use.n = TRUE, cex = 0.8)

# Predict and evaluate using rpart
pred_rpart <- predict(model_rpart, test_data, type = "class")
conf_matrix_rpart <- confusionMatrix(pred_rpart, test_data$cyl)
print(conf_matrix_rpart)

## (B) Using tree
model_tree <- tree(cyl ~ mpg + hp + wt + disp, data = train_data)
plot(model_tree)
text(model_tree, all = TRUE, cex = 0.6)

# Predict and evaluate using tree
pred_tree <- predict(model_tree, test_data, type = "class")
conf_matrix_tree <- confusionMatrix(pred_tree, test_data$cyl)
print(conf_matrix_tree)

## (C) Using ctree (Conditional Inference Trees)
model_ctree <- ctree(cyl ~ mpg + hp + wt + disp, data = train_data)
plot(model_ctree)

# Predict and evaluate using ctree
pred_ctree <- predict(model_ctree, test_data)
conf_matrix_ctree <- confusionMatrix(pred_ctree, test_data$cyl)
print(conf_matrix_ctree)

# 2. Fine-Tuning Trees

## (A) Fine-Tuning rpart
model_rpart_tuned <- rpart(cyl ~ mpg + hp + wt + disp, data = train_data, method = "class", control = rpart.control(maxdepth = 3))
pred_rpart_tuned <- predict(model_rpart_tuned, test_data, type = "class")
conf_matrix_rpart_tuned <- confusionMatrix(pred_rpart_tuned, test_data$cyl)
print(conf_matrix_rpart_tuned)

## (B) Fine-Tuning tree
model_tree_tuned <- tree(cyl ~ mpg + hp + wt + disp, data = train_data, control = tree.control(nobs = 22, mincut = 5))
pred_tree_tuned <- predict(model_tree_tuned, test_data, type = "class")
conf_matrix_tree_tuned <- confusionMatrix(pred_tree_tuned, test_data$cyl)
print(conf_matrix_tree_tuned)

## (C) Fine-Tuning ctree
model_ctree_tuned <- ctree(cyl ~ mpg + hp + wt + disp, data = train_data, controls = ctree_control(maxdepth = 3, minsplit = 5))
pred_ctree_tuned <- predict(model_ctree_tuned, test_data)
conf_matrix_ctree_tuned <- confusionMatrix(pred_ctree_tuned, test_data$cyl)
print(conf_matrix_ctree_tuned)

# 3. Cross-Validation on rpart
train_control <- trainControl(method = "cv", number = 10)
cv_model_rpart <- train(cyl ~ mpg + hp + wt + disp, data = train_data, method = "rpart", trControl = train_control)

# Predict and evaluate using cross-validation model
pred_cv_rpart <- predict(cv_model_rpart, test_data)
conf_matrix_cv_rpart <- confusionMatrix(pred_cv_rpart, test_data$cyl)
print(conf_matrix_cv_rpart)

# 4. Compare Accuracy of All Models
accuracy_results <- data.frame(
  Model = c("rpart", "tree", "ctree", "rpart (Tuned)", "tree (Tuned)", "ctree (Tuned)"),
  Accuracy = c(
    conf_matrix_rpart$overall["Accuracy"],
    conf_matrix_tree$overall["Accuracy"],
    conf_matrix_ctree$overall["Accuracy"],
    conf_matrix_rpart_tuned$overall["Accuracy"],
    conf_matrix_tree_tuned$overall["Accuracy"],
    conf_matrix_ctree_tuned$overall["Accuracy"]
  )
)

# Print accuracy results and plot comparison
print(accuracy_results)
barplot(accuracy_results$Accuracy, names.arg = accuracy_results$Model, col = rainbow(6), 
        main = "Decision Tree Model Accuracy (mtcars)", ylab = "Accuracy", ylim = c(0, 1))
