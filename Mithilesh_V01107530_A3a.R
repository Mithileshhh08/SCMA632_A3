# Load necessary libraries
library(dplyr)
library(ggplot2)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)

# Set working directory and load the dataset
setwd("E:/BOOTCAMP/ASSIGNMENTS/SCMA/A3")
df <- read.csv("advertising.csv")

# Display the first few rows of the dataset
head(df)
# Data Cleaning and EDA
# Remove spaces in column names
colnames(df)[colnames(df) == "default "] <- "default"

# Check class balance
table(df$default)

# Identify categorical features
cat_features <- df %>% select_if(is.character)

# Data Encoding for Categorical Variables
encoded_num_df <- as.data.frame(lapply(cat_features, as.factor))

# Convert necessary columns to appropriate types
df$Male <- as.factor(df$Male)
df$Clicked.on.Ad <- as.factor(df$Clicked.on.Ad)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(df$Clicked.on.Ad, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dfTrain <- df[ trainIndex,]
dfTest  <- df[-trainIndex,]



# Fit the logistic regression model
logistic_model <- glm(Clicked.on.Ad ~ . -Timestamp -Ad.Topic.Line -City -Country, 
                      data = dfTrain, 
                      family = binomial)
# Summary of the logistic regression model
summary(logistic_model)

# Check residuals
par(mfrow = c(2, 2))
plot(logistic_model)

# Predict probabilities
predicted_probabilities <- predict(logistic_model, dfTest, type = "response")

# Set a threshold (default is 0.5)
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Create a confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = dfTest$Clicked.on.Ad)
confusion_matrix

# Calculate accuracy, sensitivity, and specificity
confusionMatrix(as.factor(predicted_classes), dfTest$Clicked.on.Ad)

# Plot the ROC curve
roc_curve <- roc(dfTest$Clicked.on.Ad, predicted_probabilities)
plot(roc_curve)
auc(roc_curve)


# Fit the decision tree model
tree_model <- rpart(Clicked.on.Ad ~ . -Timestamp -Ad.Topic.Line -City -Country, 
                    data = dfTrain, 
                    method = "class")

# Plot the decision tree
rpart.plot(tree_model)

# Predict using the decision tree model
tree_predictions <- predict(tree_model, dfTest, type = "class")

# Create a confusion matrix
confusion_matrix_tree <- table(Predicted = tree_predictions, Actual = dfTest$Clicked.on.Ad)
confusion_matrix_tree

# Calculate accuracy, sensitivity, and specificity for the decision tree
confusionMatrix(as.factor(tree_predictions), dfTest$Clicked.on.Ad)

# Plot the ROC curve for the decision tree
tree_probabilities <- predict(tree_model, dfTest, type = "prob")[, 2]
roc_curve_tree <- roc(dfTest$Clicked.on.Ad, tree_probabilities)
plot(roc_curve_tree)
auc(roc_curve_tree)

# Comparison Table
comparison_df <- rbind(
  as.data.frame(logrepo$byClass),
  as.data.frame(dtree$byClass)
)
comparison_df$model <- c("Logistic Regression", "Decision Tree")
rownames(comparison_df) <- NULL

