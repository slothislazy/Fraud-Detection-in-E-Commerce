# Load necessary libraries  ======================================================================================================
library(ggplot2)
library(reshape2)
library(pROC)
library(ROSE)
library(randomForest)
library(xgboost)

# Loading data ======================================================================================================

# Load data
df <- read.csv("merged_dataset.csv", stringsAsFactors = FALSE)

# Check for missing values
print(colSums(is.na(df)))

# Check summary of dataset
str(df)
summary(df)

# Filter out invalid Customer Age
df <- df[df$Customer.Age >= 0, ]

# Splitting dataset into train and test sets  ======================================================================================================

set.seed(123)  # For reproducibility
split_ratio <- 0.7  # 70% train, 30% test

# Create an index for the training set
train_index <- sample(seq_len(nrow(df)), size = floor(split_ratio * nrow(df)))

# Split the data into train and test sets
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Verify the dimensions of the splits
print(paste("Train Data Dimensions:", dim(train_data)[1], "rows,", dim(train_data)[2], "columns"))
print(paste("Test Data Dimensions:", dim(test_data)[1], "rows,", dim(test_data)[2], "columns"))

# EDA ======================================================================================================

# Histograms
hist(df$Transaction.Amount, col = "steelblue", main = "Histogram of Transaction Amount", xlab = "Transaction Amount")
hist(df$Customer.Age, col = "steelblue", main = "Histogram of Age", xlab = "Age")
hist(df$Account.Age.Days, col = "steelblue", main = "Histogram of Account Age in Days", xlab = "Account Age")
hist(df$Transaction.Hour, col = "steelblue", main = "Transaction Hour", xlab = "Transaction Hour")

# Ensure Is.Fraudulent is a factor
df$Is.Fraudulent <- as.factor(df$Is.Fraudulent)

# Bar chart to compare 2 variables
ggplot(df, aes(x = Device.Used, fill = Is.Fraudulent)) +
  geom_bar(position = "fill", alpha = 0.7) +
  scale_fill_manual(values = c("steelblue", "salmon"), 
                    labels = c("Not Fraudulent", "Fraudulent")) +
  labs(title = "Proportion of Browser by Fraud Status",
       x = "Browser",
       y = "Proportion",
       fill = "Fraud Status") +
  theme_minimal()

# Histogram to compare 2 variables
ggplot(df[df$Transaction.Amount > 1000 & df$Transaction.Amount < 5000, ], aes(x = Transaction.Amount, fill = Is.Fraudulent)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 50) +
  scale_fill_manual(values = c("steelblue", "salmon"), 
                    labels = c("Not Fraudulent", "Fraudulent")) +
  labs(title = "Comparison of Transaction Amount by Fraud Status",
       x = "Transaction Amount",
       y = "Frequency",
       fill = "Fraud Status") +
  ylim(0, 2000) +
  theme_minimal()

# Preprocessing ======================================================================================================

# Preprocessing: Consistently Apply to Train and Test Data
# Convert "Transaction.Date" to Date format and extract features
train_data$Transaction.Date <- as.Date(train_data$Transaction.Date, format = "%Y-%m-%d")
test_data$Transaction.Date <- as.Date(test_data$Transaction.Date, format = "%Y-%m-%d")

train_data$Transaction.Day <- as.numeric(format(train_data$Transaction.Date, "%d"))
test_data$Transaction.Day <- as.numeric(format(test_data$Transaction.Date, "%d"))

train_data$Transaction.DOW <- as.numeric(format(train_data$Transaction.Date, "%u"))
test_data$Transaction.DOW <- as.numeric(format(test_data$Transaction.Date, "%u"))

train_data$Transaction.Month <- as.numeric(format(train_data$Transaction.Date, "%m"))
test_data$Transaction.Month <- as.numeric(format(test_data$Transaction.Date, "%m"))

# One-hot encoding of categorical variables
encode_dummies <- function(data) {
  source_dummies <- model.matrix(~ source - 1, data = data)
  browser_dummies <- model.matrix(~ browser - 1, data = data)
  sex_dummies <- model.matrix(~ sex - 1, data = data)
  payment_method_dummies <- model.matrix(~ Payment.Method - 1, data = data)
  product_category_dummies <- model.matrix(~ Product.Category - 1, data = data)
  device_used_dummies <- model.matrix(~ Device.Used - 1, data = data)
  
  data <- cbind(data, source_dummies, browser_dummies, sex_dummies,
                payment_method_dummies, product_category_dummies, device_used_dummies)
  
  # Remove original categorical columns
  data <- data[, !colnames(data) %in% c("source", "browser", "sex", 
                                        "Payment.Method", "Product.Category", 
                                        "Device.Used", "Transaction.Date")]
  
  # Ensure column names are valid
  colnames(data) <- make.names(colnames(data))
  return(data)
}

train_data <- encode_dummies(train_data)
test_data <- encode_dummies(test_data)

# Balancing the training dataset
#train_data <- ovun.sample(Is.Fraudulent ~ ., data = train_data, method = "both", p = 0.5)$data

# Ensure Is.Fraudulent is a factor
train_data$Is.Fraudulent <- as.factor(train_data$Is.Fraudulent)
test_data$Is.Fraudulent <- as.factor(test_data$Is.Fraudulent)

# Logistic Regression Model
selected_predictors <- c("Transaction.Amount", "Account.Age.Days", "Transaction.Hour", "Transaction.Day",
                         "Transaction.Month", "browserChrome", "browserFireFox", "browserIE", 
                         "browserOpera", "browserSafari", "sourceAds", "sourceDirect", "sourceSEO")

formula <- as.formula(paste("Is.Fraudulent ~", paste(selected_predictors, collapse = " + ")))

logistic_model <- glm(
  formula = formula,
  data = train_data,
  family = binomial
)

# Predict on the test set
predicted_probs <- predict(logistic_model, newdata = test_data, type = "response")

# Convert probabilities to binary classes
threshold <- 0.7
predicted_classes <- ifelse(predicted_probs > threshold, 1, 0)

# Confusion Matrix and Metrics for Logistic Regression
confusion_matrix <- table(Predicted = predicted_classes, Actual = as.numeric(as.character(test_data$Is.Fraudulent)))
print("Confusion Matrix:")
print(confusion_matrix)

# Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# Precision, Recall, F1 Score
TP <- confusion_matrix[2, 2]
FP <- confusion_matrix[2, 1]
FN <- confusion_matrix[1, 2]

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

print(paste("Precision:", round(precision * 100, 2), "%"))
print(paste("Recall (Sensitivity):", round(recall * 100, 2), "%"))
print(paste("F1 Score:", round(f1_score * 100, 2), "%"))

# Plot ROC Curve
roc_curve <- roc(as.numeric(as.character(test_data$Is.Fraudulent)), predicted_probs)
plot(roc_curve, main = "ROC Curve", col = "blue")
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 2)))

# Random Forest ===========================================================================================
train_data$Is.Fraudulent <- as.factor(train_data$Is.Fraudulent)
test_data$Is.Fraudulent <- as.factor(test_data$Is.Fraudulent)

# Train a random forest model
rf_model <- randomForest(
  Is.Fraudulent ~ ., 
  data = train_data, 
  ntree = 50,
  mtry = sqrt(ncol(train_data) - 1)
)

# Predict classes on test data
predicted_classes <- predict(rf_model, newdata = test_data)

# Predict probabilities on test data
predicted_probs <- predict(rf_model, newdata = test_data, type = "prob")[, 2]

# Confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_data$Is.Fraudulent)
print("Confusion Matrix:")
print(confusion_matrix)

# Extract true positives, false positives, true negatives, and false negatives
TP <- confusion_matrix[2, 2]  # True Positives
FP <- confusion_matrix[2, 1]  # False Positives
TN <- confusion_matrix[1, 1]  # True Negatives
FN <- confusion_matrix[1, 2]  # False Negatives

# Accuracy
accuracy <- (TP + TN) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# Precision
precision <- TP / (TP + FP)
print(paste("Precision:", round(precision * 100, 2), "%"))

# Recall
recall <- TP / (TP + FN)
print(paste("Recall (Sensitivity):", round(recall * 100, 2), "%"))

# F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1 Score:", round(f1_score * 100, 2), "%"))

# ROC curve and AUC
roc_curve <- roc(test_data$Is.Fraudulent, predicted_probs)
plot(roc_curve, main = "ROC Curve - Random Forest", col = "blue")
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 2)))

# XGBoost ===========================================================================================

# Prepare data for xgboost
train_labels <- as.numeric(train_data$Is.Fraudulent) - 1  # Convert to binary (0/1)
test_labels <- as.numeric(test_data$Is.Fraudulent) - 1
train_matrix <- as.matrix(train_data[, -which(names(train_data) == "Is.Fraudulent")])
test_matrix <- as.matrix(test_data[, -which(names(test_data) == "Is.Fraudulent")])

# Train an XGBoost model
xgb_model <- xgboost(
  data = train_matrix,
  label = train_labels,
  nrounds = 100,
  objective = "binary:logistic",
  eval_metric = "auc",
  verbose = 0
)

# Predict probabilities on test data
predicted_probs <- predict(xgb_model, newdata = test_matrix)

# Convert probabilities to binary classes
threshold <- 0.7
predicted_classes <- ifelse(predicted_probs > threshold, 1, 0)

# Calculate accuracy
correct_predictions <- sum(predicted_classes == test_labels)
total_predictions <- length(test_labels)
accuracy <- correct_predictions / total_predictions

# Print accuracy as a percentage
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# Create confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = test_labels)
print("Confusion Matrix:")
print(confusion_matrix)

# Extract confusion matrix components
TP <- confusion_matrix[2, 2]  # True Positives
FP <- confusion_matrix[2, 1]  # False Positives
TN <- confusion_matrix[1, 1]  # True Negatives
FN <- confusion_matrix[1, 2]  # False Negatives

# Calculate metrics
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print metrics
print(paste("Precision:", round(precision * 100, 2), "%"))
print(paste("Recall (Sensitivity):", round(recall * 100, 2), "%"))
print(paste("F1 Score:", round(f1_score * 100, 2), "%"))

# ROC curve and AUC
roc_curve <- roc(test_labels, predicted_probs)
plot(roc_curve, main = "ROC Curve - XGBoost", col = "blue")
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 2)))

