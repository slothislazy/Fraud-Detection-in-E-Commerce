# Load necessary libraries  ======================================================================================================
library(ggplot2)
library(reshape2)

# Load the datasets ======================================================================================================
dataset1 <- read.csv("Dataset1.csv", stringsAsFactors = FALSE)
dataset2 <- read.csv("Dataset2.csv", stringsAsFactors = FALSE)

# Check dimensions and structure of datasets
cat("Dataset 1 - Rows:", nrow(dataset1), "Columns:", ncol(dataset1), "\n")
cat("Dataset 2 - Rows:", nrow(dataset2), "Columns:", ncol(dataset2), "\n")
cat("Dataset 1 Structure:\n")
str(dataset1)
cat("Dataset 2 Structure:\n")
str(dataset2)

# Check for missing values
cat("Missing values in Dataset 1:", sum(is.na(dataset1)), "\n")
cat("Missing values in Dataset 2:", sum(is.na(dataset2)), "\n")

# Handle duplicates
dataset1 <- dataset1[!duplicated(dataset1), ]
dataset2 <- dataset2[!duplicated(dataset2), ]

# Take only 150000 Data
dataset1 <- dataset1[sample(nrow(dataset1), 150000), ]
dataset2 <- dataset2[sample(nrow(dataset2), 150000), ]

# EDA ======================================================================================================
hist(dataset1$Transaction.Amount, main = "Histogram of Transaction Amount (Dataset 1)", xlab = "Transaction Amount")
hist(dataset1$Customer.Age, main = "Histogram of Customer Age (Dataset 1)", xlab = "Customer Age")

hist(dataset2$purchase_value, main = "Histogram of Purchase Value (Dataset 2)", xlab = "Purchase Value")
hist(dataset2$age, main = "Histogram of Age (Dataset 2)", xlab = "Age")

# Plot boxplots for visual inspection of outliers
boxplot(dataset1$Transaction.Amount, main = "Boxplot of Transaction Amount (Dataset 1)")
boxplot(dataset1$Customer.Age, main = "Boxplot of Customer Age (Dataset 1)")
boxplot(dataset2$purchase_value, main = "Boxplot of Purchase Value (Dataset 2)")
boxplot(dataset2$age, main = "Boxplot of Age (Dataset 2)")

# Preparing data to merge ======================================================================================================

# Rename columns for consistency in dataset2
colnames(dataset2) <- gsub("user_id", "Customer.ID", colnames(dataset2))
colnames(dataset2) <- gsub("age", "Customer.Age", colnames(dataset2))
colnames(dataset2) <- gsub("class", "Is.Fraudulent", colnames(dataset2))
colnames(dataset2) <- gsub("purchase_value", "Transaction.Amount", colnames(dataset2))

# Convert time columns to POSIXct format
dataset2$signup_time <- as.POSIXct(dataset2$signup_time, format = "%Y-%m-%d %H:%M:%S")
dataset2$purchase_time <- as.POSIXct(dataset2$purchase_time, format = "%Y-%m-%d %H:%M:%S")

# Create new columns based on date differences
dataset2$Account.Age.Days <- as.integer(difftime(dataset2$purchase_time, dataset2$signup_time, units = "days"))
dataset2$Transaction.Hour <- as.integer(format(dataset2$purchase_time, "%H"))
colnames(dataset2)[colnames(dataset2) == "purchase_time"] <- "Transaction.Date"

# Find address match
dataset1$Address.Match <- as.integer(dataset1$Shipping.Address == dataset1$Billing.Address)

# Merge datasets
merged_dataset <- merge(dataset2, dataset1, by = intersect(colnames(dataset2), colnames(dataset1)), all = TRUE)

# Drop unnecessary columns
merged_dataset <- merged_dataset[, !colnames(merged_dataset) %in% c("outliers", "signup_time", "device_id", "Customer.ID", "Transaction.ID", "Customer.Location", "IP.Address", "ip_address", "Billing.Address", "Shipping.Address")]

# Function to calculate counts and percentages
calculate_counts_and_percentages <- function(column, column_name) {
  counts <- table(column)
  percentages <- prop.table(counts) * 100
  cat("Counts for", column_name, ":\n")
  print(counts)
  cat("Percentages for", column_name, ":\n")
  print(round(percentages, 2))
}

# Calculate counts and percentages for various columns
calculate_counts_and_percentages(dataset2$browser, "Browser")
calculate_counts_and_percentages(dataset2$source, "Source")
calculate_counts_and_percentages(dataset2$sex, "Sex")
calculate_counts_and_percentages(dataset1$Payment.Method, "Payment Method")
calculate_counts_and_percentages(dataset1$Device.Used, "Device Used")
calculate_counts_and_percentages(dataset1$Product.Category, "Product Category")
calculate_counts_and_percentages(dataset1$Address.Match, "Address Match")

# Define categories and probabilities for filling missing values
set.seed(420)
fill_missing_values <- function(dataset) {
  # Define category probabilities
  browsers <- c("Chrome", "FireFox", "IE", "Opera", "Safari")
  browser_prob <- c(40.65, 16.29, 24.30, 2.43, 16.32) / 100
  sources <- c("Ads", "Direct", "SEO")
  source_probabilities <- c(39.63, 20.26, 40.11) / 100
  sex_categories <- c("M", "F")
  payment_method <- c("credit card", "bank transfer", "debit card", "PayPal")
  product_category <- c("health & beauty", "electronics", "clothing", "toys & games", "home & garden")
  device_used <- c("mobile", "tablet", "desktop")
  address_match_prob <- c(10.02, 89.98) / 100
  
  # Fill missing values based on predefined probabilities
  dataset$browser[is.na(dataset$browser)] <- sample(browsers, sum(is.na(dataset$browser)), replace = TRUE, prob = browser_prob)
  dataset$source[is.na(dataset$source)] <- sample(sources, sum(is.na(dataset$source)), replace = TRUE, prob = source_probabilities)
  dataset$sex[is.na(dataset$sex)] <- sample(sex_categories, sum(is.na(dataset$sex)), replace = TRUE)
  dataset$Payment.Method[is.na(dataset$Payment.Method)] <- sample(payment_method, sum(is.na(dataset$Payment.Method)), replace = TRUE)
  dataset$Product.Category[is.na(dataset$Product.Category)] <- sample(product_category, sum(is.na(dataset$Product.Category)), replace = TRUE)
  dataset$Device.Used[is.na(dataset$Device.Used)] <- sample(device_used, sum(is.na(dataset$Device.Used)), replace = TRUE)
  dataset$Quantity[is.na(dataset$Quantity)] <- sample(1:5, sum(is.na(dataset$Quantity)), replace = TRUE)
  dataset$Address.Match[is.na(dataset$Address.Match)] <- sample(c(0, 1), sum(is.na(dataset$Address.Match)), replace = TRUE, prob = address_match_prob)
  
  return(dataset)
}

# Merge ======================================================================================================

# Fill missing values in the merged dataset
merged_dataset <- fill_missing_values(merged_dataset)

# Calculate counts and percentages again for the merged dataset
calculate_counts_and_percentages(merged_dataset$sex, "Sex")
calculate_counts_and_percentages(merged_dataset$source, "Source")
calculate_counts_and_percentages(merged_dataset$browser, "Browser")
calculate_counts_and_percentages(merged_dataset$Payment.Method, "Payment Method")
calculate_counts_and_percentages(merged_dataset$Device.Used, "Device Used")
calculate_counts_and_percentages(merged_dataset$Product.Category, "Product Category")

# Save the merged dataset to a CSV file
write.csv(merged_dataset, "merged_dataset.csv", row.names = FALSE)
