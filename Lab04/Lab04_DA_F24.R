##########################################
### Principal Component Analysis (PCA) ###
##########################################
install.packages("psych")

library(ggplot2)
library(ggfortify)
library(e1071)
library(class)
library(psych)

# PCA with WINE dataset
wine_da <- read.csv("~/ITWS-1100/DataAnalytics2024_Olatunde_Mojisola/lab04/wine/wine.data", header=FALSE)

colnames(wine) <- c("Type", "Alcohol", "Malic acid", 'Ash', 'Alcalinity of ash', 'Magnesium', 'Total phenols', 'Flavanoids', ' Nonflavanoid phenols', 'Proanthocyanins', 'Color Intensity', 'Hue', 'Diluted wines', 'Proline' )

head(wine_da)

# Inspect the data
summary(wine_da)

wine_da$Type <- as.factor(wine_da$Type)

View(wine)


wine_nxt <- wine_da[,-c(4,5,10)]

pairs.panels(wine_nxt[,-1],gap = 0,bg = c("orange", "green", "red")[wine_nxt$Type],pch=21)


# Perform PCA on the wine dataset (excluding the Type column)
wine_pca_result <- prcomp(wine_da[,-1], scale. = TRUE)

# Plot the first two principal components
autoplot(wine_pca_result, data = wine, colour = 'Type', main = "PCA Plot (PC1 vs PC2)")

# Extract loadings and find variables contributing most to the 1st principal component
loadings <- wine_pca_result$rotation[,1]
contributing_vars <- sort(abs(loadings), decreasing = TRUE)
print(contributing_vars)

# Load libraries for the classification
install.packages("randomForest")

library(randomForest)
library(caret)

# Set seed for reproducibility
set.seed(123)

# Split data into training and test sets
win_trainIndex <- createDataPartition(wine_da$Type, p = 0.7, list = FALSE)
win_train_data <- wine_da[win_trainIndex,]


win_test_data <- wine_da[-win_trainIndex,]
#since i'm having error in training model due to space in columns
#I use the make.names() function to standardize column names:
names(win_train_data) <- make.names(names(win_train_data))
names(win_test_data) <- make.names(names(win_test_data))


# Train Random Forest classifier
rf_model_all <- randomForest(Type ~ ., data = win_train_data)
rf_pred_all <- predict(rf_model_all, win_test_data)

# Evaluate model
confusionMatrix(rf_pred_all, win_test_data$Type)

#Training a Classifier Using the First 3 Principal Components

# Project data onto the first 3 principal components
pca_data <- as.data.frame(wine_pca_result$x[,1:3])
pca_data$Type <- wine_da$Type

# Split data into training and test sets
train_pca <- pca_data[win_trainIndex,]
test_pca <- pca_data[-win_trainIndex,]

# Train Random Forest on the first 3 PCs
rf_model_pca <- randomForest(Type ~ ., data = train_pca)
rf_pred_pca <- predict(rf_model_pca, test_pca)

# Evaluate model
confusionMatrix(rf_pred_pca, test_pca$Type)

#Dropping The Least Contributing Variables and Rerun PCA

# Select the top contributing variables (for instance, top 10)
top_vars <- names(contributing_vars[1:10])
wine_da_reduced <- wine_da[, c("Type", top_vars)]

# Perform PCA on reduced dataset
pca_result_reduced <- prcomp(wine_da_reduced[,-1], scale. = TRUE)

# Project data onto the first 3 PCs
pca_data_reduced <- as.data.frame(pca_result_reduced$x[,1:3])
pca_data_reduced$Type <- wine_da_reduced$Type

# Split into training and test sets
train_pca_reduced <- pca_data_reduced[win_trainIndex,]
test_pca_reduced <- pca_data_reduced[-win_trainIndex,]

# Train Random Forest model on reduced PCA data
rf_model_pca_reduced <- randomForest(Type ~ ., data = train_pca_reduced)
rf_pred_pca_reduced <- predict(rf_model_pca_reduced, test_pca_reduced)

# Evaluate model
confusionMatrix(rf_pred_pca_reduced, test_pca_reduced$Type)


#Comparing the Models

# Define a new function to calculate metrics for multiclass
calculate_multiclass_metrics <- function(pred, actual) {
  # Create a confusion matrix
  conf_mat <- confusionMatrix(pred, actual)
  
  # Extract precision, recall, and F1 for each class
  precision <- diag(conf_mat$table) / rowSums(conf_mat$table)
  recall <- diag(conf_mat$table) / colSums(conf_mat$table)
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  # Calculate macro-average F1 score
  macro_f1 <- mean(f1, na.rm = TRUE)
  
  list(
    confusion_matrix = conf_mat$table,
    precision = precision,
    recall = recall,
    f1 = f1,
    macro_f1 = macro_f1
  )
}


# Model metrics
metrics_all <- calculate_multiclass_metrics(rf_pred_all, win_test_data$Type)
metrics_pca <- calculate_multiclass_metrics(rf_pred_pca, test_pca$Type)
metrics_pca_reduced <- calculate_multiclass_metrics(rf_pred_pca_reduced, test_pca_reduced$Type)

# Print metrics
print(metrics_all)
print(metrics_pca)
print(metrics_pca_reduced)




