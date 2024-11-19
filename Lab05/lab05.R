###############################
###     Lab 05 - SVM       ###
###############################

library(ggplot2)
library(lattice)
library("caret")
library(e1071)

wine_l5 <- read.csv("~/ITWS-1100/DataAnalytics2024_Olatunde_Mojisola/lab04/wine/wine.data", header=FALSE)


wine_l5$Type <- as.factor(wine_l5$Type)


colnames(wine_l5) <- c("Type", "Alcohol", "Malic acid", 'Ash', 'Alcalinity of ash', 'Magnesium', 'Total phenols', 'Flavanoids', ' Nonflavanoid phenols', 'Proanthocyanins', 'Color Intensity', 'Hue', 'Diluted wines', 'Proline' )

View(wine_l5)

# For reproducibility
set.seed(123)

# Create training and test sets (70-30 split)
wine_l5train_index <- createDataPartition(wine_l5$Type, p = 0.7, list = FALSE)


l5_train_winedata <- wine_l5[wine_l5train_index,]
l5_test_winedata <- wine_l5[-wine_l5train_index,]

## Pick a subset of features
# Using Alcohol, Ash, Alcalinity, Color, Hue, and Proline
l5_choosen_wine_features <- c(2,4,5,11,12,14)

l5_X_wine_train <- l5_train_winedata[, l5_choosen_wine_features]
l5_y_wine_train <- l5_train_winedata$Type

l5_X_wine_test <- l5_test_winedata[, l5_choosen_wine_features]
l5_y_wine_test <- l5_test_winedata$Type


# Scale the features
l5_preproc <- preProcess(l5_X_wine_train, method = c("center", "scale"))

l5_X_wine_train_scaled <- predict(l5_preproc, l5_X_wine_train)
l5_X_wine_test_scaled <- predict(l5_preproc, l5_X_wine_test)

# Tune linear SVM
set.seed(123)
l5_linear_tune <- tune.svm(
  x = l5_X_wine_train_scaled,
  y = l5_y_wine_train,
  kernel = "linear",
  cost = 2^(-2:8),
  tunecontrol = tune.control(cross = 5)
)

# Print best parameters for linear SVM
print(l5_linear_tune$best.parameters)


# Train linear SVM with best parameters
l5_linear_svm <- svm(
  x = l5_X_wine_train_scaled,
  y = l5_y_wine_train,
  kernel = "linear",
  cost = l5_linear_tune$best.parameters$cost
)


# Tune radial SVM
set.seed(123)
l5_radial_wine_tune <- tune.svm(
  x = l5_X_wine_train_scaled,
  y = l5_y_wine_train,
  kernel = "radial",
  cost = 2^(-2:8),
  gamma = 2^(-8:2),
  tunecontrol = tune.control(cross = 5)
)


# Print best parameters for radial SVM
print(l5_radial_wine_tune$best.parameters)

# Train radial SVM with best parameters
l5_radial_svm <- svm(
  x = l5_X_wine_train_scaled,
  y = l5_y_wine_train,
  kernel = "radial",
  cost = l5_radial_wine_tune$best.parameters$cost,
  gamma = l5_radial_wine_tune$best.parameters$gamma
)


##Make predictions on test set
l5_linear_svm_pred <- predict(l5_linear_svm, l5_X_wine_test_scaled)
l5_radial_svm_pred <- predict(l5_radial_svm, l5_X_wine_test_scaled)



# Calculate and print performance metrics
l5_linear_cm <- confusionMatrix(l5_linear_svm_pred, l5_y_wine_test)
l5_radial_cm <- confusionMatrix(l5_radial_svm_pred, l5_y_wine_test)



## Print Linear SVM Performance
print(l5_linear_cm$overall)
print(l5_linear_cm$byClass)

## Print Radial SVM Performance
print(l5_radial_cm$overall)
print(l5_radial_cm$byClass)



#### Using kNN to train a classifier ####
#### based on the same features. ####

library(class)

# Prepare data
kNN_X <- wine_l5[, l5_choosen_wine_features]
kNN_y <- wine_l5$Type

# Scale features
kNN_X_scaled <- scale(kNN_X)

# Create training and test sets
kNN_train_index <- createDataPartition(kNN_y, p = 0.7, list = FALSE)
kNN_X_train <- kNN_X_scaled[kNN_train_index, ]
kNN_X_test <- kNN_X_scaled[-kNN_train_index, ]
kNN_y_train <- kNN_y[kNN_train_index]
kNN_y_test <- kNN_y[-kNN_train_index]

# Tune k using cross-validation
k_values <- seq(1, 15, by = 2)
cv_accuracies <- sapply(k_values, function(k) {
  pred <- knn.cv(kNN_X_train, kNN_y_train, k = k)
  mean(pred == kNN_y_train)
})

# Select best k
best_k <- k_values[which.max(cv_accuracies)]

# Train final kNN model
knn_pred <- knn(kNN_X_train, kNN_X_test, kNN_y_train, k = best_k)

# Compute confusion matrix
knn_cm <- confusionMatrix(knn_pred, kNN_y_test)

# Print results
cat("Best k value:", best_k, "\n")
print("kNN Performance:")
print(knn_cm$overall)
print(knn_cm$byClass)

# Visualization of k selection
plot(k_values, cv_accuracies, 
     type = "b", 
     xlab = "k", 
     ylab = "Cross-Validation Accuracy",
     main = "k Selection for kNN")




### Compare the performance of the   ###
### 2 models (Precision, Recall, F1)  ###


# Combine performance metrics for SVM and kNN


# From model results
svm_linear_metrics <- l5_linear_cm$byClass
svm_radial_metrics <- l5_radial_cm$byClass
knn_metrics <- knn_cm$byClass

# Create comparison table
performance_comparison <- data.frame(
  Model = c("Linear SVM", "Radial SVM", "kNN"),
  Precision = c(
    svm_linear_metrics["Precision"],
    svm_radial_metrics["Precision"],
    knn_metrics["Precision"]
  ),
  Recall = c(
    svm_linear_metrics["Sensitivity"],
    svm_radial_metrics["Sensitivity"],
    knn_metrics["Sensitivity"]
  ),
  F1_Score = c(
    svm_linear_metrics["F1"],
    svm_radial_metrics["F1"],
    knn_metrics["F1"]
  )
)


### Print Linear SVM Confusion Matrix
print(t_linear_cm$table)

### Print Radial SVM Confusion Matrix
print(t_radial_cm$table)

### Print kNN Confusion Matrix
print(t_knn_cm$table)

# Print comparison
print(performance_comparison)






### NY Housing Dataset ###


