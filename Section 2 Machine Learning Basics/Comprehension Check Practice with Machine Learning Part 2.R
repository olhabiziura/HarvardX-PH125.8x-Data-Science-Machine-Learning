library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(76)
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

calculate_accuracy <- function(feature, target, cutoff) {
  predicted <- ifelse(feature > cutoff, "virginica", "versicolor")
  accuracy <- mean(predicted == target)
  return(accuracy)
}

best_accuracy <- 0
second_best_accuracy <- 0
best_feature <- NULL
second_best_feature <- NULL
best_cutoff <- NULL

features <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

for (feature in features) {
  feature_values <- train[[feature]]
  possible_cutoffs <- seq(min(feature_values), max(feature_values), by = 0.1)
  
  for (cutoff in possible_cutoffs) {
    accuracy <- calculate_accuracy(feature_values, train$Species, cutoff)
    
    if (accuracy > best_accuracy) {
      second_best_accuracy <- best_accuracy
      second_best_feature <- best_feature
      best_accuracy <- accuracy
      best_feature <- feature
      best_cutoff <- cutoff
    }
  }
}


print(paste("Best feature:", best_feature))
print(paste("Second best feature:", second_best_feature))
print(paste("Best cutoff:", best_cutoff))
print(paste("Best accuracy:", round(best_accuracy, 4)))


test_feature_values <- test[[best_feature]]
test_predictions <- ifelse(test_feature_values > best_cutoff, "virginica", "versicolor")
test_accuracy <- mean(test_predictions == test$Species)
print(paste("Test accuracy:", round(test_accuracy, 4)))



petal_length_cutoffs <- seq(min(train$Petal.Length), max(train$Petal.Length), by = 0.1)
petal_width_cutoffs <- seq(min(train$Petal.Width), max(train$Petal.Width), by = 0.1)

best_accuracy <- 0
best_length_cutoff <- NULL
best_width_cutoff <- NULL

for (length_cutoff in petal_length_cutoffs) {
  for (width_cutoff in petal_width_cutoffs) {
    
    
    predictions <- ifelse(train$Petal.Length > length_cutoff & train$Petal.Width > width_cutoff, 
                          "virginica", "versicolor")
    
    
    accuracy <- mean(predictions == train$Species)
    
    
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_length_cutoff <- length_cutoff
      best_width_cutoff <- width_cutoff
    }
  }
}

test_predictions <- ifelse(test$Petal.Length > best_length_cutoff & test$Petal.Width > best_width_cutoff, 
                           "virginica", "versicolor")


test_accuracy <- mean(test_predictions == test$Species)
print(paste("Test accuracy with optimized cutoffs:", round(test_accuracy, 4)))

