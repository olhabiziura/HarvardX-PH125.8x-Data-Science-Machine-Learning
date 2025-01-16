library(tidyverse)
library(caret)
library(magrittr)

#--------------------------------------------------------------------
  
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>% data.frame() %>% setNames(c("x", "y"))


rmse <- function(pred, actual) {
  sqrt(mean((pred - actual)^2))
}

set.seed(1)
rmse_values <- replicate(100, {
  indices <- sample(nrow(dat), size = 0.5 * nrow(dat))
  train <- dat[indices, ]
  test <- dat[-indices, ]
  model <- lm(y ~ x, data = train)
  predictions <- predict(model, newdata = test)
  rmse(predictions, test$y)
})

any(is.na(rmse_values))

mean_rmse <- mean(rmse_values)
sd_rmse <- sd(rmse_values)

cat("Mean RMSE:", round(mean_rmse, 3), "\n")
cat("Standard Deviation of RMSE:", round(sd_rmse, 5), "\n")

#-------------------------------------------------------------------
  
set.seed(1)

dataset <- function(n){
  Sigma <- 9 * matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, mu = c(69, 69), Sigma) %>% data.frame() %>% setNames(c("x", "y"))
  

  rmse <- function(pred, actual) {
    sqrt(mean((pred - actual)^2))
  }
  
  # Run the replicate loop to calculate RMSE for 100 models
  rmse_values <- replicate(100, {
    indices <- sample(nrow(dat), size = 0.5 * nrow(dat))
    train <- dat[indices, ]
    test <- dat[-indices, ]
    model <- lm(y ~ x, data = train)
    predictions <- predict(model, newdata = test)
    rmse(predictions, test$y)
  })
  
 
  mean_rmse <- mean(rmse_values)
  sd_rmse <- sd(rmse_values)
  
  return(c(mean_rmse = mean_rmse, sd_rmse = sd_rmse))
}


n_values <- c(100, 500, 1000, 5000, 10000)
result <- sapply(n_values, dataset)


print(result)

#------------------------------------------------------------------------

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
  
rmse <- function(pred, actual) {
  sqrt(mean((pred - actual)^2))
}

set.seed(1)
rmse_values <- replicate(100, {
  indices <- sample(nrow(dat), size = 0.5 * nrow(dat))
  train <- dat[indices, ]
  test <- dat[-indices, ]
  model <- lm(y ~ x, data = train)
  predictions <- predict(model, newdata = test)
  rmse(predictions, test$y)
})

any(is.na(rmse_values))

mean_rmse <- mean(rmse_values)
sd_rmse <- sd(rmse_values)

cat("Mean RMSE:", round(mean_rmse, 3), "\n")
cat("Standard Deviation of RMSE:", round(sd_rmse, 5), "\n")

#------------------------------------------------------------------------
library(MASS)
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))


trainIndex <- createDataPartition(dat$y, p = 0.5, list = FALSE)
train_data <- dat[trainIndex, ]
test_data <- dat[-trainIndex, ]


model_x1 <- lm(y ~ x_1, data = train_data)
predictions_x1 <- predict(model_x1, newdata = test_data)
rmse_x1 <- sqrt(mean((predictions_x1 - test_data$y)^2))


model_x2 <- lm(y ~ x_2, data = train_data)
predictions_x2 <- predict(model_x2, newdata = test_data)
rmse_x2 <- sqrt(mean((predictions_x2 - test_data$y)^2))

model_x1x2 <- lm(y ~ x_1 + x_2, data = train_data)
predictions_x1x2 <- predict(model_x1x2, newdata = test_data)
rmse_x1x2 <- sqrt(mean((predictions_x1x2 - test_data$y)^2))


cat("RMSE using x_1:", round(rmse_x1, 3), "\n")
cat("RMSE using x_2:", round(rmse_x2, 3), "\n")
cat("RMSE using both x_1 and x_2:", round(rmse_x1x2, 3), "\n")

if (rmse_x1 < rmse_x2 & rmse_x1 < rmse_x1x2) {
  cat("The best model is the one using x_1 only.\n")
} else if (rmse_x2 < rmse_x1 & rmse_x2 < rmse_x1x2) {
  cat("The best model is the one using x_2 only.\n")
} else {
  cat("The best model is the one using both x_1 and x_2.\n")
}

#------------------------------------------------------------------------
