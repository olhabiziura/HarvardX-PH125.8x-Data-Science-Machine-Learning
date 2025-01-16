library(tidyverse)
library(caret)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
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


mean_rmse <- mean(rmse_values)
sd_rmse <- sd(rmse_values)

cat("Mean RMSE:", round(mean_rmse, 3), "\n")
cat("Standard Deviation of RMSE:", round(sd_rmse, 3), "\n")


