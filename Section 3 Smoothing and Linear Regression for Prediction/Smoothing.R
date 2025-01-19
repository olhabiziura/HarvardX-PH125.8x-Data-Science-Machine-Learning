library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_tibble() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")


loess_fit <- loess(deaths ~ as.numeric(date), data = dat, span = 60 / diff(range(as.numeric(dat$date))), degree = 1)

smoothed_data <- data.frame(
  date = seq(min(dat$date), max(dat$date), by = "1 day")
)
smoothed_data$deaths <- predict(loess_fit, newdata = data.frame(date = as.numeric(smoothed_data$date)))


library(ggplot2)
ggplot(dat, aes(x = date, y = deaths)) +
  geom_point(alpha = 0.5, size = 1, color = "blue") + 
  geom_line(data = smoothed_data, aes(x = date, y = deaths), color = "red", size = 1) +  
  labs(title = "Smoothed Estimate of Deaths Over Time",
       x = "Date",
       y = "Number of Deaths") +
  theme_minimal()

dat %>% 
  mutate(smooth = predict(loess_fit, newdata = data.frame(date = as.numeric(date))), 
         day = yday(date), 
         year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)


#-----------------------------------------------------------

library(broom)
library(dslabs)

data(mnist_27)


mnist_27$train$y_numeric <- as.numeric(mnist_27$train$y == 7)
mnist_27$test$y_numeric <- as.numeric(mnist_27$test$y == 7)

sum(is.na(mnist_27$train$x_2))
sum(is.na(mnist_27$test$x_2))

loess_fit <- loess(y_numeric ~ x_2, data = mnist_27$train, degree = 1)

predictions <- predict(loess_fit, newdata = mnist_27$test)
sum(is.na(predictions))
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
sum(is.na(predicted_classes))

accuracy <- mean(predicted_classes == mnist_27$test$y_numeric, na.rm = TRUE)
print(accuracy)


