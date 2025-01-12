library(dslabs)
library(dplyr)
library(lubridate)
library(caret)

data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

online_data <- dat %>% filter ( type == "online")
percentage_online <-mean(online_data$sex == "Female", na.rm = TRUE) 

class_data <- dat %>% filter (type =="inclass")
percentage_inclass <-mean(class_data$sex == "Female", na.rm = TRUE) 



dat <- dat%>%mutate(predict= ifelse(type == "inclass", "Female", "Male") )
y_hat <- factor(dat$predict,c("Female", "Male"))

accuracy <- mean(dat$sex == dat$predict, na.rm = TRUE)

print(accuracy)
print(table(y_hat,y))

cm <- confusionMatrix(y_hat, y)
print(cm)


df <- data.frame(truth = y, estimate = y_hat)

