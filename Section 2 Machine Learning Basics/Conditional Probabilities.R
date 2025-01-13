set.seed(1) 
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))


# probability that the test is positive:
# P(positive) = P(positive|healthy)*P(healthy) +P(positive|disease)*P(disease)

P_healthy <- mean(disease == 0)  # P(healthy)
P_disease <- mean(disease == 1) # P(disease)


P_positive_given_healthy <- mean(test[disease == 0] == 1) # P(positive | healthy)
P_positive_given_disease <- mean(test[disease == 1] == 1) # P(positive | disease)


P_positive <- P_positive_given_healthy * P_healthy + P_positive_given_disease * P_disease
P_positive


#probability that an individual has the disease if the test is negative
# P(disease | negative ) = P(disease)*P(negative | disease)/ P(negative)

P_negative_given_healthy = 1 - P_positive_given_healthy
P_negative_given_disease = 1-P_positive_given_disease

P_negative = P_negative_given_disease*P_disease + P_negative_given_healthy* P_healthy

P_disease_negative = P_disease*P_negative_given_disease/P_negative
P_disease_negative


#probability that you have the disease if the test is positive
# P(disease | positive ) = P(disease)*P(positive | disease)/ P(positive)

P_disease_positive = P_disease*P_positive_given_disease/P_positive
P_disease_positive

# prevalence of disease in people who test positive to the overall prevalence of disease
#First calculate the probability of having the disease given a positive test, then divide by the probability of having the disease

prevalence = P_disease_positive/P_disease
prevalence








#write code to compute conditional probabilities for being male 
#in the heights dataset. Round the heights to the closest inch. 
#Plot the estimated conditional probability P(x) = P(Male|height = x ) for each x


library(dslabs)
library(dplyr)
library(ggplot2)

data("heights")
heights %>% mutate(height = round(height)) %>% group_by(height) %>% summarize(p = mean(sex == "Male")) %>%
qplot(height, p, data =.)


#In the plot we just made in Q6 we see high variability 
#for low values of height. This is because we have few data points. 
#This time use the quantile 0.1 ... 0.9 and the cut() function to assure each 
#group has the same number of points.


ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)


#generate data from a bivariate normal distrubution using the MASS package

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)

