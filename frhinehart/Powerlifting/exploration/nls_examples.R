library(tidyverse)
powerlift <- read.csv(here::here("smshel21/powerlifting/module/data/powerlift_sample_data_updated.csv"))

# Define shifted gamma function
#shifted_gamma <- function(x, a, b, x0) {
#  x_shift <- pmax(x - x0, 0)
#  a * x_shift * exp(-b * x_shift)
#}

## Fit with nls
#nls_fit <- nls(
#  Best3DeadliftKg ~ shifted_gamma(Age, a, b, x0),
#  data = powerlift,
#  start = list(a = 300, b = 0.05, x0 = 10)
#)


## Define the shifted gamma-like function
#shifted_gamma <- function(x, a, b, x0) {
#  x_shifted <- pmax(x - x0, 0)
#  a * x_shifted * exp(-b * x_shifted)
#}

powerlift8000 = powerlift

powerlift <- powerlift8000 %>% drop_na(Age, Best3DeadliftKg)

## Fit the model using nls
#nls_fit <- nls(
#  Best3DeadliftKg ~ shifted_gamma(Age, a, b, x0),
#  data = powerlift,
#  start = list(a = 300, b = 0.05, x0 = 10)
#)

# Add predicted values to your dataset
#powerlift$pred <- predict(nls_fit)

# Plot actual vs predicted
library(ggplot2)

#ggplot(powerlift, aes(x = Age)) +
#  geom_point(aes(y = Best3DeadliftKg), 
#             alpha = 0.3, color = "gray40") +
#  geom_smooth(aes(y = Best3DeadliftKg), color = "blue", fill = "blue") +
#  geom_line(aes(y = pred), color = "red", linewidth = 1.2) +
#  labs(
#    title = "Fitted Shifted Gamma Curve to Deadlift vs. Age",
#    x = "Age",
#    y = "Best 3 Deadlift (kg)"
#  ) +
#  theme_minimal()


############################
# Define the reparameterized double exponential with x_peak
double_exp_peak <- function(x, A, k1, k2, x_peak) {
  x_shift <- pmax(x - x_peak, 0)
  A * exp(-k1 * x_shift) * (1 - exp(-k2 * x_shift))
}

# Fit the model
nls_fit <- nls(
  Best3DeadliftKg ~ double_exp_peak(Age, A, k1, k2, x_peak),
  data = powerlift,
  start = list(A = 300, k1 = 0.02, k2 = 0.1, x_peak = 27)
)

# Predicted values
powerlift$pred <- predict(nls_fit)

# Plot
library(ggplot2)

ggplot(powerlift, aes(x = Age)) +
  geom_point(aes(y = Best3DeadliftKg), alpha = 0.3, color = "gray40") +
  geom_smooth(aes(y = Best3DeadliftKg), color = "blue", fill = "blue") +
  geom_line(aes(y = pred), color = "darkgreen", linewidth = 1.2) +
  labs(
    title = "Double Exponential with Explicit Peak Parameter",
    subtitle = "Estimated peak location is directly modeled",
    x = "Age",
    y = "Best 3 Deadlift (kg)"
  ) +
  theme_minimal()



confint(nls_fit, "x_peak")
coef(nls_fit)

########################

# Weibull-like function
weibull_like <- function(x, a, b, k, x0) {
  x_shifted <- pmax(x - x0, 0)
  a * (x_shifted^k) * exp(-b * x_shifted)
}

# Fit the model
nls_weibull <- nls(
  Best3DeadliftKg ~ weibull_like(Age, a, b, k, x0),
  data = powerlift,
  start = list(a = 1, b = 0.05, k = 2, x0 = 5)
)

# Get fitted values
powerlift$weibull_pred <- predict(nls_weibull)

# Plot
library(ggplot2)

ggplot(powerlift, aes(x = Age)) +
  geom_point(aes(y = Best3DeadliftKg), alpha = 0.3, color = "gray40") +
  geom_smooth(aes(y = Best3DeadliftKg), color = "blue", fill = "blue") +
  
  geom_line(aes(y = weibull_pred), color = "purple", linewidth = 1.2) +
  labs(
    title = "Weibull-Like Model Fit to Deadlift vs. Age",
    subtitle = expression(y == a %.% (x - x[0])^k %.% e^{-b %.% (x - x[0])}),
    x = "Age",
    y = "Best 3 Deadlift (kg)"
  ) +
  theme_minimal()


########################

# Reparameterized difference-of-exponentials function
diffexp_reparam <- function(x, a, b, d, x_max) {
  a * (exp(b * x) - (b / d) * exp(d * (x - x_max)))
}

# Fit with nls
library(minpack.lm)

nls_reparam <- nlsLM(
  Best3DeadliftKg ~ diffexp_reparam(Age, a, b, d, x_max),
  data = powerlift,
  start = list(a = 100, b = 0.01, d = 0.1, x_max = 30),
  control = nls.lm.control(maxiter = 200)
)
# Add predicted values
powerlift$reparam_pred <- predict(nls_reparam)

# Plot
library(ggplot2)

ggplot(powerlift, aes(x = Age)) +
  geom_point(aes(y = Best3DeadliftKg), alpha = 0.3, color = "gray40") +
  geom_smooth(aes(y = Best3DeadliftKg), color = "blue", fill = "blue") +
  geom_line(aes(y = reparam_pred), color = "steelblue", linewidth = 1.2) +
  labs(
    title = "Reparameterized Difference of Exponentials",
    subtitle = expression(y == a %.% (e^{b %.% x} - (b / d) %.% e^{d %.% (x - x[max])})),
    x = "Age",
    y = "Best 3 Deadlift (kg)"
  ) +
  theme_minimal()

summary(nls_reparam)
