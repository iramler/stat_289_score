library(tidyverse)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  
  # Smooth spline quantile regression
  geom_quantile(
    quantiles = c(0.1, 0.9),
    method = "rqss",
    lambda = 0.1,
    color = "red",
    linewidth = 1
  ) +
  
  # Polynomial quantile regression (2nd degree)
  geom_quantile(
    quantiles = c(0.1, 0.9),
    method = "rq",
    formula = y ~ poly(x, 2),
    color = "blue",
    linewidth = 1,
    linetype = "dashed"
  ) 
