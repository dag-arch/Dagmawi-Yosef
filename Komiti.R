set.seed(123)
n <- 50
X <- seq(0, 10, length.out = n)
Y <- 2 + 3*X + 0.5*X^2 + sin(X) + rnorm(n, mean = 0, sd = 2)
TT <- data.frame(X = X, Y = Y)
model <- lm(Y ~ X + I(X^2), data = TT)
ggplot(TT, aes(x = X, y = Y)) +
  geom_point(color = "blue", size = 2) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "red") + 
  labs(title = "Observations and Fitted Model",
       x = "X",
       y = "Y") +
  theme_minimal()
summary(model)
coefficients <- coef(model)
p_values <- summary(model)$coefficients[, 4]
r_squared <- summary(model)$r.squared
results_table <- data.frame(
  Parameter = names(coefficients),
  Estimate = coefficients,
  p_value = p_values
)
results_table <- rbind(results_table, data.frame(
  Parameter = "R-squared",
  Estimate = r_squared,
  p_value = NA
))
print(results_table)

