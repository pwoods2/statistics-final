
# 3. Fitting the Model and Assessing Significance

# Fit a multiple linear regression model
gestation_lm <- lm(gestation ~ wt + smoke + age, data = Gestation_edits)

# Summary of the model
summary(gestation_lm)

# Diagnostic plots for the model
autoplot(gestation_lm)

# 4. Improving the Model

# Check assumptions using residuals
hist(residuals(gestation_lm), main = "Histogram of Residuals", xlab = "Residuals")
shapiro.test(residuals(gestation_lm))

# Try log transformation of gestation period
Gestation_edits$log_gestation <- log(Gestation_edits$gestation)

# Fit a new model with log-transformed outcome
gestation_log_lm <- lm(log_gestation ~ wt + smoke + age, data = Gestation_edits)

# Diagnostics for improved model
autoplot(gestation_log_lm)
summary(gestation_log_lm)

# 5. Robustness of Conclusions

# Identify influential points
influential_points <- which(cooks.distance(gestation_lm) > (4 / nrow(Gestation_edits)))

# Remove influential points and refit the model
Gestation_no_influential <- Gestation_edits[-influential_points, ]
model_no_influential <- lm(gestation ~ wt + smoke + age, data = Gestation_no_influential)

# Summary of the updated model
summary(model_no_influential)

# Diagnostics for the updated model
autoplot(model_no_influential)

# 6. Variance Inflation Factors (VIFs)
library(car)
vif(gestation_lm)

# 7. Automatic Model Selection
step_model <- step(gestation_lm, direction = "both")
summary(step_model)

