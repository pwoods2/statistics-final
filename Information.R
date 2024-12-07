library(ggplot2)
library(dplyr)
library(tidyverse)

install.packages(mosaicData)
library(mosaicData) 
data(Gestation)

Gestation_edits <- select(Gestation, marital, gestation, age, parity, dht, wt, smoke) %>%
  filter(
    !is.na(age),
    !is.na(gestation),
    !is.na(parity),
    !is.na(dht),
    !is.na(wt),
    !is.na(marital),
    !is.na(smoke)
  )


## 1 Age
ggplot(Gestation_edits, aes(x = age, y = gestation)) + 
  geom_point() +
  labs(
    title = "Maternal Age vs Gestation",
    x = "Maternal Age (years)",
    y = "Gestation period (days)"
  ) +
  theme_classic(
  )
  
##  Parity 

ggplot(Gestation_edits, aes(x = parity, y = gestation)) + 
  geom_point() +
  labs(
    title = "Maternal Parity vs Gestation",
    x = "Maternal Parity (number of children)",
    y = "Gestation period (days)"
  ) +
  theme_classic(
  )

## 3 Birth weight

ggplot(Gestation_edits, aes(x = gestation, y = wt)) + 
  geom_point() +
  labs(
    title = "Gestation Period vs Birth Weight",
    x = "Gestation Period (days)",
    y = "Birth Weight (oz)"
  ) +
  theme_classic(
  )


## Paternal Height

ggplot(Gestation_edits, aes(x = dht, y = gestation)) + 
  geom_point() +
  labs(
    title = "Paternal Height vs Gestation Period",
    x = "Paternal Height (in)",
    y = "Gestation Period (days)"
  ) +
  theme_classic(
  )

## Marital Status
ggplot(Gestation_edits, aes(x = marital, y = gestation)) + 
  geom_boxplot() +
  labs(
    title = "Gestation Period vs Birth Weight",
    x = "Gestation period (days)",
    y = "Birth Weight (oz)"
  ) +
  theme_classic(
  )


## 2 Smoking

ggplot(Gestation_edits, aes(x = smoke, y = gestation)) + 
  geom_boxplot() +
  labs(
    title = "Gestation Period vs Smoking Status",
    x = "Smoking Status",
    y = "Gestation period (days)"
  ) +
  theme_classic(
  )

# fitting the model
library(ggfortify)
gestation_lm <- lm(gestation ~ wt + smoke + age, data = Gestation_edits)
summary(gestation_lm)

autoplot(gestation_lm)
hist(residuals(gestation_lm))
shapiro.test(Gestation$gestation)
#p value is small so fail- Q-Q plot violated

# Improving the model
# try and take lower (didnt work)
hist(residuals(gestation_lm))

hist((Gestation$gestation), xlab = "Gestation Period")
hist(sqrt(Gestation$gestation), xlab = "Gestation Period")
hist(log(Gestation$gestation), xlab = "Gestation Period")

## Make log Gestation Period - this works! just talk about the outliers
Gestation$logexp <- log(Gestation$gestation)
Gestation_log_lm <- lm(logexp ~ age + wt + smoke, data = Gestation)

autoplot(Gestation_log_lm)

#Just explain i tried and didn't really work






#5#
model <- lm(gestation ~ wt, data = Gestation_edits)

# Summary of the model
summary(model)

# ANOVA test (1)
anova(model)

# Drop1 test
drop1(model, test = "F")

##Both of these p-cales are small, which means 


#6#
# Load necessary library
library(ggfortify)

# Fit the model
model <- lm(gestation ~ wt + smoke + age, data = Gestation_edits)

# Diagnostic plots
autoplot(model)

# Identify influential points
influential_points <- which(cooks.distance(model) > (4 / nrow(Gestation_edits)))

# Remove influential points
Gestation_no_influential <- Gestation_edits[-influential_points, ]

# Refit the model without influential points
model_no_influential <- lm(gestation ~ wt + smoke + age, data = Gestation_no_influential)
summary(model_no_influential)
