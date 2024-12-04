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
gestation_lm <- lm(gestation ~ wt + smoke + age, data = Gestation)
summary(gestation_lm)
