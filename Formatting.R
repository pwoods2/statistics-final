#0# Introduction

## Introduce your dataset and each of your outcomes and predictors of interest.

# Where did you find your data?

# What are each of the variables measuring?

# How many observations are there?

# How many observations are there per group for your categorical/binary variables?

# What makes this dataset interesting to you?

#1# Formulation of Hypotheses

## Based on the the data you have chosen to analyze, specify at least three scientific hypotheses
## you are interested in assessing.

# 1. 
# 2. 
# 3.

#2# Exploring Marginal Relationships in the Data

## Use visualizations to visually assess the relationships between each of your predictors and your
## numeric outcome.

# Libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(mosaicData)
library(ggfortify)

# Data
data(Gestation)

# Data Cleaning
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

# Maternal Age vs Gestation Period
autoplot(
  lm(gestation ~ age, data = Gestation_edits), 
  which = 1:2, 
  ncol = 2
) +
  labs(title = "Maternal Age vs Gestation Period")

# Smoking Status vs Gestation Period (Categorical)
autoplot(
  lm(gestation ~ smoke, data = Gestation_edits), 
  which = 1:2, 
  ncol = 2
) +
  labs(title = "Gestation Period by Smoking Status")

# Gestation Period vs Birth Weight
autoplot(
  lm(wt ~ gestation, data = Gestation_edits), 
  which = 1:2, 
  ncol = 2
) +
  labs(title = "Gestation Period vs Birth Weight")

# Which is most highly correlated with the outcome of interest?

# Why do you think this is true?

#3# Fitting the Model and Assessing the Overall Model Significance

## Fit a multiple linear regression model to predict your chosen outcome from the set you chose.
## Interpret the estimates of the slope in the model.

# How do the variables apear to relate to the outcome?

# Perform the overall F-test for model significance; state the null and alternative hypotheses for 
# this test and the conclusion from the result.

# Interpret the Multiple-R^2

# Relate the residual standard error to the accuracy of your model in forming its predictions; how 
# close are they to the actual?

#4# Improving the Model

## Use the diagnostic plots to assess whether any of the assumtions underlying the linear regression
## model are violated.

# Are the errors zero on average for all fitted values?

# Do they have constant variance?

# Are the errors normally distributed?

# Use a transformation of either the outcome or the predictors to fix it.

# Reassess the diagnostic plots. (Mean-zero, then constant variance, then normality)

# Hypothesize the existence of an interaction, then check to see if it is justified (fit a model).

# Interpret the interaction, regardless of its statistical significance.

#5# Testing Your Hypotheses

# Formalize the hypotheses from Section 1 in terms of null and alternative.

# Perform these hypothesis tests using the output of the usual R functions (summary, anova, drop1).

# State the conclusion of each test using a cutoff of 0.05 for the P-value.

# Relate the results back to your informal hypotheses (section 1). Do the results support or fail them?

#6# Assessing the Robustness of Your Conclusion

# Revisit the output of autoplot to check for highly influential points.

# If any: what makes them high leverage, and what happens if you delete them?

# Are there any potential issues with multicolinearity of predictors? Compute variance inflation factors
# for each predictor to test this.

# Use cross-validation to estimate the leave-one-out prediction error.

# Comment if there is/is not evidence that your model is overfit.

#7# Automatic Model Selection

# Perform one of the following techniques: forward selection, backward selection, or all subsets. Why?

# Comment on the limitations of the automatic model selection: whatt impact does it have on P-values?


#8# Conclusion

# Summarize what you have learned about your dataset after engaging in this process.

# Which conclusions were most interesting/relevant to you?

# Suggest some future avenues for research on this topic.
