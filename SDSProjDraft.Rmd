# Load all the necessary libraries
library(ggplot2)
library(dplyr)
##**Need this first
gestation_lm <- lm

#Another try
library(mosaicData)
data(Gestation)

gestation_lm <- lm(wt ~ smoke + parity + marital + age, data = Gestation)

summary(gestation_lm)

#0# Introduction - Data from the Child Health and Development Studies
  #The dataset we'll be working with is the Gestation dataset from 
  #the mosaicData package in R. This dataset contains information 
  #on the gestational age and birth weights of babies, along with 
  #various maternal characteristics. The data was collected from a 
  #study on the effects of smoking during pregnancy.

#*Outcome Variable:
  #Birth Weight (bwt): This is a numeric variable representing the 
    #birth weight of the baby in grams.

#Predictors of Interest:
  #Gestational Age (gestation): Numeric variable representing the length of the pregnancy in days.
  #Maternal Age (age): Numeric variable representing the age of the mother in years.
  #Maternal Weight (wt): Numeric variable representing the weight of the mother in pounds.
  #Smoking Status (smoke): Categorical variable indicating whether the mother smoked during pregnancy (1 = smoker, 0 = non-smoker).
  #Race (race): Categorical variable indicating the race of the mother (1 = White, 2 = Black, 3 = Other).

#Dataset Details:
  #Source: The data is part of the mosaicData package in R, which includes datasets for teaching and learning statistics.
  #Observations: The dataset contains 1236 rows, each representing a unique observation.
  #Observations per Group:
  #Smoking Status: There are 715 non-smokers and 521 smokers.
  #Race: The distribution is 849 White, 86 Black, and 301 Other.
  #Interest in the Dataset: This dataset is particularly interesting because it allows us to explore the impact of various maternal factors on birth outcomes. Understanding these relationships can provide valuable insights into prenatal care and public health strategies to improve birth outcomes


#1#
#* Hypothesis 1: Maternal Smoking and Birth Weight
   #*Informal Statement: Mothers who smoke during pregnancy tend to have babies with lower birth weights compared to mothers who do not smoke.
   #*Question: Does smoking during pregnancy affect the birth weight of the baby?
   #*Claim: Smoking during pregnancy is associated with lower birth weights.

#*Hypothesis 2: Marital Status and Maternal Age
  #Informal Statement: Married mothers tend to be older compared to unmarried mothers.
  #Question: Is there a relationship between marital status and maternal age?
  #Claim: Married mothers are generally older than unmarried mothers.

#*Hypothesis 3: Maternal smoking and Gestational Periods
  #Informal Statement: Mothers who smoke during pregnancy tend to have shorter gestational periods
  #Question: How does smoking during pregnancy influence birth outcomes in gestation period?
  #Claim: Higher family income is positively correlated with gestational periods.

#*# More posible hypothesis
#*The number of cigarettes smoked per day affects the maternal weight during pregnancy.
    # Boxplot of maternal weight by smoking
ggplot(Gestation, aes(x = number, y = wt.1)) +
  geom_boxplot() +
  labs(title = "Maternal Weight by Number of Cigarettes",
       x = "Number of Cigarettes",
       y = "Maternal Weight")
plot()
  
#*The effect of the number of cigarettes smoked per day on birth weight differs
#*The duration of smoking during pregnancy affects the gestational age of the baby.
#*The number of cigarettes smoked per day is negatively correlated with birth weight.
#*Parity (total number of previous pregnancies) is a significant predictor of birth weight

#2#
# Boxplot of birth weight by maternal smoking status
ggplot(Gestation, aes(x = smoke, y = wt)) +
  geom_boxplot() +
  labs(title = "Birth Weight by Maternal Smoking Status",
       x = "Maternal Smoking Status",
       y = "Birth Weight")

# Boxplot of maternal age by marital status
ggplot(Gestation, aes(x = marital, y = age)) +
  geom_boxplot() +
  labs(title = "Maternal Age by Marital Status",
       x = "Marital Status",
       y = "Maternal Age")

# Boxplot of gestation period by maternal smoking status
ggplot(Gestation, aes(x = smoke, y = gestation)) +
  geom_boxplot() +
  labs(title = "Gestation Period by Maternal Smoking Status",
       x = "Maternal Smoking Status",
       y = "Gestation Period (days)")

#Hypothesis 1: Smoking during pregnancy is associated with lower birth weights.
#Boxplot: Smoking Status vs. Birth Weight
ggplot(Gestation, aes(x = smoke, y = wt)) +
  geom_boxplot() +
  labs(title = "Smoking Status vs. Birth Weight",
       x = "Smoking Status",
       y = "Birth Weight (oz)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Hypothesis 2: The number of cigarettes smoked per day affects the maternal weight during pregnancy.
#Scatterplot: Number of Cigarettes Smoked per Day vs. Maternal Weight During Pregnancy
ggplot(Gestation, aes(x = number, y = wt.1)) +
  geom_point() +
  labs(title = "Number of Cigarettes Smoked per Day vs. Maternal Weight During Pregnancy",
       x = "Number of Cigarettes Smoked per Day",
       y = "Maternal Weight During Pregnancy (lbs)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Hypothesis 3: Mothers who smoke during pregnancy tend to have shorter gestation periods.
#Boxplot: Smoking Status vs. Gestational Period
ggplot(Gestation, aes(x = smoke, y = gestation)) +
  geom_boxplot() +
  labs(title = "Smoking Status vs. Gestational Period",
       x = "Smoking Status",
       y = "Gestational Period (days)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Hypothesis 4: Parity (total number of previous pregnancies) is influenced by marital status.
#Boxplot: Marital Status vs. Parity
ggplot(Gestation, aes(x = marital, y = parity)) +
  geom_boxplot() +
  labs(title = "Marital Status vs. Parity",
       x = "Marital Status",
       y = "Parity (Number of Previous Pregnancies)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Load necessary libraries
library(ggplot2)

# Scatterplot: Smoking Status vs. Birth Weight
ggplot(Gestation, aes(x = smoke, y = bwt)) +
  geom_point() +
  labs(title = "Smoking Status vs. Birth Weight",
       x = "Smoking Status",
       y = "Birth Weight (oz)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##Which of your predictors seems to be most highly correlated with the 
#outcome of interest? Why do you think this is true?
  #* Marital status vs. parity

#3#
# Fit the linear regression model
model <- lm(parity ~ marital, data = Gestation)

# Print the summary of the model
summary(model)

# Perform the overall F-test
anova_result <- anova(model)

# Print the result
print(anova_result)

#can also try
glance(model)

#4#
library(ggfortify)
autoplot(model)

#*ALL assumption violated

#remove NA


#Transforming

## make log medical expenditure
Gestation$logexp <- log(Gestation$parity)
gestation_parity_log_lm <- lm(logexp ~ marital, data = Gestation)

autoplot(gestation_parity_log_lm)


#*RESTARTING*#
# 
gestation_lm <- lm(wt ~ smoke + parity + marital + age, data = Gestation)
gestation_lm <- lm(wt ~ smoke + parity + marital + ed, data = Gestation)
gestation_lm <- lm(wt ~ smoke + number + marital + ed, data = Gestation)
summary(gestation_lm)

boxplot(Gestation$wt ~ Gestation$smoke)
plot(Gestation$parity, Gestation$wt)
boxplot(Gestation$wt ~ Gestation$marital)
plot(Gestation$age, Gestation$wt)
boxplot(Gestation$wt ~ Gestation$ed)

#try as boxplots
boxplot(Gestation$wt ~ Gestation$age)
boxplot(Gestation$wt ~ Gestation$parity)

plot(Gestation$parity, Gestation$smoke)

autoplot(gestation_lm)

#hypothesis options ?
#People who only have a High School degree have babies with lower birth weight
# I could do something based on the number of cigarettes thye smoke
# Smoking causes lower birth weights
