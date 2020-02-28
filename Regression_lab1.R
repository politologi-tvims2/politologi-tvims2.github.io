### Regression analysis. Lab 1
### February 18, 2020

# First, let us brush up the key ideas of ANOVA
install.packages("dplyr")
library(dplyr)

minutes <- c(28,35,30,32,30,25,27,26,27,30,24,27,29,26,24)
group <- rep(1:3, each = length(minutes)/3)
data <- as.data.frame(cbind(minutes, group))
data
group_by(data, group) %>% summarise(mean = mean(minutes), var = var(minutes))

summary(aov(minutes ~ as.factor(group)))

# Second, run pairwise linear regression model
install.packages("haven")
install.packages("psych")
install.packages("ggplot2")
library(haven)
library(psych)
library(ggplot2)

reg <- read_dta(file.choose())
# Description 
# country
# gove - government effectiveness, 2007 (the Worldwide Governance Indicators)
# va - voice and accountability, 2007 (the Worldwide Governance Indicators)
# ps - political stability and absence of violence, 2007 (the Worldwide Governance Indicators)
# rq - regulatory quality, 2007 (the Worldwide Governance Indicators)
# rl - rule of law, 2007 (the Worldwide Governance Indicators)
# cc - control of corruption (the Worldwide Governance Indicators)

head(reg)
lab1 <- select(reg, gove, va)
lab1 <- na.omit(lab1)
describe(lab1)

# Null (empty) model 
m0 <- lm(gove ~ 1, data = lab1)
summary(m0)

# Questions:
# 1) How to derive the intercept in this null (empty) model?  
# 2) Is the intercept coefficient significant or not?

m1 <- lm(gove ~ va, data = lab1)
summary(m1)

# Questions:
# 1) How to derive t-values?
# 2) Calculate p-values by yourself and compare with the values provided in the output
# 3) Are the coefficients significant?
# 4) Interpret the coefficient estimates
# 5) What does R-squared measure show? Interpret its value
# 6) What is the difference between R-squared and adjusted R-squared?
# 7) What does the F-statistic show? Formulate the null hypothesis and interpret the results. 

# Visualize the results
ggplot(data = lab1, aes(x = va, y = gove)) +
  geom_smooth(method="lm",se=F) +
  geom_point() 

m1$coefficients
vcov(m1) # Covariance matrix for the coefficient estimates
sqrt(diag(vcov(m1))) 
# Question: interpret the covariance matrix elements (diagonal + off-diagonal elements)

# Let us derive confidence intervals for the predictor coefficient 
m1$coefficients[2] - qt(0.975, dim(lab1)[1]-length(m1$model))*sqrt(diag(vcov(m1)))[2]
m1$coefficients[2] + qt(0.975, dim(lab1)[1]-length(m1$model))*sqrt(diag(vcov(m1)))[2]
confint(m1)

# ANOVA table
anova(m1)
# What does ESS mean? 
# What about RSS?
# Calculate TSS
# Let us derive TSS, ESS and RSS directly from 
TSS <- var(lab1$gove)*(dim(lab1)[1] - 1)
TSS
ESS <- var(m1$coefficients[1] + m1$coefficients[2]*lab1$va)*(dim(lab1)[1] - 1)
ESS
res <- m1$residuals
RSS <- sum(res^2)
RSS

# R-squared
ESS/TSS
summary(m1)$r.squared
