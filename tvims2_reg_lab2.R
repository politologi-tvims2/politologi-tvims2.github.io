### Regression analysis. Lab 2
### Multiple regression. Multicollinearity
### February 19, 2021

install.packages("haven")
install.packages("psych")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("car")
install.packages("memisc")

library(haven)
library(psych)
library(dplyr)
library(ggplot2)
library(GGally)
library(car)
library(memisc)

# open reg_lab1.dta
reg <- read_dta(file.choose())
head(reg)

# Description 
# country
# gove - government effectiveness, 2007 (the Worldwide Governance Indicators)
# va - voice and accountability, 2007 (the Worldwide Governance Indicators)
# ps - political stability and absence of violence, 2007 (the Worldwide Governance Indicators)
# rq - regulatory quality, 2007 (the Worldwide Governance Indicators)
# rl - rule of law, 2007 (the Worldwide Governance Indicators)
# cc - control of corruption, 2007 (the Worldwide Governance Indicators)
# gdp2 - GDP per capita, 2007
# lngdp2 - natural logarithm of GDP per capita, 2007

hist(reg$gdp2) # skewed distribution
hist(reg$lngdp2) 

## prepare your data
lab2 <- dplyr::select(reg, -gdp2)
lab2 <- na.omit(lab2)

describe(lab2)

## run a pairwise regression model
m1 <- lm(gove ~ va, data = lab2)
summary(m1)
# Comment on the relationship between government effectiveness and voice and accountability
# What about (in)significance of the predictor coefficient ? 
# What does the R-squared measure indicate? Is it significant?

## perfect multicollinearity
lab2$va10 = 10*lab2$va
m1_1 <- lm(gove ~ va + va10, data = lab2)
summary(m1_1) 

## the coefficient estimates cannot be calculated. Why is this so?
X <- dplyr::select(lab2, va, va10)
intercept_vector <- rep(1, dim(lab2)[1])
X <- as.matrix(cbind(intercept_vector, X))
X
y <- as.matrix(lab2$gove)

inverse_matrix <- solve(t(X) %*% X) 
det(t(X) %*% X) ## the reason is that the determinant is 0, i.e. we deal with the singular matrix!

## add Rule of law as a predictor
m2 <- lm(gove ~ va + rl, data = lab2)
summary(m2)
# If there are changes as compared to Model 1, please comment on them. What has happened to the effect of voice of accountability?

# derive coefficient estimates by matrix-vector manipulations
X <- dplyr::select(lab2, va, rl)
intercept_vector <- rep(1, dim(lab2)[1])
X <- as.matrix(cbind(intercept_vector, X))

inverse_matrix <- solve(t(X) %*% X)%*%t(X)%*%y
inverse_matrix
# compare the vector elements with the coefficient estimates (in Model 2)

# run a multiple regression model
m3 <- lm(gove ~ va + rl + rq + ps + cc + lngdp2, data = lab2)
summary(m3)
# interpret the coefficient estimates and their significance

# identify the approximate values of Pearson's correlation coefficients
# draw a more detailed graph
lab2
ggpairs(lab2[,-c(5,8,9)])
# correlation heatmap
ggcorr(lab2[,-c(5,8,9)], label = TRUE)
corrmatrix <- cor(lab2[,-c(5,8,9)])
corrmatrix 

## you can also examine partial correlations, however, this is not a guarantee that multicollinearity will occur

## Auxiliary models
# Examine R-squared and their significance
summary(lm(va ~ rl + rq + ps + cc + lngdp2, data=lab2))
summary(lm(rl ~ va + rq + ps + cc + lngdp2, data=lab2))
summary(lm(rq ~ va + rl + ps + cc + lngdp2, data=lab2))
summary(lm(ps ~ va + rl + rq + cc + lngdp2, data=lab2))
summary(lm(cc ~ va + rl + rq + ps + lngdp2, data=lab2))
summary(lm(lngdp2 ~ va + rl + rq + ps + cc, data=lab2))

## Variance inflation factor
rsquared <- seq(0, 1, by = 0.01)
vif_values <- 1/(1 - rsquared)
data <- data.frame(rsquared, vif_values)
ggplot(data, aes(rsquared, vif_values)) + geom_line()

## calculate VIF 
vif <- vif(m3)
vif
1/(1 - summary(lm(va ~ rl + rq + ps + cc + lngdp2, data=lab2))$r.sq)
# Interpret the VIF values
## VIF values higher than 10 indicate severe multicollinearity problems

## or tolerance: the bigger the better
tolerance <- 1/vif
tolerance

## Instability of estimates
lab2$n = seq(dim(lab2)[1])
m3_new <- lm(gove ~ va + rl + rq + ps + cc + lngdp2, data=lab2[lab2$n>10,])
summary(m3_new)

mtable(m3, m3_new)
