### Regression analysis. Lab 3
### Multiple regression. Heteroskedasticity
### February 26, 2021

install.packages("haven")
install.packages("ggplot2")
install.packages("sandwich")
install.packages("lmtest")

library(haven)
library(ggplot2)
library(sandwich)
library(lmtest)

# open reg_lab1.dta
lab3 <- read_dta(file.choose())

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

############################ Heteroskedasticity ###############################

# run the model of lngdp2 on regulatory quality and control of corruption
lab3 <- dplyr::select(lab3, lngdp2, rq, cc)
lab3 <- na.omit(lab3)
m1 <- lm(lngdp2 ~ rq + cc, data = lab3)
summary(m1)
vcov(m1)

# visualize the relationship between predictors and Y 
ggplot(lab3, aes(rq, lngdp2)) + geom_point()
ggplot(lab3, aes(cc, lngdp2)) + geom_point() 
ggplot(lab3, aes(m1$fitted.values, lngdp2)) + geom_point()

# visualize the relationship between squared residuals and predictor variables
ggplot(lab3, aes(rq, m1$residuals^2)) + geom_point()
ggplot(lab3, aes(cc, m1$residuals^2)) + geom_point()
ggplot(lab3, aes(m1$fitted.values, m1$residuals^2)) + geom_point()

# formal tests: 
bptest(m1) # Breusch-Pagan test
bptest(m1, varformula = ~ cc + I(cc^2) + rq + I(rq^2) + cc:rq, data = lab3) # White test
gqtest(m1, order.by = ~cc, data = lab3, fraction = 0.2, alternative = "less") # Goldfeld-Quandt test: we suggest that variance of residuals depends on control of corruption 
gqtest(m1, order.by = ~rq, data = lab3, fraction = 0.2, alternative = "less") # Goldfeld-Quandt test: we suggest that variance of residuals depends on regulatory quality

# adjusted standard errors (heteroskedasticity-consistent)
vcovHC(m1)
coeftest(m1, vcov=vcovHC(m1)) # HC3 by default 

vcovHC(m1, "HC0")
coeftest(m1, vcov=vcovHC(m1, "HC0")) # White standard errors - asymptotics

