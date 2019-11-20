# Seminar 7
# problem 1
values <- c(60,102,41,200)
rows <- c("A1", "A2")
columns <- c("B1", "B2") 
A <- matrix(values, nrow=2, ncol=2, byrow=TRUE, dimnames=list(rows, columns))
A
chi <- chisq.test(A, correct = FALSE)
chi
chi$expected
chi$stdres

#problem 2
# Hint: Fisher distribution. Distribution functions and quantiles
# pf(q, df1, df2)
# qf(p, df1, df2)

# problem 3
# open "price.dta"
install.packages("haven")
install.packages("psych")
library(haven)
library(psych)
price <- read_dta(file.choose())
var(price$price)
var.test(price ~ okrug, data = price, alternative = "two.sided") 

describe(subset(price, okrug == 1)$price)
describe(subset(price, okrug == 2)$price)
var(subset(price, okrug == 1)$price)
var(subset(price, okrug == 2)$price)
