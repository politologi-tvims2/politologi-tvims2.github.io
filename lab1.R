### ТВиМС 2019 ###
### Задания из семинара 1 ###

### Binomial distribution ### 

### Задание 2 ### 
choose(6, 2)*0.1^2*0.9^4 + choose(6, 1)*0.1^1*0.9^5 + choose(6, 0)*0.1^0*0.9^6
pbinom(2, size = 6, prob = 0.1) 

data1 <- data.frame(unemployed = 0:6, prob = dbinom(0:6, 6, 0.1))
data1
install.packages("ggplot2")
library(ggplot2)
ggplot(data1, aes(unemployed, prob)) + geom_bar(stat = "identity", colour = 'red', fill = 'yellow')+geom_text(aes(label=round(prob, 2)))

### Задание 3 ###
1 - choose(6, 0)*(10/36)^0*(26/36)^6
pbinom(1, 6, 10/36, lower.tail = FALSE) + dbinom(1, 6, 10/36)

### Poisson distribution ###
### Задание 4 ###
## 4.1 
exp(1)^(-4)*4^3/factorial(3)
dpois(x = 3, lambda = 4)
data2 <- data.frame(scores = 0:10, prob = dpois(0:10, 4)) # могли бы взять и больше 10, это для иллюстрации
data2
ggplot(data2, aes(scores, prob)) + geom_bar(stat = "identity", colour = 'red', fill = 'violet')+geom_text(aes(label=round(prob, 2)))

## 4.2 
ppois(q = 3, lambda = 4, lower.tail = TRUE) - dpois(x = 3, lambda = 4)

#####################################################################################
### РУБРИКА Сделай своими руками ###
### Задание 5 ###

### Задание 6 ###

### Задание 7 ###

#####################################################################################
### Compare Poisson and Binomial distribution ###
n =    ### try different sample sizes
prob =  ### and different probabilities
data3 <- data.frame(k = 0:n, Poisson_dist = dpois(x = 0:n, lambda = n*prob),
                 Binomial_dist = dbinom(x = 0:n, size = n, p = prob))
data3
install.packages("tidyr")
library(tidyr)
data4 <- gather(data3, "Dist", "Probability", -k)
data4
ggplot(data4, aes(x = k, y = Probability, fill = Dist)) +
  geom_col(position = "dodge") + labs(x = "the number of events", y = "Probability")




