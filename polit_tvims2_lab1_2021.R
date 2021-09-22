### Theory of Probability and Mathematical Statistics, 2021 ###
### Lab 1 ###

### Binomial distribution ### 

### Problem 2 (Seminar 1) ### 
# Уровень безработицы среди взрослого населения в некотором крупном городе страны составляет 10%. Какова вероятность того, что среди 6 случайно
# отобранных жителей этого города не более 2 человек окажутся безработными?
choose(6, 2)*0.1^2*0.9^4 + choose(6, 1)*0.1^1*0.9^5 + choose(6, 0)*0.1^0*0.9^6
pbinom(2, size = 6, prob = 0.1) 

# plot the distribution
data1 <- data.frame(unemployed = 0:6, prob = dbinom(0:6, 6, 0.1))
install.packages("ggplot2")
library(ggplot2)
ggplot(data1, aes(unemployed, prob)) + geom_bar(stat = "identity", colour = 'red', fill = 'yellow')+geom_text(aes(label=round(prob, 2)))

### Poisson distribution ###
### Problem 1 (Seminar 2) ###
### Количество голов, забитой футбольной командой N соперникам, имеет распределение Пуассона. Известно, что в среднем за один тайм (45 минут)
### команда N забивает 2 гола.
### Найдите вероятность того, что за футбольный матч (90 минут) команда N
### забьет 3 мяча?
exp(-4)*4^3/factorial(3)
dpois(x = 3, lambda = 4)

# plot the distribution
data2 <- data.frame(scores = 0:10, prob = dpois(0:10, 4)) # let us take N = 10 for illustration purposes
data2
ggplot(data2, aes(scores, prob)) + 
  geom_bar(stat = "identity", colour = 'red', fill = 'violet') +
  geom_text(aes(label=round(prob, 2))) +
  scale_x_continuous(breaks=seq(0,10,1))

# Найдите вероятность того, что за футбольный матч (90 минут) команда N
# забьет менее 3 голов?
ppois(q = 2, lambda = 4)

#####################################################################################
### Practice makes perfect ###
### Problem 2 (Seminar 2) ###
### В среднем на 4 страницы текста книги приходится 1 опечатка. Найдите вероятность того, что на одной странице текста книги встретится более одной
### опечатки, если сл. в. «число опечаток в книге» имеет приближенно распределение
### Пуассона?

### Problem 3 (Seminar 2) ###
### Вероятность того, что пассажир не придет к отправлению автобуса, равна 0.01. Найдите вероятность того, что 2 из 30 пассажиров не придут к
### отправлению автобуса. Сравните ответы, полученные с помощью биномиального
### распределения и распределения Пуассона.

#####################################################################################
### Compare Poisson and Binomial distribution ###
n =  ### try different sample sizes
prob =  ### and different probabilities
data3 <- data.frame(k = 0:n, Poisson_dist = dpois(x = 0:n, lambda = n*prob),
                 Binomial_dist = dbinom(x = 0:n, size = n, p = prob))
head(data3)
install.packages("tidyr")
library(tidyr)
data4 <- gather(data3, "Dist", "Probability", 2:3)
data4
ggplot(data4, aes(x = k, y = Probability, fill = Dist)) +
  geom_col(position = "dodge") + labs(x = "the number of events", y = "Probability")

#####################################################################################

