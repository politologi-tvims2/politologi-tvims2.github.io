### Theory of probability and Mathematical Statistics. Lab 3

### Maximum likelihood: let us illustrate the key idea
N = 10 # you can put your own value
K = 6 # you can put your own value
p<-seq(0.1, 0.95, 0.01)
L <-K*log(p) + (N-K)*log(1-p)
df <- data.frame(L, p)
p[which.max(L)]
ggplot(df, aes(p,L))+geom_line(size=1)+xlab("Probability") +
  ylab("Likelihood")+
  geom_vline(xintercept = p[which.max(L)], color="red")

### Chi-square distribution
sample1 <- rnorm(1000, mean = 0, sd = 1)
sample2 <- rnorm(1000, mean = 0, sd = 1)
sample3 <- rnorm(1000, mean = 0, sd = 1)

chisquare1 <- sample1^2
chisquare2 <- sample2^2
chisquare3 <- sample3^2

hist(chisquare3, breaks = 50, prob=TRUE) 
curve(dchisq(x, df = 1), col='red', add=TRUE) 

hist(chisquare2+chisquare1+chisquare3, breaks = 50, prob=TRUE) 
curve(dchisq(x,df = 3), col='red', add=TRUE) 

### Probabilities and quantiles for chi-square
# pchisq(quantile, df) 
# qchisq(probability, df)

###Задание 2 из Семинара 5 - сделай самостоятельно в R
