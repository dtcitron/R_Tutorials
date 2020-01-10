# Borrowing from: https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/

library(ggplot2)
library(foreign)
library(nnet)
library(reshape2)
library(data.table)

# Data from Wikipedia
# https://en.wikipedia.org/wiki/Logistic_regression
passing.test.scores = data.table(hours = c(0.50,	0.75,	1.00,	1.25,	1.50,	1.75,	1.75,	2.00,	2.25,	2.50,	2.75,	3.00,	3.25,	3.50,	4.00,	4.25,	4.50,	4.75,	5.00,	5.5),
           pass = c(0, 0,	0, 0,	0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1),
           pol = rep(c(0,1), 10)
)
passing.test.scores$pol <- as.factor(passing.test.scores$pol)
passing.test.scores$pol


mylogit <- glm(pass ~ hours, data = passing.test.scores, family = "binomial")
summary(mylogit)


x <- seq(0,6,.1)
plot(passing.test.scores$hours, passing.test.scores$pass)
lines(x, 1/(1 + exp(-(mylogit$coefficients[[1]] + mylogit$coefficients[[2]]*x))))
