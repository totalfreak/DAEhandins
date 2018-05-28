#!/usr/bin/env Rscript
set.seed(123)
library(scales)
library(nortest)
library(R.matlab)

se <- function(x) {
    sqrt(sd(x)/length(x))
}


# 1
lambda <- 2
mu <- 1/lambda
N <- 40

# a
chinese <- readMat('ChineseEnglish.mat')
print('Chinese mean and standard error')
mean(chinese$x.ch)
se(chinese$x.ch)
print('American mean and standard error')
mean(chinese$x.us)
se(chinese$x.us)

hist(chinese$x.ch, xlim=c(4, 16), ylim=c(0, 14),  main = paste("Histogram of", 'x.ch and x.us'), col = 'skyblue')
hist(chinese$x.us, add=T, col = scales::alpha('red', .4))

# b
qqnorm(chinese$x.ch)
qqline(chinese$x.ch)
shapiro.test(chinese$x.ch)


qqnorm(chinese$x.us)
qqline(chinese$x.us)
shapiro.test(chinese$x.us)


# c
# H0 = Chinese speaking are equally good singers as american
# H1 = Chinese speaking are not better singers than american
ttest <- t.test(chinese$x.ch, chinese$x.us, alternative='greater', conf.level=0.99)
ttest


