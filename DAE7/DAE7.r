#!/usr/bin/env Rscript
set.seed(123)
library(scales)
library(nortest)
library(R.matlab)

se <- function(x) {
    sqrt(sd(x)/length(x))
}

cohens_d <- function(x, y) {
    lenX <- length(x) - 1
    lenY <- length(y) - 1
    meanDiff <- abs(mean(x) - mean(y))
    csd <- lenX * var(x) + lenY * var(y)
    csd <- csd/(lenX + lenY)
    csd <- sqrt(csd)
    
    cd <- meanDiff/csd
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
png('DAE7-1a.png')
hist(chinese$x.ch, xlim=c(4, 16), ylim=c(0, 14), xlab='x.ch & x.us',  main = paste("Histogram of", 'x.ch and x.us'), col = 'skyblue')
hist(chinese$x.us, add=T, col = scales::alpha('red', .4))
legend("topright", c("x.ch", "x.us"), fill=c(scales::alpha('skyblue', .8), scales::alpha('red', .8)))
dev.off()
# b
png('DAE7-1b1.png')
qqnorm(chinese$x.ch)
qqline(chinese$x.ch)
dev.off()
shapiro.test(chinese$x.ch)

png('DAE7-1b2.png')
qqnorm(chinese$x.us)
qqline(chinese$x.us)
dev.off()
shapiro.test(chinese$x.us)

# c
# H0 = Chinese speaking are equally good singers as american
# H1 = Chinese speaking are better singers than american
# Two sample
# Independent I guess
# Dunno about no tails
# Yes, H0 rejected, mean is bigger.

ttest <- t.test(chinese$x.ch, chinese$x.us, alternative='greater', conf.level=0.99, mu=10.49732)
ttest

theD <- cohens_d(chinese$x.ch, chinese$x.us)
theD









