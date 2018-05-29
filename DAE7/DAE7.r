#!/usr/bin/env Rscript
set.seed(123)
library(scales)
library(nortest)
library(R.matlab)
library(reshape2)
library(ggplot2)

se <- function(x) {
    sqrt(sd(x)/length(x))
}

sem <- function(x){
  sd(x)/sqrt(length(x))
}

cohens_d <- function(x, y) {
    lenX <- length(x) - 1
    lenY <- length(y) - 1
    meanDiff <- abs(mean(x) - mean(y))
    csd <- lenX * var(x) + lenY * var(y)
    csd <- csd/(lenX + lenY)
    csd <- sqrt(csd)
    print("Cohen's D")
    print(cd <- meanDiff/csd)
}

# 1
lambda <- 2
mu <- 1/lambda
N <- 40

# a
chinese <- readMat('ChineseEnglish.mat')
data <- data.frame(ch=chinese$x.ch, us=chinese$x.us)

print('Chinese mean and standard error')
mean(data$ch)
se(data$ch)
print('American mean and standard error')
mean(data$us)
se(data$us)
png('DAE7-1a.png')
hist(data$ch, xlim=c(4, 16), ylim=c(0, 14), xlab='data$ch & data$us',  main = paste("Histogram of", 'data$ch and data$us'), col = 'skyblue')
hist(data$us, add=T, col = scales::alpha('red', .4))
legend("topright", c("data$ch", "data$us"), fill=c(scales::alpha('skyblue', .8), scales::alpha('red', .8)))
dev.off()
# b
# Interval level of measurement
png('DAE7-1b1.png')
qqnorm(data$ch, col='red', pch=3)
qqline(data$ch)
dev.off()
shapiro.test(data$ch)

png('DAE7-1b2.png')
qqnorm(data$us, col='red', pch=3)
qqline(data$us)
dev.off()
shapiro.test(data$us)

# c
# H0 = Chinese speaking are equally good singers as american
# H1 = Chinese speaking are better singers than american
# Two sample
# Independent I guess
# Dunno about no tails
# Yes, H0 rejected, mean is bigger.

ttest <- t.test(data$ch, data$us, alternative='greater', conf.level=0.99, mu=10.49732)
ttest

# d
# theD = 0.807 = big effect
theD <- cohens_d(data$ch, data$us)

# f
sme <- apply(data, 2, mean)
ssem <- apply(data, 2, sem)
sme_ssem <- data.frame(mean = sme, sem = ssem, group=names(data))

png('DAE7-1f.png')
ggplot(sme_ssem, aes(x = group, y = mean)) + geom_bar(stat = 'identity') + geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .2, col='red') + xlab('Means') + ylab('Value')
dev.off()

# 2
weights <- readMat('weights.mat')
weights <- data.frame(before = weights$w.before, after = weights$w.after)

# a
before.mean <- mean(weights$before)
after.mean <- mean(weights$after)

before.sd <- sd(weights$before)
after.sd <- sd(weights$after)

png('DAE7-2a.png')
hist(weights$before, xlim=c(60, 120), ylim=c(0, 10), xlab='weights$before & weights$after',  main = paste("Histogram of", 'weights$before and weights$after'), col = 'skyblue')
hist(weights$after, add=T, col = scales::alpha('red', .4))
legend("topright", c("weights$before", "weights$after"), fill=c(scales::alpha('skyblue', .8), scales::alpha('red', .8)))
dev.off()

# b
# Interval level of measurement I guess
png('DAE7-2b1.png')
qqnorm(weights$before, col='red', pch=3)
qqline(weights$before)
dev.off()

png('DAE7-2b2.png')
qqnorm(weights$after, col='red', pch=3)
qqline(weights$after)
dev.off()
# Both look like they come from a normal distribution
shapiro.test(weights$before) # w = 0.95608 p = 0.3419
shapiro.test(weights$after)  # w = 0.95425 p = 0.3119
print('Before.mean')
before.mean # 92.42223
print('After mean')
after.mean # 93.82223
# H0 = The weights will not have changed
# H1 = The weights will have decreased
t.test(weights$before, weights$after, pair=T, alternative='less', conf.level=.95)
# Do paired test to see if mean difference proofs weight loss.
# Dependent as the samples are from the same people.
# Can't reject null hypothesis based on t test, as t = -2.1433 and p-value = 0.02122, but can't accept alternative hypothesis either.
# Maybe reformulate null hypothesis?










