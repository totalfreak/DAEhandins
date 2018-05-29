#!/usr/bin/env Rscript
set.seed(123)
library(scales)
library(nortest)
library(R.matlab)
library(reshape2)
library(ggplot2)
library(ggpubr)

sem <- function(x){
  sd(x)/sqrt(length(x))
}

cohens_d <- function(x, y) {
    lenX <- length(x) - 1
    lenY <- length(y) - 1
    meanDiff <- abs(mean(x) - mean(y))
    pssd <- lenX * var(x) + lenY * var(y)
    pssd <- pssd/(lenX + lenY)
    pssd <- sqrt(pssd)
    print("Cohen's D")
    print(cd <- meanDiff/pssd)
}

# 1

# a
chinese <- readMat('ChineseEnglish.mat')
data <- data.frame(ch=chinese$x.ch, us=chinese$x.us)

print('Chinese mean and standard error')
mean(data$ch)
sem(data$ch)
print('American mean and standard error')
mean(data$us)
sem(data$us)
png('DAE7-1a.png')
hist(data$ch, xlim=c(4, 16), ylim=c(0, 14), xlab='data$ch & data$us',  main = paste("Histogram of", 'data$ch and data$us'), col = 'skyblue')
hist(data$us, add=T, col = scales::alpha('red', .4))
legend("topright", c("data$ch", "data$us"), fill=c(scales::alpha('skyblue', .8), scales::alpha('red', .8)))
dev.off()
# b
# Interval level of measurement
png('DAE7-1b1.png')
ggqqplot(data$ch)
dev.off()
shapiro.test(data$ch)

png('DAE7-1b2.png')
ggqqplot(data$us)
dev.off()
shapiro.test(data$us)

# c
# H0 = Chinese speaking are as good singers on average as american speaking
# H1 = Chinese speaking are better on average than american speaking
# Two sample
# Independent I guess
# Dunno about no tails
# Yes, H0 rejected, mean of chinese is bigger.

ttest <- t.test(data$ch, data$us, conf.level=0.99)
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

c(before.mean, before.sd)
c(after.mean, after.sd)

png('DAE7-2a.png')
hist(weights$before, xlim=c(60, 120), ylim=c(0, 10), xlab='weights$before & weights$after',  main = paste("Histogram of", 'weights$before and weights$after'), col = 'skyblue')
hist(weights$after, add=T, col = scales::alpha('red', .4))
legend("topright", c("weights$before", "weights$after"), fill=c(scales::alpha('skyblue', .8), scales::alpha('red', .8)))
dev.off()

# b
# Interval level of measurement I guess
png('DAE7-2b1.png')
ggqqplot(weights$before)
dev.off()

png('DAE7-2b2.png')
ggqqplot(weights$after)
dev.off()
# Both look like they come from a normal distribution
shapiro.test(weights$before) # w = 0.95608 p = 0.3419
shapiro.test(weights$after)  # w = 0.95425 p = 0.3119
print('Before.mean')
before.mean # 92.42223
print('After mean')
after.mean # 93.82223
# c
# H0 = The weights will not have changed
# H1 = The weights will have decreased
t.test(weights$before, weights$after, pair=T, alternative='less', conf.level=.9995)
# Do paired test to see if mean difference proofs weight loss.
# Dependent as the samples are from the same people.
# Reject null hypothesis based on t test, as t = -2.1433 and p-value = 0.02122, but can't accept alternative hypothesis either.
# Maybe reformulate null hypothesis?

# e
sme <- apply(weights, 2, mean)
ssem <- apply(weights, 2, sem)
sme_ssem <- data.frame(mean = sme, sem = ssem, group=names(weights))

png('DAE7-2e.png')
ggplot(sme_ssem, aes(x = group, y = mean)) + geom_bar(stat = 'identity') + geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .2, col='red') + xlab('Means') + ylab('Value')
dev.off()

# 3
heights <- readMat('Dutchdanish.mat')
heights <- data.frame(du=heights$x.du, da=heights$x.da)
# a
du.mean <- mean(heights$du)
da.mean <- mean(heights$da)

du.se <- sem(heights$du)
da.se <- sem(heights$da)

c(du.mean, du.se)
c(da.mean, da.se)

png('DAE7-3a.png')
hist(heights$du, xlab='heights$du & heights$da',  main = paste("Histogram of", 'heights$du and heights$da'), col = 'skyblue')
hist(heights$da, add=T, col = scales::alpha('red', .4))
legend("topright", c("heights$du", "heights$da"), fill=c(scales::alpha('skyblue', .8), scales::alpha('red', .8)))
dev.off()
# b
# Ratio level of measurement I guess
png('DAE7-3b1.png')
ggqqplot(heights$du)
dev.off()

png('DAE7-3b2.png')
ggqqplot(heights$da)
dev.off()

shapiro.test(heights$du) # W = 0.97796 p-value = 0.8418
shapiro.test(heights$da) # W = 0.97478 p-value = 0.7664
# Test shows that both are from a normal distribution"with p-values of 0.8418 & 0.7664
# Both look to be drawn from a normal distribution.

# c
# H0 = Danish and Dutch males aged 20-25 dont' differ in height on average
# H1 = Danish and Dutch males aged 20-25 differ in height on average

# Doing an F test shows that the two samples don't have the same variance, with a F value = 1.356
var.test(heights$du, heights$da, conf.level=.95)

t.test(heights$du, heights$da)
# t = 2.3117 p-value = 0.02523
# H0 rejected

# d
theD <- cohens_d(heights$du, heights$da)
# theD = 0.6538611
# Close to medium effect size

# f
sme <- apply(heights, 2, mean)
ssem <- apply(heights, 2, sem)
sme_ssem <- data.frame(mean = sme, sem = ssem, group=names(heights))

png('DAE7-3f.png')
ggplot(sme_ssem, aes(x = group, y = mean)) + geom_bar(stat = 'identity') + geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .2, col='red') + xlab('Means') + ylab('Value')
dev.off()










