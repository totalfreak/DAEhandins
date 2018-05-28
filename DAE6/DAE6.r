#!/usr/bin/env Rscript
set.seed(123)
library(scales)
library(nortest)

# 1
lambda <- 2
mu <- 1/lambda
N <- 40

x_exp <- rexp(N, mu)

x1 <- seq(from=-2, to=7, by=0.1)
x1m <- mean(x1)
x1s <- sd(x1)
nor <- 1/(x1s*sqrt(2*pi))*exp(-(x1-x1m)^2/(2*x1s^2))*6

png('DAE6-1c.png')
hist(x_exp, col = scales::alpha('skyblue', .7))
lines(x1, nor, col = scales::alpha('red', .8))
legend("topright", c("x1", "nor"), fill=c(scales::alpha('skyblue', .8), scales::alpha('red', .8)))
dev.off()

xm_exp <- mean(x_exp)
xs_exp <- sd(x_exp)

# 2
N <- 40
mu <- c(2, -2)
sigma <- c(2, 1.95)

x1 <- rnorm(N, mu[1], sigma[1])
x2 <- rnorm(N, mu[2], sigma[2])
png('DAE6-2c.png')
hist(x1, xlim=c(-6, 7), xlab = 'x', main = paste("Histogram of", 'x1 + x2'), col = 'skyblue')
hist(x2, add=T, col = scales::alpha('red', .4))

sm <- c(mean(x1, 2), mean(x2, 2))
se <- c(sd(x1), sd(x2))

nor1 <- 1/(se[1]*sqrt(2*pi))*exp(-(x1-sm[1])^2/(2*se[1]^2))*6
nor2 <- 1/(se[2]*sqrt(2*pi))*exp(-(x2-sm[2])^2/(2*se[2]^2))*6

lines(x1, nor1*2, col = scales::alpha('yellow', .8))
lines(x2, nor2*2, col = scales::alpha('green', .8))
legend("topright", c("x1", "x2", "nor1", "nor2"), fill=c(scales::alpha('skyblue', .8), scales::alpha('red', .8), scales::alpha('yellow', .8), scales::alpha('green', .8)))
dev.off()

x <- c(1, 2, 5, 1, 9, 2)
y <- c(5, 9, 1, 3, 4, 5)
z <- c(17, 19, 21, 15, 21, 28)

png('DAE6-4a.png')
par(mfrow=c(1,3))

hist(x, col = scales::alpha('skyblue', .6))
hist(y, col = scales::alpha('red', .6))
hist(z, col = scales::alpha('orange', .6))
dev.off()
par(mfrow=c(1,1))
median(x)
median(y)
median(z)

qx <- quantile(x)
qy <- quantile(y)
qz <- quantile(z)

qyz <- sort(c(y, z))

qx;qy;qz
png('DAE6-4d.png')
plot(qx, qy)
dev.off()
png('DAE6-4e.png')
qqnorm(qyz)
qqline(qyz)
dev.off()


# 5
png('DAE6-5a.png')
qqplot(x, x1)
dev.off()
# 6
png('DAE6-6a.png')
qqnorm(x)
qqline(x)
dev.off()
png('DAE6-6b.png')
qqnorm(x1)
qqline(x1)
dev.off()
png('DAE6-6c.png')
qqnorm(x2)
qqline(x2)
dev.off()

# 7
lambda <- 2
mu <- 1/lambda
N <- 40

x_exp <- rexp(N, mu)
ad.test(x_exp)
ad.test(x1)
ad.test(x2)









