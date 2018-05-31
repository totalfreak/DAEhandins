#!/usr/bin/env Rscript
set.seed(123)
library(scales)
library(nortest)
library(R.matlab)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(corrplot)
library(tidyr)
library(Hmisc)
library(ltm)

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
data <- read.csv('anscombe.csv')

cor.test(data$x1, data$y1, method='pearson')
cor.test(data$x2, data$y2, method='pearson')
cor.test(data$x3, data$y3, method='pearson')
cor.test(data$x4, data$y4, method='pearson')

matrix <- cor(data, use='complete.obs', method='pearson')

png('DAE8-1corrplot.png')
corrplot(matrix)
dev.off()

# 2
data <- read.csv('cash_happy.csv')
cor.test(data$cash, data$happy, method='pearson')
matrix <- cor(data, use='complete.obs', method='pearson')

png('DAE8-2corrplot.png')
corrplot(matrix)
dev.off()
#cor.test(data$x1, data$y1, method='pearson')

# 3
data <- readMat('gametimes.mat')
data <- data.frame(ftimes = data$ftimes, mtimes = data$mtimes)

# H0 = Average weekly gaming hours are the same for male and female Medialogy students
# H1 = Average weekly gaming hours are not the same for male and female Medialogy students
data$ftimes
data$mtimes
data$total <- gather(data$ftimes, data$mtimes)
data$total
t.test(data$ftimes, data$mtimes)

cohens_d(data$ftimes, data$mtimes)

biserial.cor(data$ftimes, data$mtimes)
