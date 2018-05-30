#!/usr/bin/env Rscript
set.seed(123)
library(scales)
library(nortest)
library(R.matlab)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(broom)

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
data <- readMat('gayshit.mat')
data$A
mean(data$A[,1])
sd(data$A)
sem(data$A)

# H0 = On average the children from Sank Annæ Skole valued their collaboration, using the prototype, as being 3 or below.
# H1 = On average the children from Sank Annæ Skole valued their collaboration, using the prototype, as being above 3.

t.test(data$A, conf.level=.9995, alternative='greater', mu=3)
png('DAE-hist.png')
hist(data$A, xlab='data$A',  main = paste("Histogram of", 'data$A'), col = 'skyblue')
dev.off()
plot(density(data$A))
polygon(density(data$A), col='red', border='red')
melted_data <- melt(data$A)

png('DAE-qqplot.png')
ggqqplot(melted_data[,3])
dev.off()

shapiro.test(melted_data[,3])

sme <- apply(data$A, 2, mean)
ssem <- apply(data$A, 2, sem)
sme_ssem <- data.frame(mean = sme, sem = ssem, group= c('1','2','3','4','5','6','7','8'))

png('DAE-barplot.png')
ggplot(sme_ssem, aes(x = group, y = mean)) + geom_bar(stat = 'identity') + geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .2, col='red') + xlab('Question no.') + ylab('Value') + ggtitle('Means of the questions')
dev.off()














