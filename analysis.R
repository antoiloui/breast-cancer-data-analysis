setwd("~/Documents/Master1_DataScience/1er QUADRI/High_Dimensional_Data_Analysis/Breast-cancer-data-analysis")
#setwd("~/Documents/INGE/MASTER/1ère\ MASTER/1er\ QUADRI/HDDA/Projects/Breast-cancer-data-analysis/")

# Data loading
data <- read.table("data.csv", header=TRUE, sep=',')
attach(data)

# Definition of new data sets
Healthy <- data[Classification==1, 1:9]
Cancer <- data[Classification==2, 1:9]

# Data viewer
View(data)

# Statistical summaries of the variables
summary(data)

# Graphics of the impact of the qualitative feature on quantitative features
boxplot(data[,1] ~ Classification, main='Age')
boxplot(data[,2] ~ Classification, main='BMI')
boxplot(data[,3] ~ Classification, main='Glucose levels')
boxplot(data[,4] ~ Classification, main='Insuline levels')
boxplot(data[,5] ~ Classification, main='HOMA')
boxplot(data[,6] ~ Classification, main='Leptin levels') 
boxplot(data[,7] ~ Classification, main='Adiponectin levels')
boxplot(data[,8] ~ Classification, main='Resistin levels')
boxplot(data[,9] ~ Classification, main='MCP.1 levels')


# Matrix of scatterplots of the quantitative features
pairs(data[,1:9])

# Graphics of the correlation matrix
c <- cor(data)
library(corrplot)
corrplot(c)


# Mahalanobis distances and multivariate 
m <- apply(data[,1:9], 2, mean)
S <- cov(data[,1:9])
maha <- mahalanobis(data[,1:9], m, S)

#Plotting standard distances (not robust)
plot(maha, type='h')
abline(h= qchisq(0.95, 9), col='red')  # p = 9 = number of quantitative variables
#Robust estimator
library(MASS)
robS = cov.rob(data[,1:9], quantile.used = floor((116 + 9 + 1)/2), method='mcd',cor = TRUE)
robust_dist<- mahalanobis(data[,1:9], robS$center, robS$cov)

#Plotting robust distances
plot(robust_dist, type = "h")
abline(h=qchisq(0.975,9), col="red")

#DDplot
plot(maha,robust_dist)

#Robust correlation matrix
corrplot(robust_est$cor)



detach(data)