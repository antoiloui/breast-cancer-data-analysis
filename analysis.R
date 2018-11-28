setwd("~/Documents/Master1_DataScience/1er QUADRI/High_Dimensional_Data_Analysis/Breast-cancer-data-analysis")
#setwd("~/Documents/INGE/MASTER/1ère\ MASTER/1er\ QUADRI/HDDA/Projects/Breast-cancer-data-analysis/")

# Data loading
data <- read.table("data.csv", header=TRUE, sep=',')
attach(data)

# Data viewer
View(data)

# Statistical summaries of the variables
summary(data)

# Graphics of the impact of the qualitative feature on quantitative features
boxplot(data[,1] ~ Classification, main='Age (years)')
boxplot(data[,2] ~ Classification, main='BMI (kg/m^2)')
boxplot(data[,3] ~ Classification, main='Glucose (mg/dL)')
boxplot(data[,4] ~ Classification, main='Insuline (uU/mL)')
boxplot(data[,5] ~ Classification, main='HOMA')
boxplot(data[,6] ~ Classification, main='Leptin (ng/mL)') 
boxplot(data[,7] ~ Classification, main='Adiponectin (ug/mL)')
boxplot(data[,8] ~ Classification, main='Resistin (ng/mL)')
boxplot(data[,9] ~ Classification, main='MCP.1 (pg/dL)')


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


# Definition of new data sets according to the qualitative variable
Healthy <- data[Classification==1, 1:9]
Cancer <- data[Classification==2, 1:9]

# PCA by default
res <- princomp(Healthy)
summary(res)

# PCA with a robust covariance matrix
library(MASS)
rob_healthy <- cov.rob(Healthy, method="mcd", quantile.used = floor((52 + 9 + 1)/2), cor= TRUE)
P <- princomp(covmat=rob_healthy$cov)
summary(P)



detach(data)