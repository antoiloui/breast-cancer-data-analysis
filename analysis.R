#[setwd("~/Documents/Master1_DataScience/1er QUADRI/High_Dimensional_Data_Analysis/Breast-cancer-data-analysis")
setwd("~/Documents/INGE/MASTER/1ère\ MASTER/1er\ QUADRI/HDDA/Projects/Breast-cancer-data-analysis/")

# Data loading
data <- read.table("data.csv", header=TRUE, sep=',')
attach(data)

# Data viewer
View(data)


#-----------------------------------------------------------------------------
# Question 2
#-----------------------------------------------------------------------------

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
abline(h=qchisq(0.95, 9), col='red')  # p = 9 = number of quantitative variables

#Robust estimator
library(MASS)
robS = cov.rob(data[,1:9], quantile.used = floor((116 + 9 + 1)/2), method='mcd', cor = TRUE)
robust <- mahalanobis(data[,1:9], robS$center, robS$cov)

#Plotting robust distances (log because of scale)
plot(log(robust), type = "h")
abline(h=log(qchisq(0.975,9)), col="red")

#DDplot (log because of scale)
plot(maha,log(robust))
abline(h=qchisq(0.975,9), col="red")
abline(v=qchisq(0.975,9), col="red")

#Robust correlation matrix
corrplot(robS$cor)


#-----------------------------------------------------------------------------
# Question 3
#-----------------------------------------------------------------------------

# PCA with an estimated covariance matrix
PCA <- princomp(covmat=robS$cov)
summary(PCA)

# Loadings
PCA$loadings

# Bar plots to represent loadings
par(mfrow=c(3,3))
for(i in 1:9)
  barplot(PCA$loadings[,i], main=paste("Component", i))
# Scree plots
plot(PCA, type='l', main='')




# Detach the data
detach(data)