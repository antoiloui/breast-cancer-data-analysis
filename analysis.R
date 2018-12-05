setwd("~/Documents/Master1_DataScience/1er QUADRI/High_Dimensional_Data_Analysis/Breast-cancer-data-analysis")
#setwd("~/Documents/INGE/MASTER/1ère\ MASTER/1er\ QUADRI/HDDA/Projects/Breast-cancer-data-analysis/")

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
par(mfrow=c(3,3))
boxplot(data[,1] ~ Classification, main='Age (years)', names = c("Healthy","Cancerous"))
boxplot(data[,2] ~ Classification, main='BMI (kg/m^2)', names = c("Healthy","Cancerous"))
boxplot(data[,3] ~ Classification, main='Glucose (mg/dL)', names = c("Healthy","Cancerous"))
boxplot(data[,4] ~ Classification, main='Insuline (uU/mL)', names = c("Healthy","Cancerous"))
boxplot(data[,5] ~ Classification, main='HOMA', names = c("Healthy","Cancerous"))
boxplot(data[,6] ~ Classification, main='Leptin (ng/mL)', names = c("Healthy","Cancerous")) 
boxplot(data[,7] ~ Classification, main='Adiponectin (ug/mL)', names = c("Healthy","Cancerous"))
boxplot(data[,8] ~ Classification, main='Resistin (ng/mL)', names = c("Healthy","Cancerous"))
boxplot(data[,9] ~ Classification, main='MCP.1 (pg/dL)', names = c("Healthy","Cancerous"))
par(mfrow=c(1,1))

# Matrix of scatterplots of the quantitative features
my_cols <- c("#00AFBB", "#FC4E07")  
pairs(data[,1:9], pch = 16,  cex = 0.9,col = my_cols[data$Classification])

# Graphics of the correlation matrix
c <- cor(data[,1:9])
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
par(mfrow=c(1,1))
plot(maha,log(robust))
abline(v=qchisq(0.975,9), col="red")
abline(h=log(qchisq(0.975,9)), col="red")

#Robust correlation matrix
#corrplot(robS$cor)


#-----------------------------------------------------------------------------
# Question 3
#-----------------------------------------------------------------------------

# PCA with an estimated covariance matrix
PCA <- princomp(data[-10], cor = TRUE)
summary(PCA)

# Loadings
PCA$loadings

# Bar plots to represent loadings
par(mfrow=c(3,3))
for(i in 1:9)
  barplot(PCA$loadings[,i], main=paste("Component", i), las=2)
par(mfrow=c(1,1))

# Scree plots
plot(PCA, type='l', main='')

# Matrix containing the scores for the 9 variables
corrplot(cor(data[-10], PCA$scores))

# Correlation circle
library(ade4)
par(mfrow=c(2,2))
sub <- c("Component 1",  "Component 2", "Component 3",  "Component 4")
for(i in 1:4)
   s.corcircle(cor(data[-10], PCA$scores)[,i:(i+1)],sub = sub[i])

# Detach the data
detach(data)
