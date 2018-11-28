# setwd("~/Documents/Master1_DataScience/1er QUADRI/High_Dimensional_Data_Analysis/Breast-cancer-data-analysis")
setwd("~/Documents/INGE/MASTER/1ère\ MASTER/1er\ QUADRI/HDDA/Projects/Breast-cancer-data-analysis/")

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

# Definition of new data sets according to the qualitative variable
Healthy <- data[Classification==1, 1:9]
Cancer <- data[Classification==2, 1:9]

# PCA by default
res <- princomp(Healthy)
summary(res)

# PCA with an estimated covariance matrix
library(MASS)
rob_healthy <- cov.rob(Healthy, method="mcd", quantile.used = 30)
P <- princomp(covmat=rob_healthy$cov)
summary(P)

detach(data)