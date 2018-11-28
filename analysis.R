# setwd("~/Documents/Master1_DataScience/1er QUADRI/High_Dimensional_Data_Analysis/Breast-cancer-data-analysis")
setwd("~/Documents/INGE/MASTER/1ère\ MASTER/1er\ QUADRI/HDDA/Projects/Breast-cancer-data-analysis/")


data <- read.table("data.csv", header=TRUE, sep=',')
attach(data)

View(data)
summary(data)

# Bivariate scatterplot
pairs(data)

#Correlation
c <- cor(data)
library(corrplot)
corrplot(cor(data))
