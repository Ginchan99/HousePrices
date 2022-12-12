rm(list=ls())
 
hp_test = read.csv("houseprices_test.csv") #load csv file
hp_train = read.csv("houseprices_train.csv") #load csv file
summary(hp_train)
summary(hp_test)
View(hp_train)

library(caTools) #load library caTools
library(ROCR)
library('ggplot2')
library('ggthemes') 
#install.packages('ggthemes')
library('scales')
library('dplyr') 
#install.packages('mice')
library('mice')
library('randomForest') 
library('data.table')
#install.packages('gridExtra')
library('gridExtra')
#install.packages('corrplot')
library('corrplot') 
#install.packages('GGally')
library('GGally')
#install.packages('e1071')
library('e1071')
# Training model

colnames(hp_train)#finding column names of train dataset 


cat_var <- names(hp_train)[which(sapply(hp_train, is.character))]
cat_car <- c(cat_var, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
numeric_var <- names(hp_train)[which(sapply(hp_train, is.numeric))]
#Creating one training dataset with categorical variable and one with numeric variable
hp_train1_cat<-hp_train[cat_var]
hp_train1_num<-hp_train[numeric_var]

summary(hp_train1_cat)
summary(hp_train1_num)

sd(hp_train$SalePrice)
var(hp_train$SalePrice)


plotHist <- function(data_in, i) 
{
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

## Density plot function

plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}
## Barplots for the categorical features

## Function to call both Bar plot and Density plot function

doPlots <- function(data_in, fun, ii, ncol=3) 
{
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

doPlots(hp_train1_cat, fun = plotHist, ii = 1:4, ncol = 2)
doPlots(hp_train1_cat, fun = plotHist, ii  = 5:8, ncol = 2)
doPlots(hp_train1_cat, fun = plotHist, ii = 9:12, ncol = 2)

doPlots(hp_train1_cat, fun = plotHist, ii = 13:18, ncol = 2)
doPlots(hp_train1_cat, fun = plotHist, ii = 19:22, ncol = 2)

doPlots(hp_train1_num, fun = plotHist, ii = 18:23, ncol = 2)

