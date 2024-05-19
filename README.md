# Housing-Price-Predictor-in-R
The Housing Price Predictor in R is a machine learning model designed to estimate the market value of residential properties based on various features such as location, size, and amenities. Utilizing techniques such as linear regression or decision trees, this predictor can help stakeholders make informed decisions in real estate investments and pricing strategies.
# Load the Libraries
library(tidyverse)
library(readr)
library(lmtest)
library(ggpubr)
library(broom)
library(ggfortify)
library(skimr)
library(ggplot2)
# Import R Dataset
df<-read_csv(url(urlfile1))
train_df<-read_csv(url(urlfile1))
test_df<-read_csv(url(urlfile2))
# View and Check Dimensions
dim(train_df)
# Check Column Names
names(train_df)
# Check Internal Structure of the Data Frame
glimpse(train_df)
# Getting Useful Summary Statistics
skim(train_df)
# To Investigate the Datatypes of Coumns
str(train_df)
# Drop the Columns with NAs
train_df <- subset(train_df, select = -c(Alley, PoolQC, Fence, MiscFeature, FireplaceQu))
str(train_df)
sum(is.na(train_df))
# Pick an Independent Variable with Right-Skewness
x <- train_df$OverallQual
hist(x, main = "Overall Quality")

x <- train_df$LotArea
hist(train_df$LotArea, main = "Lot Area")

# LotArea is clearly right-skewed.
X = train_df$LotArea
# The target variable we are trying to predict is SalePrice, the
# property's sale price in dollars.
Y = train_df$SalePrice
# Show histogram of SalePrice (target).
# SalePrice
hist(train_df$SalePrice, main = "Sale Price")

# Probability
# Summarize X (independent var.) and Y (target)
df = train_df %>% dplyr::select(LotArea, SalePrice)
summary(df)

#Assign quartile values to variables.
XQ1<-quantile(train_df$LotArea, 0.25)
YQ1<-quantile(train_df$SalePrice, 0.25)

#Create subsets of data based on quartile operators.
yY <- subset(train_df,SalePrice <= YQ1)
Yy <- subset(train_df,SalePrice > YQ1)
Xx_Yy<- subset(Yy, LotArea > XQ1)
Xx_yY<- subset(yY, LotArea > XQ1)
xX_Yy<- subset(Yy, LotArea <= XQ1)
xX_yY<- subset(yY, LotArea <= XQ1)

# calculate the required probabilities
a. P(X>x | Y>y)
#for P(X>x|Y>y)
a <- nrow(Xx_Yy)
nrow(Xx_Yy)/nrow(train_df)

b. P(X>x, Y>y)
#for P(X>x|y<Y)
b <- nrow(Xx_yY)
nrow(Xx_yY)/nrow(train_df)

c. P(X<x | Y>y)
c <- nrow(xX_Yy)
nrow(xX_Yy)/nrow(train_df)

#for P(X<x|Y>y)
c <- nrow(xX_Yy)
nrow(xX_Yy)/nrow(train_df)

#P(X<x|y<Y)
d <-nrow(xX_yY)
nrow(xX_yY)/nrow(train_df)

# Creating a table of counts
table <- matrix(c(d,c,(d+c),b,a,(b+a),(b+d),(c+a),(a+b+c+d)),ncol=3, nrow=3,byrow=TRUE)
colnames(table) <- c("<=1Q", ">1Q", "Total")
rownames(table) <- c('<=1Q', '>1Q', 'Total')
result_table <- as.table(table)
result_table

