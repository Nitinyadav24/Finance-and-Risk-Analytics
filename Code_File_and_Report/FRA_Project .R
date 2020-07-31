---
title: "FRA_NitinYadav"
author: "Nitin Yadav"
date: "14/02/2020"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Business Problem

We have been given the data for various companies in terms of financial information which includes Assets, Capital, Income, Sales and key financial ratios. Based on the Networth "Next Year" we have to build a predictive model to identify if the company will default next year or not. We will use the Logistic regression model to build the model and check the model performance on the Test set for the accuracy.





## Observation from data 

1. Total of 3541 entries with 53 variables 
2. Dependant variable is Default wwhich is derived from "Net worth Nextyear"
3. All independent variables are numeric except the dependent which is categorical
4. Missing values are present in most of the variables
5. Outliers are present in the key predictor variabes like "Income", "Assets", "Net worth" and other variables 
6. Default rate is around 6.9% for the overall datapoints 





## Outlier and Missing value Treatment

Check histogram and box plot for income and Sales as they have highest maximum amount and consider only values that are less than 50000. Still after doing this we notice that there are lot of outliers present in multiple variables in dataset, but still we will go ahead with the data as we will lose important data aspect.

Also we notice that there are lot of NA values present in the dataset,almost in many of the columns , so we will treat the NA values. 

We have option to do the imputation using mean, median or using the MICE package. We will not use Mice package as couple of variable names in the dataset have space in between them, hence the other preferred method is KNN imputation which will use the nearby variables to calculate the imputation value.

We will also take the variables of interest in our dataset and the final dataset after treating for outliers and missing value. The final dataset consist of 3302 entries and 22 columns. The test data has 670 entries and 22 columns


## New Variable creations

We will create the below new variables to be included in the dataset. Liquidity ratio are already calculated as part of dataset, so we will not crete variables for liquidity.

Profitability 

1 . Return on Asset 
2. Return on Equity 

Leverage 

1. Debt to Equity ratio - This ratio already given as part of dataset
2. Debt to Capital ratio

Liquidity Ratio

1. Current Ratio - given as part of dataset
2. Quick Ratio - given as part of dataset 
3. Cash ratio - given as part of dataset 

Company Size Ratio

1. Income to Sales Ratio 
2. Profit to Sales Ratio
3. Profit to Income Ratio



## Multicolinearity Analysis

We notice that the financial ratios have been derived from the individual variables , hence if we check the correlation plot , we will surely notice the collinearity among variables,so we will build the logistic regression model and after that we will check multicolinearity using VIF and will also create a correlation plot.

The same has been covered as part of LR model building


```{r}
library(readxl)
library(dplyr)
library(corrplot)
library(mice)
library(DMwR)
library(car)
library(caret)
library(SDMTools)
library(lattice)

Tr_D=read_excel("raw-data.xlsx")





dim(Tr_D)
summary(Tr_D)
str(Tr_D)

Tr_D$Default=ifelse(Tr_D$`Networth Next Year`>0,0,1)
Tr_D$Default=as.factor(Tr_D$Default)

# Check for NA values 

sum(is.na(Tr_D))
colSums(is.na(Tr_D))

# Take the variables of interest in data drame and exclude other variables not required

dim(Tr_D)
Tr_D1=Tr_D[,c(3:5,8,11,17,20,23,24,36:40,51,53)]

dim(Tr_D1)

summary(Tr_D1)



boxplot(Tr_D1)
boxplot(Tr_D1$Sales)
boxplot(Tr_D1$Sales)

Tr_D2=Tr_D1[which(Tr_D1$`Total assets`<50000),]
summary(Tr_D2)
boxplot(Tr_D2$`Net working capital`)
dim(Tr_D2)
boxplot(Tr_D2)


Tr_D3=Tr_D2[which(Tr_D2$`Total income`<50000),]
summary(Tr_D3)
boxplot(Tr_D3)
dim(Tr_D3)


tail(Tr_D3[order(Tr_D3$`Total income`),])


## Missing value treatment 

md.pattern(Tr_D3)
colSums(is.na(Tr_D3))

# As we see there are lot of missing values we will use KNN imputation. It will impute the values based on its nearest neighbors. We can also use the Mice function but i am not using that here considering that given column names have spaces inbetween then and the code will throw a parsing error, so we have to change column names before using mice package. We will go with KNN as it is equally reliable for the current dataset


Tr_D4=Tr_D3[,-16]
Tr_D4=data.frame(Tr_D4)

Tr_D5=knnImputation(Tr_D4, k = 5)

colSums(is.na(Tr_D5))

summary(Tr_D5)

Train_Data=Tr_D5


colnames(Train_Data)


## Creation of new variables 

## Profitability 

# 1 . Return on Asset 

Train_Data$ReturnonAsset=Train_Data$Total.income/Train_Data$Total.assets



# 2. Return on Equity 

Train_Data$ReturnonEquity=Train_Data$Profit.after.tax/Train_Data$Net.worth



## Leverage 

# 1. Debt to Equity ratio - This ratio already given as part of dataset

# 2. Debt to Capital ratio

Train_Data$DebttoCapital=Train_Data$Borrowings/Train_Data$Total.capital


## Liquidity Ratio

# 1. Current Ratio - given as part of dataset
# 2. Quick Ratio - given as part of dataset 
# 3. Cash ratio - given as part of dataset 


## Company Size Ratio

# 1. Income to Sales Ratio 

Train_Data$Incometosales=Train_Data$Total.income/Train_Data$Sales

# 2. Profit to Sales Ratio

Train_Data$ProfittoSales=Train_Data$Profit.after.tax/Train_Data$Sales

# 3. Profit to Income Ratio

Train_Data$ProfittoIncome=Train_Data$Profit.after.tax/Train_Data$Total.income

Train_Data$ProfittoIncome[is.na(Train_Data$ProfittoIncome)]=median(Train_Data$ProfittoIncome,na.rm = T)



Train_Data=cbind(Train_Data,Tr_D3[,16])

dim(Train_Data)



```


## Univariate and Bivariate analysis

For Train and Test Data

1. Defaulting companies loses profit by 6 unit per 100 units and non defaulting companies earn profit by 3 units per 100 units of income.
2. Non Defaulting companies have more assets compared to defaulting companies 
3. PAT , Cash profit and Return on Equity is more for Non defaulting companies compared to Defaulting ones
4. Debt to Equity ratio for defaulting companies is higher means they have more debt to finance their operations and hence have higher chance of no repayment and default.
5. Total liabilities are more and Total sales are less for Defaulting companies compared to Non Defaulting companies.









```{r}

## EDA for Train Data 

# Observations

# 1. Defaulting companies loses profit by 6 unit per 100 units and non defaulting companies earn profit by 3 units per 100 units of income.


summary(Tr_D$`PAT as % of total income`[Tr_D$Default==0])
summary(Tr_D$`PAT as % of total income`[Tr_D$Default==1])


boxplot(Tr_D3$`Total assets`~Tr_D3$Default, xlab="Defaut",
        ylab="Assets",main="Assets by Default",col=c("Red","Green"))


# Numerical 

qplot(Train_Data$Total.assets, data = Train_Data, main=" Total Assets ", xlab = "Assets ", ylab = "frequency")


qplot(Train_Data$Total.income, data = Train_Data, main=" Income Plot ", xlab = "Income ", ylab = "frequency")


qplot(Train_Data$Total.liabilities, data = Train_Data, main=" Liabilities Plot ", xlab = "Income ", ylab = "frequency")


# Categorical 

qplot(Train_Data$Default, data = Train_Data, main=" Default Bar Plot", xlab = "Default", ylab = "frequency")



# Categorical and numerical together

qplot(Train_Data$Default,Train_Data$Profit.after.tax, data = Train_Data, , geom="boxplot",main=" PAT by Default", xlab = "Default", ylab ="PAT")


qplot(Train_Data$Default,Train_Data$Cash.profit, data = Train_Data, , geom="boxplot",main=" Cash Profit by Default", xlab = "Default", ylab ="Cash Profit")

qplot(Train_Data$Default,Train_Data$Debt.to.equity.ratio..times., data = Train_Data, , geom="boxplot",main=" Debt to Equity by Default", xlab = "Default", ylab ="Debt to Equity")

qplot(Train_Data$Default,Train_Data$ReturnonAsset, data = Train_Data, , geom="boxplot",main=" Return on Equity by Default", xlab = "Default", ylab ="Return on Equity")



# Numerical against numerical and color the dots by categorical variable

plot(Train_Data[,c(1:4,6,7,15)])


qplot(Train_Data$Total.assets, Train_Data$Net.worth, data = Train_Data, color=Train_Data$Default , main = "Asset vs Networth", xlab = "Total Asset", ylab = "Net worth")

qplot(Train_Data$Total.assets, Train_Data$Total.income, data = Train_Data, color=Train_Data$Default , main = "Asset vs Income", xlab = "Total Asset", ylab = "Total Income")


qplot(Train_Data$Sales, Train_Data$Profit.after.tax, data = Train_Data, color=Train_Data$Default , main = "Asset vs Income", xlab = "Sales", ylab = "Profit After Tax")

qplot(Train_Data$Total.liabilities, Train_Data$Profit.after.tax, data = Train_Data, color=Train_Data$Default , main = "Liabilities vs PAT", xlab = "PAT", ylab = "Total Liabilities")


by(Train_Data, INDICES = Train_Data$Default, FUN = summary)


histogram(~Train_Data$Profit.after.tax|factor(Train_Data$Default), data = Train_Data)

```

## Logistic Regression model on important variables

We build the model on the below variables 

1. Profit to Income
2. Income to Sales
3. Current Ratio
4. Debt to Equity
5. Debt to Capital
6. Return on Equity
7. Return on Asset

We notice that Probability of default is a logit function with coefficient and intercept. The positive sign in the coefficient indicates that as the value increases , the probability of default increases and negative sign means they are inversly proportional ie as the value increases chance of default decrease..for eg as PAT increase , risk of default decrease

These coffiencients are meanigful and they are signficantly different from zero and hence are important predictors in identifying default

when we check the VIF for multicolinearity we find that all the variables have scores less than 3 so they are are independent. The same is visible from correlation plot.


```{r}
## Model Building for Train Data 

colnames(Train_Data)

# Building logistic regression model 

attach(Train_Data)


Tr_L1=glm(Train_Data$Default~ProfittoIncome, data = Train_Data, family = binomial)
summary(Tr_L1)

# using company size measures as predictor 

Tr_L2=glm(Train_Data$Default~ProfittoIncome+Incometosales, data = Train_Data, family = binomial)
summary(Tr_L2)

# Using liquidity measures as predictor 

Tr_L3=glm(Train_Data$Default~ProfittoIncome+Current.ratio..times.+Incometosales, data = Train_Data, family = binomial)
summary(Tr_L3)

Tr_L4=glm(Train_Data$Default~ProfittoIncome+Incometosales+Current.ratio..times.+Cash.to.current.liabilities..times., data = Train_Data, family = binomial)
summary(Tr_L4)


# Using leverage measures as predictor 

Tr_L5=glm(Train_Data$Default~ProfittoIncome+Incometosales+Current.ratio..times.+Debt.to.equity.ratio..times.+DebttoCapital, data = Train_Data, family = binomial)
summary(Tr_L5)

# checking VIF for multicolinearity and removing the variable 

vif(Tr_L5)

# Using profitbility measures as predictor 
Tr_L6=glm(Train_Data$Default~ProfittoIncome+Incometosales+Current.ratio..times.+Debt.to.equity.ratio..times.+DebttoCapital+ReturnonEquity, data = Train_Data, family = binomial)
summary(Tr_L6)


Tr_L7=glm(Train_Data$Default~ProfittoIncome+Incometosales+Current.ratio..times.+Debt.to.equity.ratio..times.+DebttoCapital+ReturnonEquity+ReturnonAsset, data = Train_Data, family = binomial)
summary(Tr_L7)


# These coffiencients are meanigful and they are signficantly different from zero and hence are important predictors in identifying default
# Positive values of coefficient tells that higher the value , higher is chance of default i.e more debt means more chances of default. For negative values , higher the value lesser is chance of default i.e more profit means lesser chance of default
# when we check the VIF for multicolinearity we find that all the variables have scores less than 3 so they are are independent.

vif(Tr_L7)

# Check for Multicolinearity thru corrplot and VIF 
colnames(Train_Data)

corrplot(cor(Train_Data[,c(12,13,16,17,18,19,21)]))


```



```{r}
## Prediction for Train Set

plot(Train_Data$Default,Tr_L7$fitted.values)

summary(Tr_L7$fitted.values)

plot(as.factor(Tr_L7$y),Tr_L7$fitted.values)

## Doing the prediction using the Train Data

Pred.Train=predict(Tr_L7,newdata = Train_Data,type = "response")

plot(Train_Data$Default,Pred.Train)



```

## Model performance for Train Set

We take threshold of 8% to find that below this threshold there is minimum prob. of default and above this threshold the risk of default increases.

OVerall Accuracy - 90% ie in identifying the 1s and 0s correctly which means 10% is missclassification

Sensitivity - 63% ie ability of model to predicting 1s correctly. We can increase this value by reducing the threshold but that will also increase the Loss rate and missclasification.

Specificity - 92% , the ability to identify 0s correctly

Loss - 2.5% current loss stands at 2.5% i.e it is the rate  for which the model actually said , company will not default but actually company defaulted.


ROC - 82% 

KS Calculation -(Difference between %Cumulative Right and % Cumulative Wrong)
KS values intrepret that the model is moderatley good in terms of separating the "1"s and "0"s and we can interpret that the model holds a good prediction ability.

KS Score - 58%

AUC calculation -Larger the AUC and larger Gini better the model is ..Gini = 2 AUC-1),AUC is the % of Box that is under the ROC curve

AUC - 82%

Gini - 42%

Concordance - 82%




```{r}
## Model Performance for Train Set

Train.logit=confusion.matrix(Train_Data$Default,Pred.Train,threshold = 0.08)
Train.logit


## Accuracy of model 

accuracy.LR<-sum(diag(Train.logit))/sum(Train.logit)
accuracy.LR

## Sensitivity

sensitivity.LR<-Train.logit[2,2]/(Train.logit[1,2]+Train.logit[2,2])
sensitivity.LR


## Specificity

specificity.LR<-Train.logit[1,1]/(Train.logit[1,1]+Train.logit[2,1])
specificity.LR

## Loss Function - When we predicted company will not default but actually company defaulted 
loss.LR<-Train.logit[1,2]/(Train.logit[1,2]+Train.logit[1,1])
loss.LR
```

```{r}
## OTher Model performance for Train Data set

## Roc curve for the model
library(ROCR)

ROCRTrain = prediction(Pred.Train,Train_Data$Default)
as.numeric(performance(ROCRTrain, "auc")@y.values)
perf = performance(ROCRTrain, "tpr","fpr")
plot(perf,col="black",lty=2, lwd=2)
plot(perf,lwd=3,colorize = TRUE)


## KS Calculation -(Difference between %Cumulative Right and % Cumulative Wrong)
## KS values intrepret that the model is moderatley good in terms of separating the "1"s and "0"s and we can interpret that the model holds a good prediction ability.

KS_Train=max(perf@y.values[[1]]-perf@x.values[[1]])
print(KS_Train)


## AUC calculation -Larger the AUC and larger Gini better the model is ..Gini = 2 AUC-1),AUC is the % of Box that is under the ROC curve

AUC_Train=performance(ROCRTrain,"auc")
AUC_Train=as.numeric(AUC_Train@y.values)
print(AUC_Train)

## Gini Calculation

library(ineq)

Gini_Train=ineq(Pred.Train,"gini")
print(Gini_Train)


## Concordance

library(InformationValue)

Concord_Train=Concordance(actuals = Train_Data$Default,predictedScores = Pred.Train)
print(Concord_Train)
```



```{r}

## Test Data 

Te_D=read_excel("validation_data.xlsx")
dim(Te_D)
summary(Te_D)
str(Te_D)


Te_D$`Default - 1`=as.factor(Te_D$`Default - 1`)


# Check for NA values 

sum(is.na(Te_D))
colSums(is.na(Te_D))

# Take the variables of interest in data drame and exclude other variables not required

dim(Te_D)
Te_D1=Te_D[,c(2:5,8,11,17,20,23,24,36:40,51)]

dim(Te_D1)

summary(Te_D)

## Outlier Treatment 

# Check histogram and box plot for income and Sales as they have highest maximum amount

boxplot(Te_D1)
boxplot(Te_D1$Sales)
boxplot(Te_D1$Sales)

Te_D2=Te_D1[which(Te_D1$`Total assets`<50000),]
summary(Te_D2)
boxplot(Te_D2$`Net working capital`)
dim(Te_D2)
boxplot(Te_D2)


Te_D3=Te_D2[which(Te_D2$`Total income`<50000),]
summary(Te_D3)
boxplot(Te_D3)
dim(Te_D3)

head(Te_D3[order(Te_D3$`Total income`,decreasing=TRUE),])


## Missing value Treatment - we can use basic imputation with mean or median or can use Mice or KNN imputation. we will go with knn as it will use the features of nearest neighbors to predict the missing value.

Te_D4=Te_D3[,-1]
Te_D4=data.frame(Te_D4)

Te_D5=knnImputation(Te_D4, k = 5)

colSums(is.na(Te_D5))

summary(Te_D5)

Test_Data=Te_D5



## Creation of new variables 

## Profitability 

# 1 . Return on Asset 

Test_Data$ReturnonAsset=Test_Data$Total.income/Test_Data$Total.assets


# 2. Return on Equity 

Test_Data$ReturnonEquity=Test_Data$Profit.after.tax/Test_Data$Net.worth



## Leverage 

# 1. Debt to Equity ratio - This ratio already given as part of dataset

# 2. Debt to Capital ratio

Test_Data$DebttoCapital=Test_Data$Borrowings/Test_Data$Total.capital


## Liquidity Ratio

# 1. Current Ratio - given as part of dataset
# 2. Quick Ratio - given as part of dataset 
# 3. Cash ratio - given as part of dataset 


## Company Size Ratio

# 1. Income to Sales Ratio 

Test_Data$Incometosales=Test_Data$Total.income/Test_Data$Sales


# 2. Profit to Sales Ratio

Test_Data$ProfittoSales=Test_Data$Profit.after.tax/Test_Data$Sales

# 3. Profit to Income Ratio

Test_Data$ProfittoIncome=Test_Data$Profit.after.tax/Test_Data$Total.income
Test_Data$ProfittoIncome[is.na(Test_Data$ProfittoIncome)]=median(Test_Data$ProfittoIncome,na.rm = T)


Test_Data=cbind(Test_Data,Te_D3[,1])

dim(Test_Data)



```

```{r}
## EDA for Test Data 

# Observations

# 1. Defaulting companies loses profit by 9 unit per 100 units and non defaulting companies earn profit by 3 units per 100 units of income.


summary(Te_D$`PAT as % of total income`[Te_D$`Default - 1`==0])
summary(Te_D$`PAT as % of total income`[Te_D$`Default - 1`==1])

boxplot(Te_D3$`Total assets`~Te_D3$`Default - 1`, xlab="Defaut",
        ylab="Assets",main="Assets by Default",col=c("Red","Green"))


# Numerical 

qplot(Test_Data$Total.assets, data = Test_Data, main=" Total Assets ", xlab = "Assets ", ylab = "frequency")


qplot(Test_Data$Total.income, data = Test_Data, main=" Income Plot ", xlab = "Income ", ylab = "frequency")


qplot(Test_Data$Total.liabilities, data = Test_Data, main=" Liabilities Plot ", xlab = "Income ", ylab = "frequency")



# Categorical 

qplot(Test_Data$Default, data = Test_Data, main=" Default Bar Plot", xlab = "Default", ylab = "frequency")


# Categorical and numerical together

qplot(Test_Data$Default,Test_Data$Profit.after.tax, data = Test_Data, , geom="boxplot",main=" PAT by Default", xlab = "Default", ylab ="PAT")


qplot(Test_Data$Default,Test_Data$Cash.profit, data = Test_Data, , geom="boxplot",main=" Cash Profit by Default", xlab = "Default", ylab ="Cash Profit")

qplot(Test_Data$Default,Test_Data$Debt.to.equity.ratio..times., data = Test_Data, , geom="boxplot",main=" Debt to Equity by Default", xlab = "Default", ylab ="Debt to Equity")

qplot(Test_Data$Default,Test_Data$ReturnonAsset, data = Test_Data, , geom="boxplot",main=" Return on Equity by Default", xlab = "Default", ylab ="Return on Equity")


# Numerical against numerical and color the dots by categorical variable

plot(Test_Data[,c(1:4,6,7,15)])


qplot(Test_Data$Total.assets, Test_Data$Net.worth, data = Test_Data, color=Test_Data$Default , main = "Asset vs Networth", xlab = "Total Asset", ylab = "Net worth")

qplot(Test_Data$Total.assets, Test_Data$Total.income, data = Test_Data, color=Test_Data$Default , main = "Asset vs Income", xlab = "Total Asset", ylab = "Total Income")


qplot(Test_Data$Sales, Test_Data$Profit.after.tax, data = Test_Data, color=Test_Data$Default , main = "Asset vs Income", xlab = "Sales", ylab = "Profit After Tax")

qplot(Test_Data$Total.liabilities, Test_Data$Profit.after.tax, data = Test_Data, color=Test_Data$Default , main = "Liabilities vs PAT", xlab = "PAT", ylab = "Total Liabilities")



by(Test_Data, INDICES = Test_Data$Default, FUN = summary)


histogram(~Test_Data$Profit.after.tax|factor(Test_Data$Default), data = Train_Data)



```

```{r}
## Doing the prediction using the Test Data
colnames(Test_Data)
str(Test_Data$Incometosales)

colnames(Test_Data)[which(names(Test_Data) == "Default - 1")] = "Default"


dim(Test_Data)
str(Test_Data)

Pred.Test=predict(Tr_L7,newdata = Test_Data,type = "response")
plot(Test_Data$Default,Pred.Test)



```

## Model performance for Test Set

We take threshold of 8% to find that below this threshold there is minimum prob. of default and above this threshold the risk of default increases.

OVerall Accuracy - 91% ie in identifying the 1s and 0s correctly which means 10% is missclassification

Sensitivity - 71% ie ability of model to predicting 1s correctly. We can increase this value by reducing the threshold but that will also increase the Loss rate and missclasification.

Specificity - 92% , the ability to identify 0s correctly

Loss - 2.1% current loss stands at 2.5% i.e it is the rate  for which the model actually said , company will not default but actually company defaulted.


ROC - 86% 

KS Calculation -(Difference between %Cumulative Right and % Cumulative Wrong)
KS values intrepret that the model is moderatley good in terms of separating the "1"s and "0"s and we can interpret that the model holds a good prediction ability.

KS Score - 65%

AUC calculation -Larger the AUC and larger Gini better the model is ..Gini = 2 AUC-1),AUC is the % of Box that is under the ROC curve

AUC - 86%

Gini - 46%

Concordance - 86%


```{r}
## Model Performance for Train Set

Test.logit=confusion.matrix(Test_Data$Default,Pred.Test,threshold = 0.08)
Test.logit


## Accuracy of model 

accuracy.T<-sum(diag(Test.logit))/sum(Test.logit)
accuracy.T

## Sensitivity

sensitivity.T<-Test.logit[2,2]/(Test.logit[1,2]+Test.logit[2,2])
sensitivity.T


## Specificity

specificity.T<-Test.logit[1,1]/(Test.logit[1,1]+Test.logit[2,1])
specificity.T

## Loss Function - When we predicted company will not default but actually company defaulted 
loss.T<-Test.logit[1,2]/(Test.logit[1,2]+Test.logit[1,1])
loss.T
```
```{r}
## OTher Model performance for Test Data set

## Roc curve for the model

ROCRTest = prediction(Pred.Test,Test_Data$Default)
as.numeric(performance(ROCRTest, "auc")@y.values)
perf1 = performance(ROCRTest, "tpr","fpr")
plot(perf1,col="black",lty=2, lwd=2)
plot(perf1,lwd=3,colorize = TRUE)


## KS Calculation -(Difference between %Cumulative Right and % Cumulative Wrong)
## KS values intrepret that the model is moderatley good in terms of separating the "1"s and "0"s and we can interpret that the model holds a good prediction ability.

KS_Test=max(perf1@y.values[[1]]-perf1@x.values[[1]])
print(KS_Test)


## AUC calculation -Larger the AUC and larger Gini better the model is ..Gini = 2 AUC-1),AUC is the % of Box that is under the ROC curve

AUC_Test=performance(ROCRTest,"auc")
AUC_Test=as.numeric(AUC_Test@y.values)
print(AUC_Test)

## Gini Calculation

Gini_Test=ineq(Pred.Test,"gini")
print(Gini_Test)


## Concordance


Concord_Test=Concordance(actuals = Test_Data$Default,predictedScores = Pred.Test)
print(Concord_Test)
```

```{r}
# Rank ordering in 10 deciles for Train and Test 

probs=seq(0,1, length=11)
print(probs)


qstrain=quantile(Pred.Train,probs)
print(qstrain)


qstest=quantile(Pred.Test,probs)
print(qstest)


Train_Data$Decile=cut(Pred.Train,unique(qstrain),include.lowest = TRUE)
print(Train_Data$Decile)
head(Train_Data)

Test_Data$Decile=cut(Pred.Test,unique(qstest),include.lowest = TRUE)
print(Test_Data$Decile)
```

## Sorting the data in Descending order for 10 Deciles both for train and Test 

When we arrange the train and test data based on 10 deciles  we found that 20% of top data for Train set gives a cumulative response rate of 73% , however for test set 20% of top data gives a response rate of 80%.

The data on validation set has performed better in terms of the overall response rate.



## Conclusion

We build the model for both train and test set and check the accuracy and model performance measures , we found that test/validation data model performance measures are better compared to train set. Also when we arranged the data in descending order based on the cumulative response rate and Prob. of default, we found that validation test performed better.

We took threshold in our case as 8%, to increasr the sesnsitivity (ability to increase TP), we can lower the threshold, however that will also increase the FP and FN and hence chances of getting loss and opportunity loss will be more, so we have to carefully balance out the threshold to get the right sensitivity and minimum loss.


```{r}
library(data.table)

## Arranging in top deciles and printing the KS for Train set 


TrainDT=data.table(Train_Data)
RankTrain=TrainDT[, list(cnt=length(Default),cnt_tar1=sum(Default==1),cnt_tar0=sum(Default==0)), by=Decile][order(-Decile)]


RankTrain$Rrate=round(RankTrain$cnt_tar1/RankTrain$cnt,4)*100
RankTrain$cum_resp=cumsum(RankTrain$cnt_tar1)
RankTrain$cum_nonresp=cumsum(RankTrain$cnt_tar0)
RankTrain$cum_relresp=round(RankTrain$cum_resp/sum(RankTrain$cnt_tar1),4)*100
RankTrain$cum_relnonresp=round(RankTrain$cum_nonresp/sum(RankTrain$cnt_tar0),4)*100
RankTrain$KS=abs(RankTrain$cum_relresp - RankTrain$cum_relnonresp)


print(RankTrain)


## Arranging in top deciles and printing the KS for Test set 


TestDT=data.table(Test_Data)
RankTest=TestDT[, list(cnt=length(Default),cnt_tar1=sum(Default==1),cnt_tar0=sum(Default==0)), by=Decile][order(Decile,decreasing=TRUE)]

RankTest$Rrate=round(RankTest$cnt_tar1/RankTest$cnt,4)*100
RankTest$cum_resp=cumsum(RankTest$cnt_tar1)
RankTest$cum_nonresp=cumsum(RankTest$cnt_tar0)

RankTest$cum_relresp=round(RankTest$cum_resp/sum(RankTest$cnt_tar1),4)*100
RankTest$cum_relnonresp=round(RankTest$cum_nonresp/sum(RankTest$cnt_tar0),4)*100

RankTest$KS=abs(RankTest$cum_relresp - RankTest$cum_relnonresp)
print(RankTest)


```

