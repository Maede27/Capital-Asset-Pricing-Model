---
title: "Modelling Suncor Energy Share Price by a Multiple Linear Regression"
date: "December 22, 2017"
Western_ID: "250998150"
output:
  pdf_document: default 

header-includes:
- \usepackage{color}
- \usepackage{titling}
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(GGally)
library(knitr)
library(stargazer)
vifx <- function (X) 
{#Author: A.I. McLeod
    Xs <- scale(X)
    xx <- t(Xs) %*% Xs/(nrow(X) - 1)
    diag(solve(xx))
}
```

#Section 1: Purpose

This report inspired by Capital Asset Pricing Model (CAPM), attempting to model stock returns based on regression. In CAPM, we are determining how our stock is related to the overall market, using a regression structure. In this report, we extend this model by adding commodity trading prices for oil, gold, gas and currency exchange rate as explanatory variables.

This report consists of four sections as mentioned below,

1.Constructing dataset using Quantmod package, providing summary and simple graphic analysis on the dataset.

2.Obtaing a model for estimating stock performance on US market, Canada to US currency exchange rate, prices for oil, gas and gold. Together with some diagnostic checks including lattice plots, variance inflation factor bar chart, visualization of correlation matrix, Durbin-Watson.

3.Modyfing the model by introducing the logarithmic returns for both response and the predictors. Also checking the assumption of linear regression for proposed model.

4.Checking for Model validation; deciding whether the numerical results obtained from regression analysis are acceptable as description of the data. This section includes test and train validation according to RMSE, and time Series diagnostic checks for residuals.

5.Conclusion and recommendation.



#Introduction to Suncor Dataset

For this project we load raw data from yahoo finance and Fred source via Quantmod package in R. Working with raw data could be very difficult. The most challenging part is to convert a dataset with missing data into a clean dataset. In Suncor Dataset, there are some days that the price is not available. Also, choosing acceptable risk-free security or other explanatory variables need researching, and spending time to find the best choices.
The dataset consists of five years (2012 to 2017) of records of the daily close price of Suncor energy INC., S&P500 from yahoo finance website, and daily future prices for crude oil, Natural gas, gold, Canada to US exchange rate From the Federal Reserve Bank of St. Louis Economic Data (FRED) website. In second model we consider 3-month Treasury constant maturity rate as the risk-free rate. which can be collected from FRED website.


##Variable description

r.SU: Logarithmic daily return of stock price for Suncor Energy Inc. which is a Canadian integrated energy company based in Calgary, Alberta. It specializes in production of synthetic crude from oil sands.

r.SP500:Logarithmic daily return of index price for Standard & Poor's 500, often abbreviated as the S&P 500, which is an American stock market index based on the market capitalizations of 500 large companies having common stock listed on the NYSE.

r.GOLD: Logarithmic daily return of gold fixing price 10:30 A.M. (London time) in London Bullion Market.

r.OIL: Logarithmic daily return of crude Oil Price: West Texas Intermediate (WTI) - Cushing, Oklahoma, based in US dollars per barrel.

r.GAS: Logarithmic daily return of Henry Hub Natural Gas Spot Price, based in US dollars per million BTU.

r.CAUS: Logarithmic daily return of Canadian Dollars to One U.S. Dollar exchange rate. 


##Summary of data output and graphs

Some libraries are needed for the rest of the project:

```{r include=FALSE}
library(zoo)
library(graphics)
library(ggfortify)
library(knitr)
library(car)
library(RColorBrewer)
```

library(zoo)

library(graphics)

library(ggfortify)

library(knitr)

library(car)

library(RColorBrewer)

library(quantmod)

library(Metrics)

library(psych)



```{r include=FALSE, comment=""}
library(quantmod)
date.start <- "2012-01-01"
date.end <- "2017-01-01"
syb <- getSymbols(c("SU","^GSPC"),adjust = T,from = '2012-01-01', to = '2017-01-01')
SP500<- GSPC
```

```{r echo=FALSE,comment="",fig.width=7,fig.height=7,fig.cap="Daily close price of S&P500 from yahoo finance website"}
chartSeries(SP500,theme= "white", name="S&P500 Index")
```

```{r echo=FALSE,comment="",fig.width=7,fig.height=7,fig.cap="Daily close price of Suncor energy INC. from yahoo finance website"}
#figure 2
chartSeries(SU,theme="white", name= "Suncor Energy")
```

![Daily prices of crude oil, Natural gas, gold and Canada to US exchange rate from 2012 to 2017 extracted from FRED website ](F:/my document/other people/Farzane_London_Regression/graphs(pictures)/merge_from_ofoct_crop.png)

\newpage

```{r comment="", include=FALSE}
getSymbols(c("DCOILWTICO","DEXCAUS","DGS3MO","GOLDAMGBD228NLBM","DHHNGSP"),src="FRED",adjust = T,from = '2012-01-01', to = '2017-01-01')
```


```{r include=FALSE}
fred.data0 <- merge(DGS3MO,GOLDAMGBD228NLBM,DEXCAUS,DCOILWTICO,DHHNGSP)['2012-01-01::2017-01-01']
apply(is.na(fred.data0), 2, sum)
library(ggfortify)

yahoo.data0 <- merge(SU$SU.Close,SP500$GSPC.Close)
names(yahoo.data0) <- c("SU", "SP500")
yahoo.data0.0 <- zoo(x = coredata(yahoo.data0), order.by = time(yahoo.data0))

```


```{r include=FALSE}
data <- merge(yahoo.data0.0, fred.data0)
index.no <- which(is.na(coredata(data$SP500))==FALSE)
CS1.0 <- data[index.no,]
apply(is.na(CS1.0), 2, sum)

cs1.1 <- na.locf(CS1.0)
apply(is.na(cs1.1), 2, sum)
save(file = 'cs1_1.RData', list = ls())
dataset<-cs1.1
```

```{r, include=FALSE}
head(dataset)
colnames(dataset) <- c("SU", "SP500", "T-BILL", "GOLD", "CAUS","OIL","GAS")
str(dataset)
apply(is.na(dataset), 2, sum)
```
All variables are numerical and the dataset doesn't have any categorical variable.There is no missing value as well. 

```{r include=FALSE}
dataset1<- dataset[ ,-3]
head(dataset1)
nrow(dataset1)
fix(dataset1) 
write.csv(dataset1, file = "Data_new.csv")
MyData <- read.csv(file="C:/Users/Maede/Documents/Data_new.csv", header=TRUE, sep=",")
MyData$X <- NULL
```

```{r DATAINPUT, echo=FALSE, results="asis"}
stargazer(MyData, type="latex", title="Dataframe Summary", header=FALSE )
```

#Section 2: Why use the logarithm of returns, rather than price or raw returns?

The purpose of this section is to demonstrate the idea behind logarithmic transformation of returns on our dataset.
First, I look at scatter plot matrix to roughly determine if there is a linear correlation between my variables. Second, I run a regression of daily Suncor stock price on other variables, and look at the diagnostic checks to see how well is the plots. finally, I use Durbin-Watson test to detect the presence of autocorrelation.

##Correlation matrix

```{r include=FALSE,comment=""}
library(psych)
``` 

```{r echo=FALSE,fig.height=8, fig.width=8, fig.cap="Correlation Matrix"}
pairs.panels(dataset1, bg=c("blue","red","yellow"), pch=25,lm=TRUE)
```

1.	The correlation matrix suggests that CAUS, OIL and GAS have significant correlations with the response variable SU.

2.	It seems that, there is a collinearity between gas and oil variable.

3.	The relationship between Suncor stock price and other variables are not linear.


\newpage
###MODEL 1:

$$SU = \beta_0 + \beta_1 (SP500) + \beta_2 (OIL) + \beta_3 (GOLD) +\beta_4 (CA/US) +\beta_5 (GAS) + error.$$
Where:

SU: Daily stock price for Suncor Energy Inc. which is a Canadian integrated energy company based in Calgary, Alberta. It specializes in production of synthetic crude from oil sands, based US dollars.

SP500: daily index price for Standard & Poor's 500, often abbreviated as the S&P 500, which is an American stock market index based on the market capitalizations of 500 large companies having common stock listed on the NYSE, based US dollars.

GOLD: daily gold fixing price 10:30 A.M. (London time) in London Bullion Market, based US dollars.

OIL: daily crude Oil Price: West Texas Intermediate (WTI) - Cushing, Oklahoma, based in US dollars per barrel, base US dollars.

GAS: daily of Henry Hub Natural Gas Spot Price, based in US dollars per million BTU, based US dollars.

CAUS: daily Canadian Dollars to One U.S. Dollar exchange rate. 

##Summary of model 1 and diagnostic plots:

```{r include=FALSE, results="asis"}
lmfit1 <- lm(SU ~ SP500+ GOLD + CAUS+ OIL+ GAS , data = MyData)
```

```{r FIT-lmfit1, echo=FALSE, results="asis"}
#Table 2
stargazer(lmfit1, type="latex", title="Model Summary", header=FALSE)
```

* All explanatory variables are significant at 0.01% level.

* Although the R^2 value is quite high (75.79%), the fitted line plots of SU vs explanatory variables suggests that the relationship between Suncor Energy stock price and other variables is not linear.

* The residual versus fitted plot also suggests that the relationship is not linear. Because the lack of linearity dominates the scale location plot, we can not use the plot to evaluate whether the error variances are equal.

```{r PLOT-lmfit1, echo=FALSE, fig.height=5, fig.width=5, fig.cap="Basic Regression Diagnostic Checks"}
#Fig 5
layout(matrix(c(1,2,3,4),2,2))
plot(lmfit1)
layout(1)
```

* We must fix the non-linearity problem before we assess the assumption of equal variances.

* The Q-Q plot suggests that the error terms are not normal; There is sufficient evidence to conclude that the error terms are not normally distributed.

##VIF test

The variance inflation factors (VIF)for CAUS and OIL are high. The higher the value of VIF, the higher the collinearity.

\newpage

```{r VIF-barchartfig, echo=FALSE, fig.height=4, fig.width=5, fig.cap="VIF for input variables."}
ttf <- vifx(MyData[,c("SP500", "GOLD","CAUS", "OIL","GAS")])
barplot(ttf, col="blue", ylab="VIF", ylim=c(0, 22))
abline(h=18, lty=5, col="red", lwd=5)
```

##Durbin-Watson Test

```{r echo=FALSE}
durbinWatsonTest(lmfit1)
```


1.The null hypothesis is that, there is no autocorrelation. We can safely reject the null at 1%. There is evidence for autocorrelation. 

2.Because the dataset is time series, and the most common issue with this type of data is autocorrelation, as we expected, independence of residuals has violated in this model. One of the common method to solve this problem, is transformation. 

In finance, it is very common to work with the logarithmic return or continuously compounded return, also known as force of interest, and a standard approach is to apply a natural log transformation to the return of stock prices before fitting a regression model. One of the justification for this method is due to ease of interpretation for coefficients. For example, if a stock is priced at 3.570 USD per share at the close on one day, and at 3.575 USD per share at the close the next day, then the logarithmic return is: ln(3.575/3.570) = 0.0014, or 0.14%.

Also, logarithmic returns are widely preferred over raw prices or returns in quantitative analysis of financial time series for various other reasons such as normalization (returns of different assets can be compared, their prices usually not), time-additivity, and other conveniences for classical statistics and mathematics.


#Section 2

Separating data into training and testing sets is an important part of evaluating data mining models. Typically, when we separate a data set into a training set and testing set, most of the data is used for training, and a smaller portion of the data is used for testing. Analysis Services randomly samples the data to help ensure that the testing and training sets are similar. By using similar data for training and testing, we can minimize the effects of data discrepancies and better understand the characteristics of the model. For this purpose, I allocate randomly 75% of my data to training set, and 25% to test set. I do all my analysis on my training set. 

###MODEL 2:

Model 2 which is inspired by CAPM (Capital asset pricing model):

$$(R_{su} - r_{rf})  = \beta_0 + \beta_1 (R_M - r_{rf} ) + \beta_2 (R_{oil} )  + \beta_3 (R_{gold} ) +\beta_4 (R_{ca/us}) +\beta_5 (R_{gas} ) + error.$$

where:

r.SU.0=r.SU-$r_{rf}$

r.SP500.0= r.sp500-$r_{rf}$

$r_{rf}$  is the rate of return for a risk-free security. Other variables are the ones that we defined in Data section.

##Correlation matrix: 

```{r include=FALSE}
r.SU <- zoo(x = as.matrix(diff(log(dataset[, "SU"]))), order.by = time(dataset[-1]))
dimnames(r.SU)[[2]] <- "r.SU"
r.GOLD <- zoo(x = as.matrix(diff(log(dataset[, "GOLD"]))), order.by = time(dataset[-1]))
dimnames(r.GOLD)[[2]] <- "r.GOLD"
r.OIL <- zoo(x = as.matrix(diff(log(dataset[, "OIL"]))), order.by = time(dataset[-1]))
dimnames(r.OIL)[[2]] <- "r.OIL"
r.GAS <- zoo(x = as.matrix(diff(log(dataset[, "GAS"]))), order.by = time(dataset[-1]))
dimnames(r.GAS)[[2]] <- "r.GAS"
r.CAUS <- zoo(x = as.matrix(diff(log(dataset[, "CAUS"]))), order.by = time(dataset[-1]))
dimnames(r.CAUS)[[2]] <- "r.CAUS"
r.SP500 <- zoo(x = as.matrix(diff(log(dataset[, "SP500"]))), order.by = time(dataset[-1]))
dimnames(r.SP500)[[2]] <- "r.SP500"
r.riskfree <- log(1+0.01*coredata(dataset[-1, "T-BILL"])* diff(as.numeric(time(dataset)))/360)
dimnames(r.riskfree)[[2]] <- "r.riskfree"
# compute the difference 
r.SU.0 <- r.SU - r.riskfree
dimnames(r.SU.0)[[2]] <- "r.SU.0"
r.SP500.0 <- r.SP500 - r.riskfree
dimnames(r.SP500.0)[[2]] <- "r.SP500.0"
r.data0 <- merge(
        r.SU.0,
        r.SP500.0)
r.dataset <- merge(
        r.data0,
        r.GAS,
        r.OIL,
        r.CAUS,
        r.GOLD)
index1<- sample(1:nrow(r.dataset),round(0.75*nrow(r.dataset)))
train<-r.dataset[index1,]
test<- r.dataset[-index1,]
head(train)
```


* As we can see, the excess rate of return of market over risk-free rate(r.SP500.0), log return of oil price(r.OIL) and log return of Canada to US dollar Exchange rate(r.CAUS) have significant correlations with excess rate of return of Suncor energy stock price over risk-free rate(r.SU.0)

* All correlations are below 0.55, which shows that, there is no evidence of collinearity in new model.

* Correlation between r.SU.0 and r.CAUS is negative, and is positive with other variables.


```{r echo=FALSE,fig.height=8, fig.width=8, fig.cap="Correlation Matrix"}
pairs.panels(r.dataset, bg=c("blue","red","yellow"), pch=25,lm=TRUE)
```

\newpage

##Summary of model 2:

```{r include=FALSE, results="asis"}
lmfit2 <- lm(r.SU.0 ~ r.SP500.0 +r.OIL+ r.CAUS+ r.GOLD+ r.GAS , data = train)

```

```{r FIT-lmfit2, echo=FALSE, results="asis"}
stargazer(lmfit2, type="latex", title="Model Summary", header=FALSE)
```

The regression is definitely significant at less than 0.1%.

1.$R^2= 0.5162$, The model explains approximately 52% of the variability in the logarithmic daily return of Suncor Energy Stock price. 

2.Slope estimate for log daily return of S&P500, log Daily return of Crude Oil and log daily return of exchange rate (Canada to US dollar) is significant at less than 0.1%.

3.The intercept is almost 0, and the p-value for intercept is high. I don't have enough evidence to reject the null hypothesis that intercept is equal to 0 .IT makes sense in finance. I can interpret that as "the investment has earned a return adequate for the risk taken".


##Stagewise

```{r include=FALSE}
lmfitBIC <- step(lmfit2, k=log(nrow(train)))
```

According to this procedure, the best model , based on BIC criteria ,is the one that includes the variables log daily return of S&P500, log daily return of oil and log daily return of exchange rate (Canada to US dollar).

\newpage

##Summary of final model (BIC model):

```{r FIT-lmfitBIC, echo=FALSE, results="asis"}
stargazer(lmfitBIC, type="latex", title="Model Summary", header=FALSE)
```

##Diagnostic plots:

1.From the normal Q-Q plot we see that the assumption of residuals being normally distributed does not seem to be violated. 

2.The Residuals vs fitted, and Scale-Location plots agree that the residuals are independently and identically distributed. There is no fan shape in the residual plot, however there are some sign of violation of homoscedasticity that we can check it by NCV test.

3.The Residuals vs Leverage plot shows that there is no concern.

This test has a p-value more that a significance level of 0.05, therefore we can not reject the null hypothesis that the variance of the residuals is constant.

```{r PLOT-lmfitBIC, echo=FALSE, fig.height=8, fig.width=7, fig.cap="Basic Regression Diagnostic Checks"}
layout(matrix(c(1,2,3,4),2,2))
plot(lmfitBIC)
layout(1)
```

\newpage

##VIF test

Variance Inflation Factor(Figure 9) â In general, the VIFs of the linear regression indicate the degree that the variances in the regression estimates are increased due to multicollinearity. VIFs values are very small, indicate that multicollinearity is not a problem in this fit.

As we can see, the transformation helped me to overcome the problem with multicollinearity.

```{r VIF-barchartfig1, echo=FALSE, fig.height=4, fig.width=5, fig.cap="VIF for input variables"}
ttf <- vif(lmfit2)
barplot(ttf, col="blue", ylab="VIF", ylim=c(0, 2))
abline(h=1.40, lty=5, col="red", lwd=5)
```

##Durbin-Watson Test

```{r echo=FALSE}
durbinWatsonTest(lmfit1)
```

\newpage

The null hypothesis is that there is no autocorrelation. According to p-value, we don't have enough evidence to reject null hypothesis. there is no evidence of autocorrelation.



#Chapter 4: Model validation

##Final model:

$$(R_{su} - r_{rf})  = \beta_0 + \beta_1 (R_M - r_{rf} ) + \beta_2 (R_{oil} )  +\beta_3 (R_{ca/us}) + error.$$

Comparing root mean square error for testing and training set:

```{r include=FALSE, comment=""}
rmse.train<- summary(lmfitBIC)$sigma
yFull <- predict(lmfitBIC, newdata=test)
library(Metrics) 
rmse.test<- rmse(test$r.SU.0, yFull)
```

```{r echo=FALSE, results="asis"}
rmse = matrix(c("RMSE.train", "RMSE.test", rmse.train,rmse.test),nrow=2,ncol=2)
rmse
```

1.I Predicted the test dataset and computed the RMSE on the test dataset and compared it with the training dataset. 

2.According to the result from summary for final model and RMSE for test dataset, result seems good. RMSE for testing and training dataset is not very different.   

##Time Series Diagnostic Checks for Residuals in OLS Model


```{r echo=FALSE, fig.height=6, fig.width=5, fig.cap="Time series plot"}
tsdiag(arima(resid(lmfitBIC)))
```

*	In the time series plot, there is no obvious systematic departures from randomness such as trend, seasonality, clustering.
*	The SACF plot is used to detect if there is strong autocorrelation present. According to plot, residuals are independent.
* There is no evidence of heteroscedasticity in this dataset.

\newpage

#Section5: conclusion and recommendation
This report contributes to the determination of the character of excess of daily stock return of Suncor Energy over risk free rate (r.SU.0) based on excess of daily market return over risk free rate(r.SP500.0) and commodities including daily return of oil(r.OIL), gas(r.GAS), gold(r.GOLD), Canada to US currency exchange rate(r.CAUS). Our final analysis shows significant relationship between r.SU.0 values and r.SP500.0, r.OIL, r.CAUS. In last two chapters we checked for any violation of linear assumption, and could not find any obvious evidence, however In 2003 Granger and his collaborator Robert Engle were jointly awarded the Nobel Memorial Prize in Economic Sciences for demonstrating that, the stock market prices exhibit conditional variance changes depending on the past value. Therefore a Garch(1,1) could be a better fit for this type of data.
