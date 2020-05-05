Calgary Property Assessment: Statistical Analysis
================
Atlanta Liu, Dany Hachem, Greg Cameron
Fall 2020

``` r
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.2.1 --

    ## v tibble  2.1.3     v purrr   0.3.2
    ## v tidyr   1.0.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## Warning: package 'tidyr' was built under R version 3.6.3

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(mosaic)
```

    ## Loading required package: lattice

    ## Loading required package: ggformula

    ## Loading required package: ggstance

    ## 
    ## Attaching package: 'ggstance'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     geom_errorbarh, GeomErrorbarh

    ## 
    ## New to ggformula?  Try the tutorials: 
    ##  learnr::run_tutorial("introduction", package = "ggformula")
    ##  learnr::run_tutorial("refining", package = "ggformula")

    ## Loading required package: mosaicData

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

    ## Registered S3 method overwritten by 'mosaic':
    ##   method                           from   
    ##   fortify.SpatialPolygonsDataFrame ggplot2

    ## 
    ## The 'mosaic' package masks several functions from core packages in order to add 
    ## additional features.  The original behavior of these functions should not be affected by this.
    ## 
    ## Note: If you use the Matrix package, be sure to load it BEFORE loading mosaic.

    ## 
    ## Attaching package: 'mosaic'

    ## The following object is masked from 'package:Matrix':
    ## 
    ##     mean

    ## The following object is masked from 'package:purrr':
    ## 
    ##     cross

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     stat

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     count, do, tally

    ## The following objects are masked from 'package:stats':
    ## 
    ##     binom.test, cor, cor.test, cov, fivenum, IQR, median,
    ##     prop.test, quantile, sd, t.test, var

    ## The following objects are masked from 'package:base':
    ## 
    ##     max, mean, min, prod, range, sample, sum

``` r
library(mosaicData)
library(mdsr)
library(devtools)
```

    ## Loading required package: usethis

``` r
library(binom)
library(resampledata)
```

    ## 
    ## Attaching package: 'resampledata'

    ## The following object is masked from 'package:datasets':
    ## 
    ##     Titanic

``` r
library(ISLR)
options(scipen = 999)
```

``` r
AllData = read.csv('AllData.csv')
head(AllData)
```

    ##   Community.Name Year Assessed.Value Crime.Count Oil.Price Population
    ## 1      ABBEYDALE 2005         154000           0  57.28417       6040
    ## 2      ABBEYDALE 2006         159000           0  66.96167       6012
    ## 3      ABBEYDALE 2007         228500           0  74.94417       6109
    ## 4      ABBEYDALE 2008         310500           0  98.58333       6005
    ## 5      ABBEYDALE 2009         294000           0  63.92333       6032
    ## 6      ABBEYDALE 2010         246000           0  79.98500       5805

## Cleaning & Filtering Dataframe

``` r
#Select which community to build a linear model of
Community = 'PANORAMA HILLS'
Community2 = 'ROXBORO'

#Look at the subset of the dataframe for this community, focusing on all years before 2018
#No Population data is available for years 2018 and 2019
#Remove crime count, we are only interested in comparing assessed value to oil price and population separately for all the years.
DFcommunity = subset(AllData, Community.Name == Community)
DFcommunity = subset(DFcommunity, Year != 2018 & Year != 2019)
DFcommunity = subset(DFcommunity, select = -c(Crime.Count))
#DFcommunity

DFcommunity2 = subset(AllData, Community.Name == Community2)
DFcommunity2 = subset(DFcommunity2, Year != 2018 & Year != 2019)
DFcommunity2 = subset(DFcommunity2, select = -c(Crime.Count))
#DFcommunity2
```

\#\#Statisical Modelling of Bivariate Data:

Is there a relationship between Population Count (Variable X) & Property
Assessment (Variable Y)? <br /> - Lets first look at the scatterplot to
get a better idea of our data snapshot by finding the correlation.

``` r
#Community 1 = High Population Count
ggplot(data=DFcommunity, aes(x = Population, y = Assessed.Value)) + geom_point(col="blue", size=2, position="jitter") + xlab("Population Count of Community") + ylab("Median Property Assessment") + ggtitle("Scatterplot of Population Count to Median Property Assessment in:", Community) + stat_smooth(method = "lm", col = 'red')
```

![](Project-Property-Assessment_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
CORcommunity = cor(~Population, ~Assessed.Value, data = filter(DFcommunity))

#Community 2 = Low Population Count
ggplot(data=DFcommunity2, aes(x = Population, y = Assessed.Value)) + geom_point(col="brown", size=2, position="jitter") + xlab("Population Count of Community") + ylab("Median Property Assessment") + ggtitle("Scatterplot of Population Count to Median Property Assessment in:", Community2) + stat_smooth(method = "lm", col = 'purple')
```

![](Project-Property-Assessment_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
CORcommunity2 = cor(~Population, ~Assessed.Value, data = filter(DFcommunity2))
```

1.  Correlation of Population to Property Assessment in: PANORAMA HILLS
    = 0.7448511 <br />
2.  Correlation of Population to Property Assessment in: ROXBORO =
    0.2024057

## Building the linear model for both communities:

``` r
#Linear Model for both communities (high and low population respectively)
LMcommunity = lm(Assessed.Value ~ Population, DFcommunity)
LMcommunity2 = lm(Assessed.Value ~ Population, DFcommunity2)

#Find the coefficients to build a linear model: y = a + bx
print(LMcommunity$coefficients)
```

    ##   (Intercept)    Population 
    ## 248924.169062      9.109066

``` r
print(LMcommunity2$coefficients)
```

    ## (Intercept)  Population 
    ##  766811.031    1112.444

Our linear function for both communities:

PANORAMA HILLS (Assessed Value) = 248924.2 + 9.109066 X(population)
ROXBORO (Assessed Value) = 766811 + 1112.444
X(population)

## Model Condition 1: The Variable y (Median Property Assessment) must be normally distributed

``` r
#Normal Probability Plot for residual terms:
ggplot(LMcommunity, aes(sample = LMcommunity$residuals)) +  stat_qq(col='blue') + stat_qqline(col='red') + ggtitle("Normal Probability Plot of Residuals In", Community)
```

![](Project-Property-Assessment_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggplot(LMcommunity2, aes(sample = LMcommunity2$residuals)) +  stat_qq(col='brown') + stat_qqline(col='purple') + ggtitle("Normal Probability Plot of Residuals In", Community2)
```

![](Project-Property-Assessment_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->
Interpretating result from the normal probability plot of the two
communities: <br /> 1 - From our normal probability plot in the high
population model, we see that the residuals (ei) appear to conform to a
normal distribution. There are a few data points towards the end of the
lind that separate away, but the vast majority of the points approximate
the normal line. As a result, this condition is passed for: PANORAMA
HILLS

2 - In the normal probability plot of the low population model, the
residuals do not appear to conform to a normal distribution. The upper
and lower ends vary greatly from the normal line. As a result, this
condition is failed to be upheld in:
ROXBORO

## Model Condition 2: Inspect homoscedascity condition by comparing fitted values with residuals

``` r
#Checking homoscedascity condition in both communities:
ggplot(LMcommunity, aes(x = LMcommunity$fitted.values, y = LMcommunity$residuals)) +  geom_point(size=2, col='blue', position="jitter") + xlab("Population Count") + ylab("Residuals") + ggtitle("Plot of Fits to Residuals in", Community) + geom_hline(yintercept=0, color="red", linetype="dashed")
```

![](Project-Property-Assessment_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot(LMcommunity2, aes(x = LMcommunity2$fitted.values, y = LMcommunity2$residuals)) +  geom_point(size=2, col='brown', position="jitter") + xlab("Population Count") + ylab("Residuals") + ggtitle("Plot of Fits to Residuals in", Community2) + geom_hline(yintercept=0, color="purple", linetype="dashed")
```

![](Project-Property-Assessment_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->
Interpretation of condition 2: 1 - There appears to be a wedge shape to
the distribution of the points. The distance from the initial points
deviate largely from the horizontal line (e = 0), and slowly converge on
the centre as the population count increases. As a result, the common
variance condition is not fulfilled in the higher populated community
of: PANORAMA HILLS

2 - This chart also shows a large wedge shape, however it is in the
opposite direction when compared to the community with the larger
population. As the population count increases, the spread of the
residuals also increase. Overall, we see that the homoscedascity
condition is not fulfilled in the lower-populated community of: ROXBORO

## Confidence Interval Testing for the value of B:

``` r
#Gather confidence intervals of B for both communities:
conf1 = confint(LMcommunity, level = 0.95)
conf2 = confint(LMcommunity2, level = 0.95)
```

Hypothesis Statement for B:

Null Hypothesis H(0): B = 0 (Community Median Property Assessment cannot
be expressed as a linear function of Population<br /> Alt. Hypothesis
H(A): B =/= 0 (Community Median Property Assessment can be expressed as
a linear function of Population

State confidence intervals and interpretation:

1 - PANORAMA HILLS <br /> 95% confidence interval of B = 3.694032 and
14.5241

The confidence interval shows the rate at which the median property
assessments will change with each increase in community population
count. At a 95% certainty, the range of this increase will be between
3.694032 and 14.5241

2 - ROXBORO <br /> 95% confidence interval of B = -2459.402 and 4684.29

The confidence interval for the lower populated community shows a 95%
certainty that the rate at which median property <br /> assessments will
change is within -2459.402 and
4684.29

## Diving further into the significance of the confidence intervals through analysis of variance:

``` r
#Acquire F(observed) values and their respective p-value to test for significance
#Note that a similar value is also achieved when using: coefficients(summary(LMcommunity))
Fobs1 = summary(aov(LMcommunity))
Fobs2 = summary(aov(LMcommunity2))
Fobs1
```

    ##             Df      Sum Sq     Mean Sq F value  Pr(>F)   
    ## Population   1 40505667057 40505667057   13.71 0.00349 **
    ## Residuals   11 32503409866  2954855442                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Analysis of Variance Interpretation:

1 - PANORAMA HILLS

F-Observed = 13.71, P-value = 0.00349

From our analysis of variance, we see that the F-observed value is quite
large(13.71). This value represents the ratio between explained and
unexplained variance with respect to the fitted values in our linear
model. A large F-ratio is a good indicator that the true means of both
variables ‘population count’ and ‘median property assessments’ are not
equal. Our P-value(0.00349) reveals that this is indeed the case and we
should reject our null hypothesis in favor of the alternative. This
suggests that population count can be expressed as a linear function of
median property assessment in PANORAMA HILLS

2 - ROXBORO

F-Observed = 0.47, P-value = 0.507

In this analysis, the F-value is quite small(0.47). This indicates that
the true means of both groups are close to equal. Our P-value(0.507)
reveals that we fail to reject our null hypothesis and continue to
assume that population count cannot be expressed as a linear function of
median property assessment in ROXBORO

## Coefficient of Determination:

``` r
#Compute coefficient of determination by using rsquared()
rcom1 = rsquared(LMcommunity)
rcom2 = rsquared(LMcommunity2)
```

Interpretation of the coefficient of determination (r^2): <br /> 1 -
PANORAMA HILLS <br /> rsquared = 0.5548032 <br /> 55.48032 percentage of
all variation in median property assessments can be attributed to its
linear dependency of population count

2 - ROXBORO <br /> rsquared = 0.04096805 <br /> 4.096805 percentage of
all variation in median property assessments can be attributed to its
linear dependency of population count.

## Prediction

Using Model as an Attempt to
Predict:

``` r
#Predict the median property assessment given than the population count in 2020 will be 5767.45
#This number is provided to us through the population projection data in the city of Calgary [1]
#Looking under the community profile of Abbeydale, the percent population change is expected to decrease by 5% from 2014 (0.95 x 6071 = 5767.45).
#Use interval='predict', as we are only interested in finding one value
predict1 = predict(LMcommunity, newdata = data.frame(Population = 5767.45), interval = 'predict', conf.level = 0.95)

#Roxboro does not have a projected change in population in 2020, so this model will attempt to predict the property assessment value given its population in 2017. [2]
predict2 = predict(LMcommunity2, newdata = data.frame(Population = 390), interval = 'predict', conf.level = 0.95)
```

Interpreting predicted results: <br /> 1 - PANORAMA HILLS <br /> The
estimated median property assessment given that the projected population
count will be 5767.45 in 2020 is between 157992.7 and 444927.8

2 - ROXBORO <br /> The estimated median property assessment given that
the projected population count will be 390 in 2020 is between 560568.1
and 1840760

\#Bootstrapping the means of A, B, & r Since both of our communities did
not successfully pass the test for all the conditions (normality &
homoscedascity), a bootstrap will be used here to approxmate the values
of a, b, and r.

\#\#Bootstrapping Panorama Hills

``` r
nsize = dim(DFcommunity)[1]
Nbootstraps = 1000 #amount to resample
cor.boot = numeric(Nbootstraps)
a.boot = numeric(Nbootstraps)
b.boot = numeric(Nbootstraps)


for (i in 1:Nbootstraps){
  index = sample(nsize, replace = TRUE)
  COM.boot = DFcommunity[index,]
  
  cor.boot[i] = cor(~Population, ~Assessed.Value, data = COM.boot)
  COM.lm = lm(Assessed.Value~Population, data = COM.boot)
  a.boot[i] = coef(COM.lm)[1]
  b.boot[i] = coef(COM.lm)[2]
}

BootComResults = data.frame(cor.boot, a.boot, b.boot)
```

Bootstrap Distribution of r(b00t) in Panorama
Hills

``` r
ggplot(BootComResults, aes(x = cor.boot)) + geom_histogram(col="white", fill="purple", binwidth=0.05) + xlab("Values of the Bootstrap Statistic: Correlation Coefficient") + ylab("Count") + ggtitle("Distribution of Bootstrap Statistics: r")
```

![](Project-Property-Assessment_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
favstats(~cor.boot, data = BootComResults)
```

    ##         min        Q1    median        Q3       max      mean        sd
    ##  -0.3757803 0.6351981 0.7596463 0.8396634 0.9953464 0.7163168 0.1904687
    ##     n missing
    ##  1000       0

``` r
qdata(~cor.boot, c(0.025, 0.975), data = BootComResults)
```

    ##        quantile     p
    ## 2.5%  0.2531092 0.025
    ## 97.5% 0.9683135 0.975

The mean of all rboot values is 0.7115, the 95% confidence interval for
these computed values is 0.2011 and 0.8375. <br /> Specifically, this
interval represents that the mean linear correlation between population
count and median property assessments in Panorama Hills will be: 0.2011
\<= r \<= 0.8375

Bootstrap Distribution of a(b00t) in Panorama
Hills

``` r
ggplot(BootComResults, aes(x = a.boot)) + geom_histogram(col="white", fill="red") + xlab("Values of the Bootstrap Statistic: y-Intercept Estimate") + ylab("Count") + ggtitle("Distribution of Bootstrap Statistics: a")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Project-Property-Assessment_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
favstats(~a.boot, data = BootComResults)
```

    ##       min       Q1   median       Q3      max     mean       sd    n
    ##  123639.4 209029.3 246237.4 294375.5 494004.5 257779.6 69278.77 1000
    ##  missing
    ##        0

``` r
qdata(~a.boot, c(0.025, 0.975), data = BootComResults)
```

    ##       quantile     p
    ## 2.5%  149163.5 0.025
    ## 97.5% 425330.9 0.975

The mean of all aboot values is 260080.1, with a 95% confidence interval
between 150478.1 and 426353.9. <br /> This suggests that when the
population count for this community is 0, the average median property
assessment for Panorama Hills will be: 150478.1 \<= B \<= 426353.9

Bootstrap Distribution of b(b00t) in Panorama
Hills

``` r
ggplot(BootComResults, aes(x = b.boot)) + geom_histogram(col="white", fill="blue") + xlab("Values of the Bootstrap Statistic: Slope Estimate") + ylab("Count") + ggtitle("Distribution of Bootstrap Statistics: b")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Project-Property-Assessment_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
favstats(~b.boot, data = BootComResults)
```

    ##       min       Q1   median       Q3      max     mean       sd    n
    ##  -2.53927 7.167501 9.257572 10.87193 14.99259 8.714362 2.999952 1000
    ##  missing
    ##        0

``` r
qdata(~b.boot, c(0.025, 0.975), data = BootComResults)
```

    ##        quantile     p
    ## 2.5%   1.549126 0.025
    ## 97.5% 13.196072 0.975

The mean of all bboot values is 8.6247, with a 95% confidence interval
between 1.4181 and 13.2026. <br /> Overall, this suggests that for each
increment in population count, the average median property assessment
value will increase by: 1.4181 \<= B \<= 13.2026

\#\#Bootstrapping Roxboro

``` r
nsize2 = dim(DFcommunity2)[1]
Nbootstraps2 = 1000 #amount to resample
cor2.boot = numeric(Nbootstraps)
a2.boot = numeric(Nbootstraps)
b2.boot = numeric(Nbootstraps)


for (i in 1:Nbootstraps2){
  index2 = sample(nsize2, replace = TRUE)
  COM2.boot = DFcommunity2[index2,]
  
  cor2.boot[i] = cor(~Population, ~Assessed.Value, data = COM2.boot)
  COM2.lm = lm(Assessed.Value~Population, data = COM2.boot)
  a2.boot[i] = coef(COM2.lm)[1]
  b2.boot[i] = coef(COM2.lm)[2]
}

BootComResults2 = data.frame(cor2.boot, a2.boot, b2.boot)
```

Bootstrap Distribution of r(b00t) in
Roxboro

``` r
ggplot(BootComResults2, aes(x = cor2.boot)) + geom_histogram(col="white", fill="purple", binwidth=0.05) + xlab("Values of the Bootstrap Statistic: Correlation Coefficient") + ylab("Count") + ggtitle("Distribution of Bootstrap Statistics: r")
```

![](Project-Property-Assessment_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
favstats(~cor2.boot, data = BootComResults2)
```

    ##         min        Q1    median        Q3       max      mean        sd
    ##  -0.4816568 0.1406434 0.2514587 0.3924886 0.8939701 0.2602189 0.1824003
    ##     n missing
    ##  1000       0

``` r
qdata(~cor2.boot, c(0.025, 0.975), data = BootComResults2)
```

    ##          quantile     p
    ## 2.5%  -0.07730386 0.025
    ## 97.5%  0.61321638 0.975

The mean of all rboot values is 0.2645, the 95% confidence interval for
these computed values is -0.0772 and 0.6271 <br /> Specifically, this
interval represents that the mean linear correlation between population
count and median property assessments in Roxboro will be within: -0.0772
\<= r \<= 0.6271. But since the confidence interval captures zero, this
is a strong indicator that there is no statisical relation between
population count and property assessment values in Roxboro.

Bootstrap Distribution of a(b00t) in
Roxboro

``` r
ggplot(BootComResults2, aes(x = a2.boot)) + geom_histogram(col="white", fill="red", bins = 35) + xlab("Values of the Bootstrap Statistic: y-Intercept Estimate") + ylab("Count") + ggtitle("Distribution of Bootstrap Statistics: a")
```

![](Project-Property-Assessment_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
favstats(~a2.boot, data = BootComResults2)
```

    ##        min        Q1   median       Q3     max     mean      sd    n
    ##  -19029219 -383443.5 696585.5 883738.1 5069712 67456.56 1678319 1000
    ##  missing
    ##        0

``` r
qdata(~a2.boot, c(0.025, 0.975), data = BootComResults2)
```

    ##       quantile     p
    ## 2.5%  -3889046 0.025
    ## 97.5%  1332368 0.975

The mean of all aboot values is 75001.42, with a 95% confidence interval
between -4062839 and 1312846.<br /> This suggests that when the
population count for this community is 0, the average median property
assessment for Panorama Hills will be: -4062839 \<= B \<= 1312846. While
this is the mathematical output, we know that property assessments
cannot be negative.

Bootstrap Distribution of b(b00t) in
Roxboro

``` r
ggplot(BootComResults2, aes(x = b2.boot)) + geom_histogram(col="white", fill="blue") + xlab("Values of the Bootstrap Statistic: Slope Estimate") + ylab("Count") + ggtitle("Distribution of Bootstrap Statistics: b")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Project-Property-Assessment_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
favstats(~b2.boot, data = BootComResults2)
```

    ##        min       Q1   median       Q3      max   mean       sd    n
    ##  -9875.641 752.2979 1403.025 3782.505 48099.86 2777.2 4056.753 1000
    ##  missing
    ##        0

``` r
qdata(~b2.boot, c(0.025, 0.975), data = BootComResults2)
```

    ##        quantile     p
    ## 2.5%   -696.386 0.025
    ## 97.5% 12124.870 0.975

The mean of all bboot values is 2765.528, with a 95% confidence interval
between -580.9235 and 12675.0474. <br /> Overall, this suggests that for
each increment in population count, the average median property
assessment value will change by: -580.9235 \<= B \<= 12675.0474

Y\_{Panorama Hills Assessment} = 260080.1 + 8.7166X\_{population}
Y\_{Roxboro Assessment} = 63571.42 + 2790.694X\_{population}

## References

\[1\]
<https://www.calgary.ca/CSPS/CNS/Documents/community_social_statistics/Abbeydale_pp.pdf>
\[2\]
<https://www.calgary.ca/CSPS/CNS/Documents/community_social_statistics/Roxboro_pp.pdf>
