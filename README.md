Health Insurance Cost Modeling
================
Kyle Offenloch
2025-08-30

## Data used in this Project:

Health Insurance dataset from kaggle.com

<https://www.kaggle.com/datasets/willianoliveiragibin/healthcare-insurance>

Includes information on attributes of 1338 insured individuals,
including their age, sex, BMI, number of children, smoking habits, and
region. It also includes their medical costs incurred.

## Goal of this project:

- Analyze data to infer effects of other variables on medical costs.
- Create generalized linear models using different assumptions to
  predict costs.
- Test and compare models.

# General Analysis

First, we break down the data to look at the entire picture.

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

These are the histograms, bar charts, and pie charts of each of the 7
attributes in this data set, helping us see the general frequency and
distributions of the variables.

Now we will plot medical charges against different attributes to deduce
the relationship, also including smoking status in each chart.

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Big difference in Charges by smoking status.

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Positive trend in charges with age, sizable difference between smoker
and non-smoker charges, with smoker charges having very high variance.

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Very little trend in charges with BMI for nonsmokers, large jump in
charges for smokers with BMI over 30.

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

No trend in charges with no. of children, region or sex.

## Conclusion of attributes effects on charges

- Smoking has the largest effect on charges, showing up in every other
  chart when plotted with other attributes
- Age has a slight effect on charges the higher it is
- BMI has a quite large effect on charges in smokers, but not much for
  nonsmokers

# Building Models

We will build six different models, all generalized linear models:

Model 1: Normal linear regression model with all six variables. This
model will include unrealistic negative cost predictions as a side
effect of assuming gaussian error terms and fully linear relationships
between variables

``` r
model1<-lm(charges ~ age + sex + bmi + children + smoker + region, data = train_data)
summary(model1)
```

    ## 
    ## Call:
    ## lm(formula = charges ~ age + sex + bmi + children + smoker + 
    ##     region, data = train_data)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -11279  -3044  -1003   1708  30064 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -12440.30    1262.15  -9.856  < 2e-16 ***
    ## age                238.85      15.20  15.715  < 2e-16 ***
    ## sexmale           -220.97     422.62  -0.523   0.6012    
    ## bmi                371.90      35.83  10.381  < 2e-16 ***
    ## children           693.87     171.06   4.056 5.43e-05 ***
    ## smokeryes        24123.82     510.83  47.225  < 2e-16 ***
    ## regionnorthwest   -607.90     600.20  -1.013   0.3114    
    ## regionsoutheast   -972.78     605.33  -1.607   0.1084    
    ## regionsouthwest  -1167.14     602.59  -1.937   0.0531 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6241 on 883 degrees of freedom
    ## Multiple R-squared:  0.7459, Adjusted R-squared:  0.7436 
    ## F-statistic:   324 on 8 and 883 DF,  p-value: < 2.2e-16

``` r
predictions1 <- predict(model1,newdata=test_data)

test_data <- data.frame(test_data,Model1Prediction=predictions1)
```
