# Heteroskedastic & Cluster Robust Standard Errors  



## Introduction  

In this chapter we are evaluating R's capability to compute different kinds of standard errors. Like with many things, R has extensive flexibility here but can be daunting when you want a quick option. To bring this down to earth, I lay out the background, provide practical recommendations, user-written commands and benchmark to STATA.  

### Packages to use  
<!--html_preserve--><pre>
 R version 3.3.2 (2016-10-31)
 Platform: x86_64-w64-mingw32/x64 (64-bit)
 Running under: Windows 10 x64 (build 14393)
 
 attached base packages:
 [1] stats     graphics  grDevices utils     datasets  base     
 
 other attached packages:
  [1] knitr_1.15.17     boot_1.3-18       lmtest_0.9-35    
  [4] zoo_1.7-14        sandwich_2.3-4    Scotty_0.0.0.9000
  [7] Hmisc_4.0-2       Formula_1.2-1     survival_2.40-1  
 [10] lattice_0.20-34   dplyr_0.5.0       purrr_0.2.2      
 [13] readr_1.1.0       tidyr_0.6.1       tibble_1.2       
 [16] ggplot2_2.2.1     tidyverse_1.1.1  
 </pre>
<!--/html_preserve-->  

"Scotty" is my own package. "tidyverse" is Wickam et al. general suite of packages/commands to work with R. "Hmisc" is Frank Harrel's miscellaneous commands, many of which are quite useful. "sandwich", "lmtest" and "boot" are specifically relevant to this chapter in order to compute various standard errors (SE).  

## Heteroskedascity  
*Heteroskedascity* in this context refers to a random variable where a given subset of a sample will have different variability compared with others. Variability being variance or some other measure of dispersion. In constrast *homoskedascity* is when variance is constant across these subpopulations (Figure 1). 


```r
#Generate Data  
  x <- runif(500)
  yHomo <- 2*x + rnorm(500)
  yHetero <- 2*x + x*rnorm(500)
  df <- as.data.frame(cbind(x, yHomo, yHetero))

#Scatter and Fitted Line 
ggplot(data=df, aes(x=x, y=yHomo)) + 
  geom_point() +
  geom_smooth(method='lm', se=F) + 
  xlab("X variable") +
  theme_bw()
```

<img src="01-vcovHC_files/figure-html/testdataHomo-1.png" width="672" />
  
**Figure 1.** Example of homoskedascity. Note how data points appear to be randomly scattered around line of best fit, and that the dispersion *appears* of the points constant across the range of X variable.


```r
#Scatter and Fitted Line 
ggplot(data=df, aes(x=x, y=yHetero)) + 
  geom_point() +
  geom_smooth(method='lm', se=F) + 
  xlab("X variable") +
  theme_bw()
```

<img src="01-vcovHC_files/figure-html/testdataHetero-1.png" width="672" />
  
**Figure 2.** Example of heteroskedascity. See how the dispersion of the points appears greater as X increases.  

## Test data  

The cluster data was obtained from:  
http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/test_data.txt  


```r
url <- "http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/test_data.txt"
df <- as_tibble(read.table(url))
names(df) <- c("group", "year", "x", "y")
head(df)
```

```
## # A tibble: 6 × 4
##   group  year          x          y
##   <int> <int>      <dbl>      <dbl>
## 1     1     1 -1.1139730  2.2515350
## 2     1     2 -0.0808538  1.2423460
## 3     1     3 -0.2376072 -1.4263760
## 4     1     4 -0.1524857 -1.1093940
## 5     1     5 -0.0014262  0.9146864
## 6     1     6 -1.2127370 -1.4246860
```
  This data represents financial information by year on a group of firms. I use this as a benchmark because several other online posts/bloggers compare this data using different specifications and software.@peterson2009
  
The expected results I will recreate are given here:
http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/test_data.htm


```r
se_results <- as_tibble(matrix(nrow=8,ncol=6))
names(se_results) <- c("method","v-cov", "int","x","peterson_int","peterson_x")
method <- c("lm","manual","manual","HC0","HC1","HC2","HC3","HC4")
type <- c("cons","cons","whitedfc","white","whitedfc","","","")
peterson_int=c(0.0284,NA,0.0284,NA,NA,NA,NA,0.0670)
peterson_x=  c(0.0286,NA,0.0284,NA,NA,NA,NA,0.0506)

for (i in 1:nrow(se_results)) {
  se_results[i,1] <- method[i]
  se_results[i,2] <- type[i]
  se_results[i,5] <- peterson_int[i]
  se_results[i,6] <- peterson_x[i]
}
```

## Regression parameter standard errors under iid  


```r
m1 <- lm(y ~ x, data = df)
se_results[1,3] <- coeftest(m1)[1,2]
se_results[1,4] <- coeftest(m1)[2,2]
se_results
```

```
## # A tibble: 8 × 6
##   method  `v-cov`        int          x peterson_int peterson_x
##    <chr>    <chr>      <dbl>      <dbl>        <dbl>      <dbl>
## 1     lm     cons 0.02835932 0.02858329       0.0284     0.0286
## 2 manual     cons         NA         NA           NA         NA
## 3 manual whitedfc         NA         NA       0.0284     0.0284
## 4    HC0    white         NA         NA           NA         NA
## 5    HC1 whitedfc         NA         NA           NA         NA
## 6    HC2                  NA         NA           NA         NA
## 7    HC3                  NA         NA           NA         NA
## 8    HC4                  NA         NA       0.0670     0.0506
```

You can compare these results with the first table "OLS Coefficients and Standard Errors" in the Peterson link above. R computes the regression coefficients with the standard $(\textbf{X}'\textbf{X})^{-1}\textbf{X}'\textbf{y}$ i.e. the coefficient is a function of X and y.

In a regression framework you compute standard errors by taking the square root of the diagonal elements of the variance-covariance matrix. 

Equation 1. Covariance matrix of the error term $u$  
$E[{uu}'|\textbf{X}] = \mathbf{\Sigma_{u}}$

Equation 2.  
$\mathbf{\Sigma_{u}} = \sigma^2 I_{N}$

Equation 3. Expectation of the variance of $\beta$ conditional on X.  
$\textrm{Var}[\hat{\mathbf{\beta}}|\textbf{X}] = (\textbf{X}'\textbf{X})^{-1}(\textbf{X}' \mathbf{\Sigma_{u}} \textbf{X}) (\textbf{X}'\textbf{X})^{-1}$

Under the assumption of independent and identically distributed errors (homoskedascity), Eq. 3 is simplified to eq. 4 (transpose matrix, using diagonal elements).  

Equation 4. iid assumed
$\textrm{Var}[\hat{\mathbf{\beta}}|\textbf{X}] = \sigma_{u}^{2}(\textbf{X}'\textbf{X})^{-1}$

Assuming $\sigma_u^2$ is fixed but unknown, a given random sample's variance, $s^2$, can be estimated:

Equation 5. Standard Error

$s^2 = \frac{\sum_{i=1}^n e_i^2}{n-k}$

Where $e$ are the squared residuals, $n$ is the sample size, and $k$ are the number of regressors. 

With this information the standard errors above can be replicated manually like so:

```r
X <- model.matrix(m1) # get X matrix/predictors
n <- dim(X)[1] # number of obs
k <- dim(X)[2] # n of predictors

# calculate stan errs as eq in the above
# sq root of diag elements in vcov
se <- sqrt(diag(solve(crossprod(X)) * as.numeric(crossprod(resid(m1))/(n-k))))
se_results[2,3] <- se[1]
se_results[2,4] <- se[2]
se_results
```

```
## # A tibble: 8 × 6
##   method  `v-cov`        int          x peterson_int peterson_x
##    <chr>    <chr>      <dbl>      <dbl>        <dbl>      <dbl>
## 1     lm     cons 0.02835932 0.02858329       0.0284     0.0286
## 2 manual     cons 0.02835932 0.02858329           NA         NA
## 3 manual whitedfc         NA         NA       0.0284     0.0284
## 4    HC0    white         NA         NA           NA         NA
## 5    HC1 whitedfc         NA         NA           NA         NA
## 6    HC2                  NA         NA           NA         NA
## 7    HC3                  NA         NA           NA         NA
## 8    HC4                  NA         NA       0.0670     0.0506
```

## "White" heteroskedastic consistent errors 

  In the setting of heteroskedascity, the parameters are consistent but inefficient and also the variance-covariance matrix is inconsistent (i.e. biased).(@white1980) The assumption of the residuals $u$ being *identically* distributed does not hold, and the diagonal matrix is invalid. However, an alternative variance-covariance matrix can be computed which is heteroskedastic consistent.(@white1980)  

  With the "robust" approach proposed by White et al., you assume  the variance of the residual is estimated as a diagonal matrix of each squared residual (vs. average above with $s^2$). Each j-th row-column element is $\hat{u}_{j}^{2}$ in the diagonal terms of ${\Sigma_{u}}$. 

The full equation is:  

### Manual estimator  

```r
u <- matrix(resid(m1)) # residual vector
meat1 <- t(X) %*% diag(diag(crossprod(t(u)))) %*% X # Sigma is a diagonal with u^2 as elements
dfc <- n/(n-k) # degrees of freedom adjust  
se <- sqrt(dfc*diag(solve(crossprod(X)) %*% meat1 %*% solve(crossprod(X))))
se_results[3,3] <- se[1]
se_results[3,4] <- se[2]
se_results
```

```
## # A tibble: 8 × 6
##   method  `v-cov`        int          x peterson_int peterson_x
##    <chr>    <chr>      <dbl>      <dbl>        <dbl>      <dbl>
## 1     lm     cons 0.02835932 0.02858329       0.0284     0.0286
## 2 manual     cons 0.02835932 0.02858329           NA         NA
## 3 manual whitedfc 0.02836067 0.02839516       0.0284     0.0284
## 4    HC0    white         NA         NA           NA         NA
## 5    HC1 whitedfc         NA         NA           NA         NA
## 6    HC2                  NA         NA           NA         NA
## 7    HC3                  NA         NA           NA         NA
## 8    HC4                  NA         NA       0.0670     0.0506
```

  You will find these "White" or robust standard errors are consistent with the second Peterson table.[@peterson2009]  They are also consistent with STATA's *robust* option. It is not technically the same as the White paper because STATA does a degree of freedom adjustment for small sample size.  
  
### R standard function   

Using the already written commands you can specify "White" standard errors with the vcovHC function in the sandwich package.[@Zeileis2006] You can report correct standard errors like below with vcovHC option in function coeftest.  

vcovHC has several types available. The general formula for the var-cov matrix is: $(X'X)^{-1} X' Omega X (X'X)^{-1}$.  

The specification of $Omega$ is determined by the `type=` option.  

`type="cons"` $\omega_i = \sigma^2$ Constant variance  
`type=HC0`    $\omega_i = \mu^2_i$ the White variance-covariance matrix   
`type=HC1`    $\omega_i = \frac{n}{n-k}\mu^2_i$ Small sample correction (STATA).  
`type=HC2`    $\omega_i = \frac{\mu^2_i}{1-h_i}$  
`type=HC3`    $\omega_i = \frac{\mu^2_i}{(1-h_i)^{2}}$  
`type=HC4`    $\omega_i = \frac{\mu^2_i}{(1-h_i)^{\delta_i}}$  

Where $h_i = H_{ii}$ are the diagonal elements of the hat matrix and $\delta_i = min({4 }, {h_i}{h¯})$. The documentation for the sandwich package recommends HC4 based on recent literature.[@Cribari2004]  


```
## Different variance-covariance options with vcovHC
```

```
## type = cons
```

```
## 
## t test of coefficients:
## 
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 0.029680   0.028359  1.0466   0.2954    
## x           1.034833   0.028583 36.2041   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## type = HC0
```

```
## 
## t test of coefficients:
## 
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 0.029680   0.028355  1.0467   0.2953    
## x           1.034833   0.028389 36.4513   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## type = HC1
```

```
## 
## t test of coefficients:
## 
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 0.029680   0.028361  1.0465   0.2954    
## x           1.034833   0.028395 36.4440   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## type = HC2,
```

```
## 
## t test of coefficients:
## 
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 0.029680   0.028361  1.0465   0.2954    
## x           1.034833   0.028401 36.4368   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## type = HC3,
```

```
## 
## t test of coefficients:
## 
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 0.029680   0.028366  1.0463   0.2955    
## x           1.034833   0.028412 36.4223   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## type = HC4,
```

```
## 
## t test of coefficients:
## 
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 0.029680   0.028363  1.0464   0.2954    
## x           1.034833   0.028418 36.4150   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Lifehack: Rather than use the `coeftest` function you can also directly modify the standard errors in the regression summary object.  


```r
s <- summary(m1)
s$coefficients[, 2] <- sqrt(diag(vcovHC(m1, type="HC1")))
s
```

```
## 
## Call:
## lm(formula = y ~ x, data = df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.7611 -1.3680 -0.0166  1.3387  8.6779 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.02968    0.02836   1.047    0.295    
## x            1.03483    0.02840  36.204   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.005 on 4998 degrees of freedom
## Multiple R-squared:  0.2078,	Adjusted R-squared:  0.2076 
## F-statistic:  1311 on 1 and 4998 DF,  p-value: < 2.2e-16
```

## Clustering  

## Cluster robust errors in R  

## Block bootstrapping  

  An alternative to computing a special variance-covariance matrix is using a non-parametric "brute-force" method termed block bootstrapping. To do this, you the sample the dataset with replacement by group or "block" instead of individual observation. The parameters are estimated for each sample instance and stored in a new table. Then, you can either compute the parameter moments (mean, variance etc.) using the stored coefficients or if a 95% parameter interval is the ultimate goal one can simply report the ordered percentiles (e.g., 2.5% - 97.5%). Other methods for computing the intervals exist, such as bias-corrected. Whichever you pick, bootstraps are about as unbiased as the above sandwich estimators, and may be advantageous when the number of clusters is small.
  
## Permutation or "Randomization" Test

(@Bertrand04howmuch) 

### Bootstrap Program  


```r
Boot.ATE <- function (model, treat, R = 250, block = "", df) 
{
  require(boot)
  require(dplyr)
  family <- model$family
  if (block == "") {
    boot.mod <- function(x, i, model, treat) {
      samp.df <- x[i, ]
      samp.glm <- try(glm(model, data = samp.df, family = family))
      if (inherits(samp.glm, "try-error")) {
        coef <- NA
        ate <- NA
        rr <- NA
        c(coef, ate, rr)
      }
      else {
        df2 <- samp.df
        df2[, paste(treat)] = 1
        pred1. <- predict.glm(samp.glm, newdata = df2, 
          type = "response")
        df2[, paste(treat)] = 0
        pred0. <- predict.glm(samp.glm, newdata = df2, 
          type = "response")
        coef <- samp.glm$coefficients[paste0(treat)]
        ate <- mean(pred1.) - mean(pred0.)
        rr <- mean(pred1.)/mean(pred0.)
        c(coef, ate, rr)
      }
    }
    boot.m <- boot(data = df, statistic = boot.mod, R = R, 
      model = model, treat = treat)
  }
  else {
    Groups = unique(df[, paste(block)])
    boot.mod <- function(x, i, model, treat, df, block, 
      iter = 0) {
      block.df <- data.frame(group = x[i])
      names(block.df) = block
      samp.df <- left_join(block.df, df, by = block)
      samp.glm <- try(glm(model, data = samp.df, family = family))
      if (inherits(samp.glm, "try-error")) {
        coef <- NA
        ate <- NA
        rr <- NA
        c(coef, ate, rr)
      }
      else {
        df2 <- samp.df
        df2[, paste(treat)] = 1
        pred1. <- predict.glm(samp.glm, newdata = df2, 
          type = "response")
        df2[, paste(treat)] = 0
        pred0. <- predict.glm(samp.glm, newdata = df2, 
          type = "response")
        coef <- samp.glm$coefficients[paste0(treat)]
        ate <- mean(pred1.) - mean(pred0.)
        rr <- mean(pred1.)/mean(pred0.)
        c(coef, ate, rr)
      }
    }
    boot.m <- boot(data = Groups, statistic = boot.mod, 
      R = R, model = model, treat = treat, df = df, block = block)
  }
  m1.confint <- c(model$coefficients[paste0(treat)], confint(model, 
    treat, level = 0.95))
  coeff = boot.ci(boot.m, index = 1, type = "perc")
  coeff = c(median(boot.m$t[, 1]), coeff$percent[, 4], coeff$percent[, 
    5])
  names(coeff) <- c("Coeff.", "2.5%", "97.5%")
  ate = boot.ci(boot.m, index = 2, type = "perc")
  ate = c(median(boot.m$t[, 2]), ate$percent[, 4], ate$percent[, 
    5])
  names(ate) <- c("ATE", "2.5%", "97.5%")
  rr = boot.ci(boot.m, index = 3, type = "perc")
  rr = c(median(boot.m$t[, 3]), rr$percent[, 4], rr$percent[, 
    5])
  names(rr) <- c("Rr", "2.5%", "97.5%")
  boot.iter = boot.m$t
  res = list(level = 0.95, model_ci = m1.confint, coeff = coeff, 
    ate = ate, rr = rr, boots = boot.iter)
  return(res)
}
```

## Stata comparison  
A full discussion of STATA programming can be seen here:   http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/se_programming.htm  
STATA blog:  
http://www.stata.com/support/faqs/statistics/standard-errors-and-vce-cluster-option/  

Briefly: In Stata one can specify a variance-covariance matrix that is heteroskedastic consistent with the *vce(robust)* option in regression models.  


e.g. robust option in STATA    
```{}
regress y x, vce(robust)
```

A Huber-White variance-covariance matrix can also be computed by some group with the **vce(cluster *group*)** option in regression models.  

e.g. cluster option in STATA  
```{}
regress y x, vce(cluster group)
```

See:  
http://www.stata.com/support/faqs/statistics/standard-errors-and-vce-cluster-option/  

## Acknowledgements  
This chapter is heavily adapted from several StackExchange and other blog posts.
See:  
http://www.richard-bluhm.com/clustered-ses-in-r-and-stata-2/  
https://sites.google.com/site/waynelinchang/r-code  
https://thetarzan.wordpress.com/2011/05/28/heteroskedasticity-robust-and-clustered-standard-errors-in-r/  

## Bibliography  
@R-base  
@angrist2008mostly  
