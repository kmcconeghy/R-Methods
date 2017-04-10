# Cluster Robust Standard Errors  



## Introduction  

In this chapter we are evaluating R's capability to compute different kinds of standard errors. Like with many things, R has extensive flexibility here but can be daunting when you want a quick option. To bring this flexibility down to earth, I lay out the background, provide practical recommendations, user-written commands and benchmark to STATA.  

### Packages to use  
<!--html_preserve--><pre>
 R version 3.3.2 (2016-10-31)
 Platform: x86_64-w64-mingw32/x64 (64-bit)
 Running under: Windows 10 x64 (build 14393)
 
 attached base packages:
 [1] stats     graphics  grDevices utils     datasets  base     
 
 other attached packages:
  [1] Scotty_0.0.0.9000 Hmisc_4.0-2       Formula_1.2-1    
  [4] survival_2.41-3   lattice_0.20-34   dplyr_0.5.0      
  [7] purrr_0.2.2       readr_1.1.0       tidyr_0.6.1      
 [10] tibble_1.3.0      ggplot2_2.2.1     tidyverse_1.1.1  
 </pre>
<!--/html_preserve-->  

"Scotty" is my own package. "tidyverse" is Wickam et al. general suite of packages/commands to work with R. "Hmisc" is Frank Harrel's miscellaneous commands, many of which are quite useful.

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
  ##############################################
## Testing coefficients in time-series data ##
##############################################

## Load investment equation data:
data(Investment)

# Fit regression model:
fm.inv <- lm(RealInv ~ RealGNP + RealInt, data = Investment)

## Test coefficients using Newey-West HAC estimator with
## user-defined and data-driven bandwidth and with Parzen kernel:
coeftest(fm.inv, df = Inf, vcov = NeweyWest(fm.inv, lag = 4, prewhite = FALSE))
coeftest(fm.inv, df = Inf, vcov = NeweyWest)

parzenHAC <- function(x, ...) kernHAC(x, kernel = "Parzen", prewhite = 2,
  adjust = FALSE, bw = bwNeweyWest, ...)
coeftest(fm.inv, df = Inf, vcov = parzenHAC)

## Time-series visualization:
plot(Investment[, "RealInv"], type = "b", pch = 19, ylab = "Real investment")
lines(ts(fitted(fm.inv), start = 1964), col = 4)

## 3-dimensional visualization:
library(scatterplot3d)
s3d <- scatterplot3d(Investment[,c(5,7,6)],
  type = "b", angle = 65, scale.y = 1, pch = 16)
s3d$plane3d(fm.inv, lty.box = "solid", col = 4)



###########################################################
## Testing and dating structural changes in the presence ##
## of heteroskedasticity and autocorrelation             ##
###########################################################

## Load real interest series:
data(RealInt)

## OLS-based CUSUM test with quadratic spectral kernel HAC estimate:
ocus <- gefp(RealInt ~ 1, fit = lm, vcov = kernHAC)
plot(ocus, aggregate = FALSE)
sctest(ocus)

## supF test with quadratic spectral kernel HAC estimate:
fs <- Fstats(RealInt ~ 1, vcov = kernHAC)
plot(fs)
sctest(fs)

## Breakpoint estimation and confidence intervals
## with quadratic spectral kernel HAC estimate:
bp <- breakpoints(RealInt ~ 1)
confint(bp, vcov = kernHAC)
plot(bp)

## Visualization:
plot(RealInt, ylab = "Real interest rate")
lines(ts(fitted(bp), start = start(RealInt), freq = 4), col = 4)
lines(confint(bp, vcov = kernHAC))
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
