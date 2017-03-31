# Heteroskedastic & Cluster Robust Standard Errors  


```
## Warning: package 'tidyverse' was built under R version 3.3.3
```

```
## Loading tidyverse: ggplot2
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
## Loading tidyverse: dplyr
```

```
## Warning: package 'readr' was built under R version 3.3.3
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## filter(): dplyr, stats
## lag():    dplyr, stats
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     combine, src, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```
## Warning: package 'lmtest' was built under R version 3.3.3
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```
## 
## Attaching package: 'boot'
```

```
## The following object is masked from 'package:survival':
## 
##     aml
```

```
## The following object is masked from 'package:lattice':
## 
##     melanoma
```

## Introduction 

In this chapter we are evaluating R's capability to compute standard errors. Overall like most things, R has substantial capacity but its flexibility can be daunting. To bring this flexibility down to earth, I lay out the background, provide practical recommendations, user-written commands and benchmark to STATA. 

### Packages to use
<!--html_preserve--><pre>
 R version 3.3.2 (2016-10-31)
 Platform: x86_64-w64-mingw32/x64 (64-bit)
 Running under: Windows 10 x64 (build 14393)
 
 attached base packages:
 [1] stats     graphics  grDevices utils     datasets  base     
 
 other attached packages:
  [1] boot_1.3-18       lmtest_0.9-35     zoo_1.7-14       
  [4] sandwich_2.3-4    Scotty_0.0.0.9000 Hmisc_4.0-2      
  [7] Formula_1.2-1     survival_2.40-1   lattice_0.20-34  
 [10] dplyr_0.5.0       purrr_0.2.2       readr_1.1.0      
 [13] tidyr_0.6.1       tibble_1.2        ggplot2_2.2.1    
 [16] tidyverse_1.1.1  
 </pre>
<!--/html_preserve-->

"Scotty" is my own package. "tidyverse" is Wickam et al. general suite of packages/commands to work with R. "Hmisc" is Frank Harrel's miscellaneous commands, many of which are quite useful. 

"sandwich", "lmtest" and "boot" are specifically relevant to this chapter in order to compute various standard errors (SE). 

### Test Data

The cluster data was obtained from:
http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/test_data.txt


### Other References
## R's calculation of standard errors

## Heteroskedascity
@angrist2008mostly

## Clustering
@Bertrand04howmuch

## Stata comparison

A full discussion of STATA programming can be seen here: http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/se_programming.htm
STATA blog:http://www.stata.com/support/faqs/statistics/standard-errors-and-vce-cluster-option/

Briefly: In Stata one can specify a variance-covariance matrix that is heteroskedastic consistent with the *vce(robust)* option in regression models.  


e.g. STATA example
```{}
regress y x z, vce(robust)
```

A Huber-White variance-covariance matrix can also be computed by some group with the **vce(cluster *group*)** option in regression models.

e.g. STATA example
```{}
regress y x z, vce(cluster *group*)
```

See: http://www.stata.com/support/faqs/statistics/standard-errors-and-vce-cluster-option/
## Heteroskedastic consistent errors in R  

## Cluster robust errors in R  

## Block bootstrapping  

An alternative to computing special variance-covariance matrices is non-parametric  "block" bootstrapping. To do this, you perform a bootstrapping procedure where you sample the group or "block" instead of unit observation. This has been shown to be about as consistent and unbiased as the above sandwich estimators, and may be advantgeous when the number of clusters is small.(Insert Bertrand citation).  

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

## Acknowledgements  
This chapter is heavily adapted from several StackExchange and other blog posts.
See:
http://www.richard-bluhm.com/clustered-ses-in-r-and-stata-2/
http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/standarderror_extra_tables.pdf  
https://sites.google.com/site/waynelinchang/r-code  
http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/test_data.htm  
https://thetarzan.wordpress.com/2011/05/28/heteroskedasticity-robust-and-clustered-standard-errors-in-r/  

## Bibliography
@R-base

