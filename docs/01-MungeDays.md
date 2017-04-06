# Creating Longitudinal DataSets From Individual Records



### Packages to use  
<!--html_preserve--><pre>
 R version 3.3.2 (2016-10-31)
 Platform: x86_64-w64-mingw32/x64 (64-bit)
 Running under: Windows 10 x64 (build 14393)
 
 attached base packages:
 [1] methods   stats     graphics  grDevices utils     datasets  base     
 
 other attached packages:
  [1] lubridate_1.6.0   knitr_1.15.17     Scotty_0.0.0.9000
  [4] devtools_1.12.0   Hmisc_4.0-2       Formula_1.2-1    
  [7] survival_2.41-2   lattice_0.20-35   dplyr_0.5.0      
 [10] purrr_0.2.2       readr_1.1.0       tidyr_0.6.1      
 [13] tibble_1.2        ggplot2_2.2.1     tidyverse_1.1.1  
 </pre>
<!--/html_preserve-->  

## Introduction  

I often come across the following issue in my work:  

  I am provided with a dataset where each row is a nursing home assessment, admission record or some other per person observation. However, I need to reshape and summarize these individual records into counts in a panel dataset. In this new dataset structure I want each row to be a unique time- group- summary of the data. Some extensions of this include computing incidence (no. events per 1000 persons) and incidence density (no. events per 1000 person-years) measures. I will go through some examples and show how these datasets and measures can be constructed from person or observation unit-level data, assuming you had cohort entry-dates (i.e. admission), event dates (the date of some thing you wish to quantify) and stop-dates (i.e. discharge).  

## Construct dataset  

First I will construct a dataset of admissions that starts counting on 2000/01/01. Each entry will have a random exit up to 1000 days from entry, but censored at 2009/12/31 (because hypothetically you stop collecting info at this point). Each entry will then have a random group classification (state), and event (0 or 1).    

```r
df <- data.frame(id=1:10000,
                 CohortEntry = sample(seq(as.Date('2000/01/01'), as.Date('2009/12/31'), by='day'), replace=T, 10000))
df <- df %>%
    mutate(CohortExit = CohortEntry+sample(0:365,10000, replace=T)) %>% #CohortExit Date (up to 1000 days from start)
    mutate(Group = sample(state.name, 10000, replace=T)) %>% #Random state for each group
    mutate(Event = sample(0:1, 10000, replace=T))

df$CohortExit <- as.Date(sapply(df$CohortExit, function(x) min(x,as.Date('2009/12/31'))), origin=origin) #Censor at 'study end'
```

## Show Data

```r
#Scatter and Fitted Line 
p <- ggplot(data=df, aes(x=CohortEntry)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5, fill=I("blue"), alpha=I(0.4)) +
  geom_density(col=2) +
  xlab("Cohort Entry") +
  theme_bw()
suppressMessages(print(p))
```

<img src="01-MungeDays_files/figure-html/Entry Date-1.png" width="672" />

```r
p <- ggplot(data=df, aes(x=CohortExit)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5, fill=I("blue"), alpha=I(0.4)) +
  geom_density(col=2) +
  xlab("Cohort Exit") +
  theme_bw()
suppressMessages(print(p))
```

<img src="01-MungeDays_files/figure-html/Entry Date-2.png" width="672" />

  A simple, even distribution to work with (cohort exit is even except censored at study end date).  
  
## Reshape Process

### Calculate time distance  

```r
df <- df %>%
    mutate(TimeDiff = as.integer(CohortExit - CohortEntry)) #timeDiff
```
