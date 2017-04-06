# Creating Longitudinal DataSets From Individual Records



### Packages to use  
<!--html_preserve--><pre>
 R version 3.3.2 (2016-10-31)
 Platform: x86_64-w64-mingw32/x64 (64-bit)
 Running under: Windows 10 x64 (build 14393)
 
 attached base packages:
 [1] stats     graphics  grDevices utils     datasets  base     
 
 other attached packages:
  [1] knitr_1.15.17     Scotty_0.0.0.9000 devtools_1.12.0  
  [4] Hmisc_4.0-2       Formula_1.2-1     survival_2.41-2  
  [7] lattice_0.20-35   dplyr_0.5.0       purrr_0.2.2      
 [10] readr_1.1.0       tidyr_0.6.1       tibble_1.2       
 [13] ggplot2_2.2.1     tidyverse_1.1.1  
 </pre>
<!--/html_preserve-->  

## Introduction  

I often come across the following issue in my work:  

  I have a dataset where each row is a nursing home assessment, admission record or some other per person observation. However, I need to reshape and summarize these individual records into counts in a panel dataset. In this new dataset structure I want each row to be a unique time- group- cross-section of the data. Some extensions of this include computing incidence (no. events per 1000 persons) and incidence density (no. events per 1000 person-years) measures. I will go through some examples and show how these datasets and measures can be constructed from person or observation unit-level data, assuming you had cohort entry-dates (i.e. admission), event dates (the date of some thing you wish to quantify) and stop-dates (i.e. discharge). 

## Construct dataset



## Show Data
