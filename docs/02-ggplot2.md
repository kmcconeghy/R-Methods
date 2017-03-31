# ggplot2 Examples



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
  [4] survival_2.40-1   lattice_0.20-34   dplyr_0.5.0      
  [7] purrr_0.2.2       readr_1.1.0       tidyr_0.6.1      
 [10] tibble_1.2        ggplot2_2.2.1     tidyverse_1.1.1  
 </pre>
<!--/html_preserve-->  

"Scotty" is my own package. "tidyverse" is Wickam et al. general suite of packages/commands to work with R. "Hmisc" is Frank Harrel's miscellaneous commands, many of which are quite useful.
## Acknowledgements  
This chapter is heavily adapted from several StackExchange and other blog posts.
See:  


@R-base 
