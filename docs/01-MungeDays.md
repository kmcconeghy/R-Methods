# Creating Longitudinal Datasets From Individual Records



## Introduction  

### Packages to use  

<!--html_preserve--><pre>
 R version 3.3.2 (2016-10-31)
 Platform: x86_64-w64-mingw32/x64 (64-bit)
 Running under: Windows 10 x64 (build 14393)
 
 attached base packages:
 [1] methods   stats     graphics  grDevices utils     datasets  base     
 
 other attached packages:
  [1] gridExtra_2.2.1   lubridate_1.6.0   knitr_1.15.17    
  [4] Scotty_0.0.0.9000 devtools_1.12.0   Hmisc_4.0-2      
  [7] Formula_1.2-1     survival_2.41-2   lattice_0.20-35  
 [10] dplyr_0.5.0       purrr_0.2.2       readr_1.1.0      
 [13] tidyr_0.6.1       tibble_1.2        ggplot2_2.2.1    
 [16] tidyverse_1.1.1  
 </pre>
<!--/html_preserve-->  

I often come across the following issue in my work:  

  Sometimes you are working with a dataset where each row is a nursing home assessment, admission record or some other per person observation. However, perhaps you are more interesting in analyzing group-level changes over time. In order to do this, you need to reshape and summarize these individual records into counts in a "panel" dataset. In this new dataset structure I want each row to be a unique time- group- summary of the data. Some extensions of this include computing incidence (no. events per 1000 persons) and incidence density (no. events per 1000 person-years) measures. I will go through some examples and show how these datasets and measures can be constructed from person or observation unit-level data, assuming you had cohort entry-dates (i.e. admission), event dates (the date of some thing you wish to quantify) and stop-dates (i.e. discharge).  

## Construct dataset  

First I will construct a test dataset to use in this chapter and subsequent ones.  

In this hypothetical I take a group of admissions that starts counting on 2000/01/01. Each entry will have a random exit up to 1000 days from entry, but censored at 2009/12/31 (because in my hypothetical example this is my study endpoint). Each entry will then have a random group classification (state), and event (0 or 1).  

The event will be assumed one per admission (E.g. death..). I didn't allow for multiple admissions by person. I then generate the event as drawn from a random bernoulli distribution with probability $p_g$ Where $g$ is a group-specific effect randomly generated from a uniform distribution between 0 and 0.3.       


```r
set.seed(12345) #So you get same result
sampsize = 20000
df <- data.frame(id=1:sampsize,
                 CohortEntry = sample(seq(as.Date('2000/01/01'), as.Date('2009/12/31'), by='day'), replace=T, sampsize))
#CohortIn, CohortOut, Group
df <- df %>%
    mutate(CohortExit = CohortEntry+sample(0:365,sampsize, replace=T)) %>% #CohortExit Date (up to 1000 days from start)
    mutate(State = sample(state.name, sampsize, replace=T)) #Random state for each group
df$CohortExit <- as.Date(sapply(df$CohortExit, function(x) min(x,as.Date('2009/12/31'))), origin=origin) #Censor at 'study end'

#Group Effect
  State <- as.data.frame(state.name)
  State$Effect <- runif(50, min=0, max=0.3) #Random effect by group

#Generate random event by group effect
  getReffect <- function(df, group) {
    p <- df[df$'state.name'==group,"Effect"]
    event = as.integer(rbernoulli(1, p = p))
    return(event)
  }
  
df$Event <- sapply(df$id, function(x) getReffect(State, df$State[x])) #Generate random event


#For more complicated procedures below, assign random event date between cohort start with event=0 to NA
  randomDate <- function(TimeIn, TimeOut, Event) {
    RDate <- sample(TimeIn:TimeOut, 1, replace=T)
    RDate <- ifelse(Event==0,NA,RDate)
    return(RDate)
  }
  
  df$EventDate = sapply(df$id, function(x) randomDate(df$CohortEntry[x], df$CohortExit[x], df$Event[x]))
  df$EventDate = as.Date(df$EventDate, origin=origin)
kable(head(df, n=10), align=c('c'))
```



 id    CohortEntry    CohortExit        State         Event    EventDate  
----  -------------  ------------  ----------------  -------  ------------
 1     2007-03-18     2007-10-10         Ohio           0          NA     
 2     2008-10-04     2009-10-04     Rhode Island       0          NA     
 3     2007-08-11     2008-04-24         Ohio           0          NA     
 4     2008-11-11     2009-10-26        Nevada          0          NA     
 5     2004-07-25     2004-08-29       New York         0          NA     
 6     2001-08-30     2002-05-29        Alaska          1      2002-03-23 
 7     2003-04-02     2004-02-23    North Carolina      0          NA     
 8     2005-02-03     2005-10-02        Maine           0          NA     
 9     2007-04-12     2007-06-09       Nebraska         0          NA     
 10    2009-11-24     2009-12-31        Maine           0          NA     

## Show Data

```r
#Scatter and Fitted Line 
p1 <- ggplot(data=df, aes(x=CohortEntry)) + 
  geom_histogram(aes(y = ..density..), binwidth = 4, fill=I("blue"), alpha=I(0.4)) +
  geom_density(col=2) +
  xlab("Cohort Entry") +
  theme_bw()

p2 <- ggplot(data=df, aes(x=CohortExit)) + 
  geom_histogram(aes(y = ..density..), binwidth = 4, fill=I("blue"), alpha=I(0.4)) +
  geom_density(col=2) +
  xlab("Cohort Exit") +
  theme_bw()
grid.arrange(p1, p2, nrow=1)
```

<img src="01-MungeDays_files/figure-html/EntryDate-1.png" width="768" />
  A simple, even distribution to work with (cohort exit is even except for censored at study end date).  
  
## Reshape Process  

If you simply seek to count the total no. of records by groups this is simple.  

### Simple group counts  


```r
  dfState <- df %>%
    group_by(State) %>% #Tells dplyr to create grouped object, and then execute following at that unit
      summarise(Records = n()) #count individuals
  
  cat("Counts of Records by State")
```

```
## Counts of Records by State
```

```r
  kable(head(dfState, n=10), align=c('c'))
```

    State       Records 
-------------  ---------
   Alabama        396   
   Alaska         393   
   Arizona        393   
  Arkansas        385   
 California       372   
  Colorado        375   
 Connecticut      401   
  Delaware        399   
   Florida        376   
   Georgia        401   

Also, if you wish to see the quantity of some event, this is easy also:  

#### Simple Event Counts  

```r
  dfState <- df %>%
    group_by(State) %>% #Tells dplyr to create grouped object, and then execute following at that unit
      summarise(Events = sum(Event)) #count no of events
  
  cat("Counts of Events by State")
```

```
## Counts of Events by State
```

```r
  kable(head(dfState, n=10), align=c('c'))
```

    State       Events 
-------------  --------
   Alabama        46   
   Alaska         26   
   Arizona        19   
  Arkansas        3    
 California       5    
  Colorado        84   
 Connecticut      61   
  Delaware       120   
   Florida       113   
   Georgia        59   

If you wish to count records by time, this is still pretty easy, but you have to be more specific. For example, if I want to count the number of Cohort entries by year, this is how:

### Cohort entries by year counts

```r
#First make year var
df <- df %>%
  mutate(EntryYear = year(CohortEntry)) #year function from lubridate
  
#Second group by this var
dfGroup <- df %>% 
  group_by(EntryYear) %>%  
  summarise(Records = n(), Events = sum(Event)) #count individuals
  cat("Counts of Records by Cohort Entry Year")
```

```
## Counts of Records by Cohort Entry Year
```

```r
  kable(head(dfGroup, n=10), align=c('c'))
```



 EntryYear    Records    Events 
-----------  ---------  --------
   2000        2015       274   
   2001        1987       273   
   2002        2041       288   
   2003        1933       295   
   2004        2043       269   
   2005        2068       313   
   2006        1964       285   
   2007        2019       308   
   2008        1974       297   
   2009        1956       293   

### Cohort prevalence by time

The next step will get a little trickier. Let's say we aren't interested in how many individuals entered/exited the cohort in a given year as above. Rather we want to identify how many total patients are in the cohort during a specified period of time (e.g. year). This is like "point prevalence", in the sense that we are measuring the number of cohort individuals in a given time interval (point prevalence is usually the no. of diseased / total population). So we need to take the "CohortEntry", "CohortExit" date variables and compute how many individuals were in the cohort in year 1, year 2 etc. What makes this tricky is that individuals don't start and stop at the same time and cross multiple time units (years in this case).  

Here is one method where I compute the "prevalent"" cohort, no. events and event rate:  

```r
  #Create New Dataframe by Time Unit
  TimeMin <- min(year(df$CohortEntry)) #lowest time unit observed
  TimeMax <- max(year(df$CohortExit)) #highest time unit observed
  
  #This following sequence step is good, in case a certain year was skipped (i.e. no admits that year)
  dfTime <- TimeMin:TimeMax %>% #Sequence years
    as_tibble() %>%
    mutate(x2 = NA, x3 = NA)
    names(dfTime) <- c("Year", "Residents", "Events")
    
  
  #Write a time-interval program for Residence (assuming x is year)
  InCohort <-  function(x, TimeIn, TimeOut) {
    #Note that the following line works because of R vectorization
    count <- if_else(x>=year(TimeIn) & x<=year(TimeOut),1,0) #Test if x is TimeIn<=x<=TimeOut
    InCohortN <- sum(count) #Add up total people
    return(InCohortN) #return
  }
  
  #Write a time-interval program for Event
  InEvent <-  function(x, Event, EventDate) {
    #Note that the following line works because of R vectorization
    events <- if_else(Event==1 & x==year(EventDate),1,0) #Added condition of event==1
    InCohortEvents <- sum(events) #Add up total events in that year
    return(InCohortEvents) #return
  }
  dfTime$Residents <- sapply(dfTime$Year, function(x) InCohort(x, df$CohortEntry,df$CohortExit))
  dfTime$Events <- sapply(dfTime$Year, function(x) InEvent(x, df$Event, df$EventDate))
  dfTime$'Event Rate' <- dfTime$Events / dfTime$Residents
  #Calculate events
  #eventnum <- df %?%
  
  cat("No. of individuals in the cohort by year")
```

```
## No. of individuals in the cohort by year
```

```r
  kable(head(dfTime, n=10), align=c('c'), digits=3)
```



 Year    Residents    Events    Event Rate 
------  -----------  --------  ------------
 2000      2015        200        0.099    
 2001      2981        271        0.091    
 2002      3040        284        0.093    
 2003      2951        291        0.099    
 2004      3034        283        0.093    
 2005      3063        299        0.098    
 2006      2956        295        0.100    
 2007      2986        301        0.101    
 2008      2972        294        0.099    
 2009      2945        373        0.127    

So because I didn't specify a time-trend in my sample generation, it makes sense that the event rate is relatively constant over time.  
We can double-check this worked with the following specific code:  


```r
  #Logic
  # IF 2004 is less than or equal to EntryDate (i.e. they entered before or during 2004)
  # AND 2004 is less than or equal to Exit (i.e. they exited after or during 2004)
  Check <- ifelse(2004>=year(df$CohortEntry) & 2004<=year(df$CohortExit),1,0)
  cat("2004 people :", sum(Check))
```

```
## 2004 people : 3034
```

### Incidence Density  


```r
df <- df %>%
    mutate(TimeDiff = as.integer(CohortExit - CohortEntry)) #timeDiff
```
