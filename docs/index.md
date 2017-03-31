--- 
title: "R-methods"
author: "Kevin W. McConeghy"
date: "2017-03-30"
site: bookdown::bookdown_site
documentclass: book
bibliography: [book.bib]
biblio-style: apalike
link-citations: yes
github-repo: /kmcconeghy/R-Methods
url: 'http\://github.com/kmcconeghy/R-Methods'
description: "A collection of working papers covering various statistical, analytical or causal inference problems."
---

# Preface {-}

## Setup for Rmd documents

```r
  require(tidyverse)
```

```r
  require(Hmisc)
```

```r
  require(Scotty)
```

```r
  knitrSet(lang='markdown', h=4.5)
  mu <- markupSpecs$html   # markupSpecs is in Hmisc
  cap  <- mu$cap           # function to output html caption
  lcap <- mu$lcap          # for continuation for long caption
  # These last 2 functions are used by the putHfig function in Hmisc
```
