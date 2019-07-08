
# Franklin Prima Portfolio at the turn

In our notebook on [mutal fund drawdowns](https://github.com/stockviz/plutons/blob/master/mutual-fund-drawdown.R.ipynb), we saw how Franklin India Prima Fund drew down the least compared to other large midcap mutual funds. How did the portfolio change through time?


```R
library(tidyverse)
library(ggthemes)
library(odbc)
library(plutoR)
library(quantmod)
library(PerformanceAnalytics)

options("scipen"=999)
options(stringsAsFactors = FALSE)
options(repr.plot.width=16, repr.plot.height=8)

source("config.R")
source("goofy/plot.common.R")
source("goofy/misc.common.R")

indices <- Indices()
mfi <- MutualFundsIndia()
```

    ── [1mAttaching packages[22m ─────────────────────────────────────── tidyverse 1.2.1 ──
    [32m✔[39m [34mggplot2[39m 3.2.0     [32m✔[39m [34mpurrr  [39m 0.3.2
    [32m✔[39m [34mtibble [39m 2.1.3     [32m✔[39m [34mdplyr  [39m 0.8.1
    [32m✔[39m [34mtidyr  [39m 0.8.3     [32m✔[39m [34mstringr[39m 1.4.0
    [32m✔[39m [34mreadr  [39m 1.3.1     [32m✔[39m [34mforcats[39m 0.4.0
    ── [1mConflicts[22m ────────────────────────────────────────── tidyverse_conflicts() ──
    [31m✖[39m [34mdplyr[39m::[32mfilter()[39m masks [34mstats[39m::filter()
    [31m✖[39m [34mdplyr[39m::[32mlag()[39m    masks [34mstats[39m::lag()
    Loading required package: xts
    Loading required package: zoo
    
    Attaching package: ‘zoo’
    
    The following objects are masked from ‘package:base’:
    
        as.Date, as.Date.numeric
    
    
    Attaching package: ‘xts’
    
    The following objects are masked from ‘package:dplyr’:
    
        first, last
    
    Loading required package: TTR
    Version 0.4-0 included new data defaults. See ?getSymbols.
    
    Attaching package: ‘PerformanceAnalytics’
    
    The following object is masked from ‘package:graphics’:
    
        legend
    
    Registering fonts with R
    
    Attaching package: ‘gridExtra’
    
    The following object is masked from ‘package:dplyr’:
    
        combine
    
    
    Attaching package: ‘lubridate’
    
    The following object is masked from ‘package:base’:
    
        date
    


### The big picture

How did the fund perform over the Midcap 150 TR index over a 5 year period?


```R
indexName <- "NIFTY MIDCAP 150 TR"
schemeCode <- 100473
schemeName <- "Franklin India Prima"
startDate <- as.Date("2014-01-01")
endDate <- as.Date("2019-06-30")
```


```R
#get the daily returns of the fund
navs <- data.frame(mfi$NavTimeSeries() %>%
        filter(SCHEME_CODE == schemeCode & TIME_STAMP >= startDate & TIME_STAMP <= endDate) %>%
        select(TIME_STAMP, NAV))
    
navXts <- dailyReturn(xts(navs$NAV, navs$TIME_STAMP))

#grab the benchmark
bm <- data.frame(indices$NseTimeSeries() %>%
    filter(NAME == indexName & TIME_STAMP >= startDate & TIME_STAMP <= endDate) %>%
    select(TIME_STAMP, CLOSE))

navXts <- merge.xts(navXts, dailyReturn(xts(bm$CLOSE, bm$TIME_STAMP)))
names(navXts) <- c("FUND", indexName)
```


```R
# plot the cumulative returns
Common.PlotCumReturns(navXts, sprintf("%s vs. %s", schemeName, indexName), "")
```


![png](Franklin-India-Prima.R_files/Franklin-India-Prima.R_5_0.png)


### How was the fund positioned going into 2018 vs. where it was in March 2019?


```R
date1 <- as.Date("2017-12-31")
date2 <- as.Date("2019-03-31")

sectorAlloc1 <- mfi$Portfolio() %>%
    filter(SCHEME_CODE == schemeCode 
           & INSTRUMENT == 'EQUITY' 
           & !is.null(WEIGHTAGE)
           & WEIGHTAGE > 0
           & PORTFOLIO_DATE == date1
           & (INSTRUMENT_TYPE == 'STOCK' 
              | INSTRUMENT_TYPE == 'E' 
              | INSTRUMENT_TYPE == '' 
              | is.null(INSTRUMENT_TYPE))) %>%
    group_by(INDUSTRY_BSE) %>%
    summarize(ALLOC = sum(WEIGHTAGE)) %>%
    arrange(desc(ALLOC)) %>%
    collect()

sectorAlloc1Df <- sectorAlloc1 %>%
    mutate(INDUSTRY_BSE = replace(INDUSTRY_BSE, INDUSTRY_BSE == '-' 
                                  | is.na(INDUSTRY_BSE), 'UNK')) %>%
    as.data.frame()

sectorAlloc2 <- mfi$Portfolio() %>%
    filter(SCHEME_CODE == schemeCode 
           & INSTRUMENT == 'EQUITY' 
           & !is.null(WEIGHTAGE)
           & WEIGHTAGE > 0
           & PORTFOLIO_DATE == date2
           & (INSTRUMENT_TYPE == 'STOCK' 
              | INSTRUMENT_TYPE == 'E' 
              | INSTRUMENT_TYPE == '' 
              | is.null(INSTRUMENT_TYPE))) %>%
    group_by(INDUSTRY_BSE) %>%
    summarize(ALLOC = sum(WEIGHTAGE)) %>%
    arrange(desc(ALLOC)) %>%
    collect()

sectorAlloc2Df <- sectorAlloc2 %>%
    mutate(INDUSTRY_BSE = replace(INDUSTRY_BSE, INDUSTRY_BSE == '-' 
                                  | is.na(INDUSTRY_BSE), 'UNK')) %>%
    as.data.frame()

```

    Warning message:
    “Missing values are always removed in SQL.
    Use `SUM(x, na.rm = TRUE)` to silence this warning
    This warning is displayed only once per session.”


```R
pie(sectorAlloc1Df$ALLOC, 
    labels=sprintf("%s (%.2f%%)", sectorAlloc1Df$INDUSTRY, sectorAlloc1Df$ALLOC), 
    col=economist_pal()(9), 
    main=sprintf("%s Sector Allocations (%s)", schemeName, date1), 
    family='Segoe UI', 
    init.angle=45)
mtext("@StockViz", side=1)
```


![png](Franklin-India-Prima.R_files/Franklin-India-Prima.R_8_0.png)



```R
pie(sectorAlloc2Df$ALLOC, 
    labels=sprintf("%s (%.2f%%)", sectorAlloc2Df$INDUSTRY, sectorAlloc2Df$ALLOC), 
    col=economist_pal()(9), 
    main=sprintf("%s Sector Allocations (%s)", schemeName, date2), 
    family='Segoe UI', 
    init.angle=45)
mtext("@StockViz", side=1)
```


![png](Franklin-India-Prima.R_files/Franklin-India-Prima.R_9_0.png)



```R
# zoom into allocations that are more than 2%

sectorAlloc1Df2 <- sectorAlloc1Df[sectorAlloc1Df$ALLOC > 2, ]
sectorAlloc2Df2 <- sectorAlloc2Df[sectorAlloc2Df$ALLOC > 2, ]
```


```R
pie(sectorAlloc1Df2$ALLOC, 
    labels=sprintf("%s (%.2f%%)", sectorAlloc1Df2$INDUSTRY, sectorAlloc1Df2$ALLOC), 
    col=economist_pal()(9), 
    main=sprintf("%s Sector Allocations (%s) > 2%%", schemeName, date1), 
    family='Segoe UI', 
    init.angle=45)
mtext("@StockViz", side=1)
```


![png](Franklin-India-Prima.R_files/Franklin-India-Prima.R_11_0.png)



```R
pie(sectorAlloc2Df2$ALLOC, 
    labels=sprintf("%s (%.2f%%)", sectorAlloc2Df2$INDUSTRY, sectorAlloc2Df2$ALLOC), 
    col=economist_pal()(9), 
    main=sprintf("%s Sector Allocations (%s) > 2%%", schemeName, date2), 
    family='Segoe UI', 
    init.angle=45)
mtext("@StockViz", side=1)
```


![png](Franklin-India-Prima.R_files/Franklin-India-Prima.R_12_0.png)


This notebook was created using [pluto](http://pluto.studio). Learn more [here](https://github.com/shyams80/pluto)
