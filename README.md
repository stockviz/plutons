# plutons
notebooks created on pluto

### Currencies
[Dollar Indices on the FRED](fred-dollar-indices.R.ipynb): Explore trade-weighted dollar indices on the FRED and compare other USD currency pairs, like USDINR, with trade-weighted indices.\
[Currency futures on the NSE](plotting-currency-futures.R.ipynb): Creating a continuous series out of futures contracts.\
[An overview of how currency sources on pluto](currency-sources.R.ipynb)

### Gold
[Gold vs. NIFTY 50](gold-nifty50.ipynb): Explore historical returns of gold vs. the NIFTY 50 Total Return (TR) index in dollars.\
[GOLDBEES vs. NIFTYBEES](GOLDBEES-NIFTYBEES.R.ipynb): Comparision of the two India listed ETFs.\
[Gold futures listed on MCX](GOLD-MCX.R.ipynb): Exploring the plethora of gold futures contracts listed on the MCX.

### NIFTY 50
[Rolling Returns](nifty-rolling-returns.R.ipynb): Plot rolling returns over different time-horizons. \
[SMA On/Off](NIFTY50-SMA.R.ipynb): Toggling market exposure based on a Simple Moving Average.

### [Statistical distribution of the returns of an index](return-distribution.R.ipynb)
Density plots and summary statistics.

### [Plot time-series](time-series-charts.R.ipynb)
ggplot, quantmod are fair game.

### [Mutual funds vs. Index drawdowns](mutual-fund-drawdown.R.ipynb)
Do active managers reduce drawdowns during downturns? Top 5 mid-cap funds by AUM at the turn 2017 show that active managers can and do navigate through drawdowns.

### [Mutual fund sector allocation](mutual-fund-sector-allocations.R.ipynb)
A tile plot to visualize sector allocations through time. A caveat: classifications can and do change over time. For example, HDFCBANK was first "BANK" and since March 2018, it is "FINANCIAL SERVICES".\
[Franklin Prima](https://github.com/stockviz/plutons/blob/e47cecbdabcf912d812171a40b9fa1a45445e8c3/mutual-fund-sector-allocations.R.ipynb)\
[Mirae Emerging Blue-chip](https://github.com/stockviz/plutons/blob/f9e12bd5fab761adbcc7116a07c3c7ba5e6ceb4e/mutual-fund-sector-allocations.R.ipynb)

### [Mutual fund sector allocation as per BSE](mutual-fund-sector-allocations-BSE.R.ipynb)
The Bombay Stock Exchange has its own logic to classifying stocks by sector. It gives a more consistent and granular view of allocations.

### [Sector allocations of the Franklin Prima Fund](Franklin-India-Prima.R.ipynb)
Pie charts of allocations at different periods. Got a bit tricky because the notebook bloted beyond the 1MB limit of github. So now, on exceeding the limit, markdown files with images extracted to separate files is uploaded instead. The big notebook files stays on pluto.

### [Visualize mutual fund portfolio overlaps with other indices and mutual funds](Mutual-fund-portfolio-overlaps.R.ipynb)
Use Upset charts for visualizing fund portfolio overlaps.

### Fun with NASDAQOMX index data-sets
Explore [commodity indices](NASDAQOMX-commodities.R.ipynb) or [Indian equity indices](NASDAQOMX-india.R.ipynb) in USD

### [Backtesting a 60/40 Indian equity/bond portfolio](60-40-equity-bond-india.R.ipynb)
Uses a monthly return series of NIFTY 50 and MIDCAP 100 total-return indices as a stand-in for equities and short-term (< 5 year maturity) government bond total-return index as a stand-in for bonds.

### [Backtesting an S&P 500 "VIX Adjusted Momentum" strategy](vix-adjusted-momentum-US.R.ipynb)
Works mostly because it side-steps some steep drawdowns. However, when the same was tested on [NIFTY/INDIA-VIX](vix-adjusted-momentum-INDIA.R.md), it under-performed buy-and-hold. And [adding an SMA(50) layer](vix-adjusted-momentum-and-SMA-INDIA.R.ipynb) only makes it worse.

### [Getting to "real" returns](inflation-adjusted-returns.R.ipynb)
Inflation tends to vary widely through time. To see what "real", i.e., after inflation, returns are, the rate of inflation needs to be subtracted from gross returns. Also, [charting historical inflation](cpi-inflaiton-world-bank.ipynb)

### Yield curves!
[Zero-coupon yields vs. Sovereign bond index yields](zero-coupon.vs.bond-yeilds.R.ipynb)\
[Plotting the 10-year yields of India, US and Euro-AAAs](10-year-yields.R.ipynb)

### Equity time-series
[Plotting a stock's price chart with corporate action overlay](charting-stocks-with-corporate-actions.R.ipynb)\
[Comparing stocks with indices](compare-stock-vs-index.R.ipynb)

### [Tracking FII flows](FII-capital-market-flows.R.ipynb)

### [NSE's "Alpha" Indices charted](NIFTY-ALPHA-indices.R.ipynb)

## Fama-French

### Momentum Factor
[Equity Risk Premium](Fama-French/Equity-Risk-Premium.R.ipynb): An equity investor takes on a lot more risk compared to someone who buys only US government bonds. So, in theory, the equity investor should earn returns in excess of those given by bonds. But how much more should that be?\
[Value vs. Equal weighting](Fama-French/Industry-Returns.R.md):The decision between value weighting and equal weighting a portfolio cannot be taken in haste. It ends up making a big difference over time.\
[Low-momentum ≠ Negative-returns](Fama-French/Momentum-Decile-Performance.R.md): Sometimes, everything rallies. Just because a set of stocks performed poorly in the past doesn’t necessarily mean that they will have *negative* returns going-forward. \
[Long-Short comes up short](Fama-French/Long-Short-Momentum.R.md): The last 10-years in the US markets have been a one-way, long-only bet. So if you constructed a long-short momentum portfolio, the chances are that you got hosed.\
[Out-lasting statistics](Fama-French/Momentum.R.ipynb): Just because momentum is statistically shown to out-perform the market over long periods of time, the actual period during which it out-performs may not overlap with your time horizon.


