# plutons
notebooks created on pluto

### [Dollar Indices on the FRED](fred-dollar-indices.R.ipynb)
Explore trade-weighted dollar indices on the FRED and compare other USD currency pairs, like USDINR, with trade-weighted indices.

### [Gold vs. NIFTY 50](gold-nifty50.ipynb)
Explore historical returns of gold vs. the NIFTY 50 Total Return (TR) index in dollars.

### [Rolling Returns of NIFTY 50](nifty-rolling-returns.R.ipynb)
Plot rolling returns over different time-horizons.

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

