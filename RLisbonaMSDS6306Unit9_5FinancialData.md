# RLisbonaMSDS6306Unit9_5FinancialDataAnalysis
Randy Lisbona  
July 17, 2016  




# Unit 9.5 Preparing Financial Data

For this assignment we will look at historical stock price data, calculate the overall volatility of the stock, calculate the log returns and plot volatility for three different decay factors

Procter and Gamble (PG) was the assigned stock for this this project.

  >Procter & Gamble Co provides consumer packaged goods. It markets its products in about 180 countries through mass merchandisers, grocery stores, membership club stores, drug stores, department stores among others. for more information on Proctor and Gamble see <https://www.thestreet.com/quote/PG/details/profile.html>

# Proctor and Gamble Historical Stock price

![](RLisbonaMSDS6306Unit9_5FinancialData_files/figure-html/getPG -1.png)<!-- -->





# Examine Log Returns for PG

This chart plots the difference in the log of each days closing price to the log of the previous day. 
Using the log of the closing price lets us add the returns over the time period.  Percent returns are not additive.
Notice that PG stock prices had significant price drops over the past 25 years. These correspond to 2 for 1 stock splits in:

1. 1992
2. 1997
3. 2004
  
![](RLisbonaMSDS6306Unit9_5FinancialData_files/figure-html/logreturns-1.png)<!-- -->

# Calculate and plot volativity using decay factors of 10,30,100 over the time period

![](RLisbonaMSDS6306Unit9_5FinancialData_files/figure-html/plotVolativity-1.png)<!-- -->

# Methodology

The functions used to calculate ovarall volatility, and historical volatility by time period are included below





```r
#########################################################################
## volatilityget function
#########################################################################


Volhistorical <- function(d,logreturns){
  var=0
  lam=0
  varlist<-c()
  for (r in logreturns){
    lam = lam*(1-1/d) +1
    var = (1-1/lam) * var + (1/lam)*r^2
    varlist <- c(varlist,var)
  }
  sqrt(varlist)}
#########################################################################
# function to download the historical data and do some basic calculations
#########################################################################
AnalyzeStock <- function(ticker){
  DailyClosingPrice <- get.hist.quote(ticker,quote="Close",quiet=TRUE)  
  
  begindt <- start(DailyClosingPrice)
  enddt <- end(DailyClosingPrice)
  
  obs=length(DailyClosingPrice)
  
  logreturns <- log(lag(DailyClosingPrice))-log(DailyClosingPrice)
  # 252 trading days per year
  vol <- round(sd(logreturns) * sqrt(252)*100,2)
  plottitle <- paste("Historical Closing price for (",ticker,") for period ",begindt, " to ", enddt,"\n",
                     " Obs= ",obs,"  ","Historical Volatility = ",vol,sep="")
  
  list(Ticker = ticker,
       Data=DailyClosingPrice,
       Obs=obs,
       LogReturns=logreturns,
       BeginDate=begindt,
       EndDate=enddt,
       Volatility=vol,
       PlotTitle = plottitle
  )
}
#########################################################################
# simple function to plot the data and include the title
#########################################################################
StockPlot <- function(stockname) {
  
  autoplot.zoo(stockname$Data) + 
    ggtitle(stockname$PlotTitle) 
  
}
```
