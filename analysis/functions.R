
## @knitr Unit9_5_Functions

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