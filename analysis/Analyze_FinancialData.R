#install.packages("zoo")
library("zoo")
library("ggplot2")
library("tseries")
#library("devtools")
#install.packages("Rcpp")
#library("Rcpp")
#install_github("ramnathv/rCharts")
#library("rCharts")



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
  DailyClosingPrice <- get.hist.quote(ticker,quote="Close")  
  
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

StockPlot(stock)
#########################################################################
# check the functions are working

#stock <-AnalyzeStock("LII")
#names(stock)
#stock$Ticker
#stock$PlotTitle
#stock$BeginDate
#stock$EndDate
#stock$Volatility
#stock$Obs
#stock$Data
#stock$LogReturns

#set digits =2
options(digits=2)

#########################################################################
# do the calcs the hard way to see how they work.

#LII <- get.hist.quote('lii',quote="Close")
#length(LII)
#LIIbegindate <- start(LII)
#LIIenddate <- end(LII)
#LIIlag <- lag(LII)
#LIIdtlcalc <- cbind(LII, LIIlag) 
#LIIlog1 <- log(LIIlag)
#LIIlog2 <- log(LII)
#LIIlog3 <- LIIlog1 - LIIlog2
#LIIret <- log(lag(LII))-log(LII)
#LIIdtlcalc <- cbind(LIIdtlcalc, LIIlog1,LIIlog2,LIIlog3,LIIret)
#PlotTitle <-paste("Lennox International (LII) closing stock price for the period ",LIIbegindate," to ", LIIenddate, sep="")
#autoplot.zoo(LII) + ggtitle(PlotTitle)

##########################################################################

StockPlot(stock)

P1<-autoplot.zoo(stock$Data) + ggtitle(stock$PlotTitle)

stockPG <- AnalyzeStock("PG")
StockPlot(stockPG)


stock <-AnalyzeStock("^gspc")
volest31<- Volhistorical(10,stock$LogReturns)
str(volest31)
head(volest31)

plot(volest31,type="l", col="red")


stock$Volatility

StockPlot(stock)
plot(stock$LogReturns)
stock <-AnalyzeStock("pg")
StockPlot(stock)



plot(LII)
autoplot.zoo(volest31) + 


volest <- Vol(10,SNPret)
voltest2 <- Vol(30,SNPret)
voltest3 <- Vol(100,SNPret)

class(volest)

plot(volest,type="l")
lines(voltest2,type="l", col="red")
lines(voltest3,type="l", col="blue")
lines(SNPdata,type="l",col="green")

P10 <-ggplot(data=fortify(volest), aes(x=Index,y=Value)) + geom_line()

vol10<- Volhistorical(10,stockPG$LogReturns)
vol30<- Volhistorical(30,stockPG$LogReturns)
vol100<- Volhistorical(100,stockPG$LogReturns)


plot(vol10,type="l",xlab="index",ylab="Volatility")
title(main=paste("Plot of volatility for ",stockPG$Ticker," for the period ",stockPG$BeginDate," to ",stockPG$EndDate,sep=""))
lines(vol30,type="l", col="red")
lines(vol100,type="l",col="blue")
legend(5000,.20,c("10","30","100"), lty=c(1,1,1),lwd=c(2.5,2.5,2.5),col=c("black","red","blue"),title="Decay Factor")

stop("no need to run anything past this point")

##########################################################################
plot(PGstock,type="l",col="blue")
lines(SNPdata,type="l", col="red")
ggplot(PGstock, geom="l")


PGstock <- get.hist.quote('pg',quote="Close")
plot(SNPdata,type="l", col="red")

P1<- autoplot.zoo(SNPdata) + ggtitle("S&P 500") +ylim(c(0,2500))
P1 +  geom_line(PGstock)

begindate <- start(SNPdata)
enddate <- end(SNPdata)

plot(PGstock,type="l", col="blue")
class(SNPdata)
class(PGstock)

str(SNPdata)
str(PGstock)

P1<-ggplot(data=PGstock, aes(x=index, y=value)) +scale_y_log10()
P1 + geom_line(data=SNPdata, aes(x=index,y=value))
P1

plot(SNPdata, value, ylim=range(c(0,10000)))
, aes(x=index, y=value)) 
+ geom_line()

SNPdata <- get.hist.quote('^gspc',quote="Close")
SNPlogreturns <- log(lag(SNPdata))-log(SNPdata)
SNPvol <- sd(SNPlogreturns) * sqrt(250)*100
volest <- Vol(10,SNPlogreturns)
plot(volest,type="l", col="blue")

