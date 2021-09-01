#################
# Htin
# Date 20210831
# Discounted Cash Flow valuation 
#################

source("yahoofinV2.R")
source("asxWACCV1.R")
source("asxprojectedFCFF.R")
library("priceR")

#calculate present value of the firm
#inputs parameters: ticker, curreny, projectedYears
#asxfirmValue("BHP")

asxfirmValue <- function(ticker, currency = "AUD", projectedYears = 3){
  #Get projectedFCFF
  projectedData = asxprojectedFCFF(ticker, projectedYears)
  projectedFCFF = projectedData$FCFF
  advanceYear = projectedData$advanceYear
  WACC = projectedData$WACC
  #Calculate present value of all projected FCFFs
  presentValueFCFF = cbind(data.frame(projectedData, presentValueFCFF = projectedFCFF/(1+WACC)^advanceYear))
  #Get Terminal Value
  terminalValue = asxterminalValue(ticker, projectedYears)
  #Calculate present value of terminalValue
  presentTerminalValue = terminalValue/(1+WACC)^projectedYears
  #Calculate the firm value
  firmValue = sum(presentValueFCFF$presentValueFCFF) + presentTerminalValue
  names(firmValue) = "firmValue"
  #convert to given currency
  currentExchangeRate = historical_exchange_rates("USD",
                                                  to = currency,
                                                  start_date = Sys.Date(),
                                                  end_date = Sys.Date())[,2]
  firmValue = firmValue*currentExchangeRate
  firmValue
}

#calculate fairValueSharePrice
#input: ticker, curreny, projectedYears
#asxfairValueSharePrice("TLS")

asxfairValueSharePrice <- function(ticker, exchange = "AX", currency = "AUD", projectedYears = 3){
  #Calculate fair value share price
  sharesOutstanding = defaultKeyStatistics(ticker, exchange)[,"sharesOutstandingraw"]
  firmValue = asxfirmValue(ticker, currency, projectedYears)
  fairValueSharePrice = firmValue/sharesOutstanding
  names(fairValueSharePrice) = "fairValueSharePrice"
  #Get market share price
  marketSharePrice = data.frame(getSymbols(paste(ticker,".",exchange,sep = ""),src = 'yahoo',
                                     from=Sys.Date(),
                                     auto.assign = FALSE))[1,4]
  data.frame(fairValueSharePrice = fairValueSharePrice, marketSharePrice = marketSharePrice, ticker = ticker)
}


