# Tony liu 2021 08 18
# asxstocks return a list of current stocks from ASX exchange
library(jsonlite)


#assetProfile -- done
#balanceSheetHistory -- done
#balanceSheetHistoryQuarterly --done
#calendarEvents -- done
#cashflowStatementHistory -- done
#cashflowStatementHistoryQuarterly -- done
#defaultKeyStatistics -- done
#earnings
#earningsHistory
#earningsTrend
#esgScores
#financialData
#fundOwnership
#fundProfile
#fundPerformance
#incomeStatementHistory -- done
#incomeStatementHistoryQuarterly --done
#indexTrend
#industryTrend
#insiderHolders
#insiderTransactions
#institutionOwnership
#majorDirectHolders
#majorHoldersBreakdown
#netSharePurchaseActivity
#price
#pageviews
#recommendationTrend
#secFilings
#sectorTrend
#summaryDetail
#summaryProfile
#symbol
#topHoldings
#upgradeDowngradeHistory
#pageviews
#quotetype


#a<-assetProfile("WOW", "AX")
assetProfile <- function(ticker, exchange) {
  json<-fromJSON(paste0("https://query1.finance.yahoo.com/v10/finance/quoteSummary/",ticker,".",exchange,"?modules=AssetProfile"))
  findata<- json$quoteSummary$result$assetProfile
  findata$ticker<-ticker
  findata
}

yExtract <- function(ticker, exchange, findata){
  findatanames<-names(findata)
  names(findata[[2]])<-paste(findatanames[2], names(findata[[2]]),sep="")
  findataall<-data.frame(enddate = as.POSIXct(findata[[2]]$endDateraw, origin = "1970-01-01"))
  for (i in 3:length(findata))
  {
    if(ncol(findata[[i]])==0)
      findata[[i]]$raw = 0
    names(findata[[i]])<-paste(findatanames[i], names(findata[[i]]),sep="")
    findataall<-cbind(findataall, findata[[i]])
  }
  findataall$ticker <- ticker
  findataall$exchange <- exchange
  findataall
}

yinfo <- function(ticker, exchange, module) {
  json<-fromJSON(paste0("https://query1.finance.yahoo.com/v10/finance/quoteSummary/",ticker,".",exchange,"?modules=", module))
  findata<- json[[1]][[1]][[1]][[1]][[1]]
  #findata$ticker<-ticker
  yExtract(ticker,exchange, findata)
}
#e.g.yinfo("WOW","AX", "BalanceSheetHistory")

#wow<-balanceSheetHistory("WOW", "AX")
balanceSheetHistory <- function(ticker, exchange) {
  #annual balanace sheet 4 years
  yinfo(ticker,exchange, "balanceSheetHistory")
}

#balanceSheetHistoryQuarterly("WOW", "AX")
balanceSheetHistoryQuarterly <- function(ticker, exchange) {
  yinfo(ticker,exchange, "balanceSheetHistoryQuarterly")
}

#cashflowStatementHistory("WOW", "AX")
cashflowStatementHistory<- function(ticker, exchange) {
  yinfo(ticker,exchange, "cashflowStatementHistory")
}

#cashflowStatementHistoryQuarterly("WOW", "AX")
cashflowStatementHistoryQuarterly<-function(ticker, exchange) {
  yinfo(ticker,exchange, "cashflowStatementHistoryQuarterly")
}

#incomeStatementHistory("WOW", "AX")
incomeStatementHistory<- function(ticker, exchange) {
  yinfo(ticker,exchange, "incomeStatementHistory")
}

#incomeStatementHistoryQuarterly("WOW", "AX")
incomeStatementHistoryQuarterly<- function(ticker, exchange) {
  yinfo(ticker,exchange, "incomeStatementHistoryQuarterly")
}

#defaultKeyStatistics("WOW", "AX")
defaultKeyStatistics<-function(ticker, exchange) {
  module <-  "defaultKeyStatistics"
  json<-fromJSON(paste0("https://query1.finance.yahoo.com/v10/finance/quoteSummary/",ticker,".",exchange,"?modules=", module))
  findata<- json[[1]][[1]][[1]]
  #findata$ticker<-ticker
  findatanames<-names(findata)
  names(findata[[2]])<-paste(findatanames[2], names(findata[[2]]),sep="")
  findataall<- findata[[2]]
  for (i in 3:length(findata))
  {
    if (class(findata[[i]])=="data.frame"){
      if(ncol(findata[[i]])==0)
        findata[[i]]$raw = 0
      names(findata[[i]])<-paste(findatanames[i], names(findata[[i]]),sep="")
      findataall<-cbind(findataall, findata[[i]])
    }
  }
  findataall$ticker <- ticker
  findataall$exchange <- exchange
  findataall
}

#calendarEvents("WOW","AX")
calendarEvents<-function(ticker, exchange) {
  module <-  "calendarEvents"
  json<-fromJSON(paste0("https://query1.finance.yahoo.com/v10/finance/quoteSummary/",ticker,".",exchange,"?modules=", module))
  findata<- json[[1]][[1]][[1]]
  #findata$ticker<-ticker
  data.frame(t(unlist(yExtract(ticker,exchange, findata))))
}
#earnings("WOW","AX")
earnings<-function(ticker, exchange) {
  module <-  "earnings"
  json<-fromJSON(paste0("https://query1.finance.yahoo.com/v10/finance/quoteSummary/",ticker,".",exchange,"?modules=", module))
  findata<- json[[1]][[1]][[1]]

}
#earningsHistory
#earningsTrend
#esgScores
#financialData
#fundOwnership
#fundProfile
#fundPerformance
#incomeStatementHistory
#incomeStatementHistoryQuarterly
#indexTrend
#industryTrend
#insiderHolders
#insiderTransactions
#institutionOwnership
#majorDirectHolders
#majorHoldersBreakdown
#netSharePurchaseActivity
#price
#pageviews
#recommendationTrend
#secFilings
#sectorTrend
#summaryDetail
#summaryProfile
#symbol
#topHoldings
#upgradeDowngradeHistory
#pageviews
#quotetype

