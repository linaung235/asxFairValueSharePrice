#################
# Htin
# Date 20210826
# Calculate the weighted average cost of capital
# V1 Output WACC name fix
#################
library(readrba)
library(priceR)
library(quantmod)
source("yahoofinV2.R")

#Assumptions
expectedMarketReturn = 0.065    #Average return over last 10 years: 6.5% per year (Australian shares)
                                #Source https://moneysmart.gov.au/how-to-invest/choose-your-investments
#costOfDebt("WOW")

costOfDebt <- function(ticker, exchange = "AX"){

  balanceSheet <- balanceSheetHistory(ticker,exchange)[1,]
  incomeStatement <- incomeStatementHistory(ticker,exchange)[1,]
  balanceSheet[is.na(balanceSheet)] = 0
  totalDebt = balanceSheet$shortLongTermDebtraw + balanceSheet$longTermDebtraw
  costOfDebt = (-1*incomeStatement$interestExpenseraw) / totalDebt

  data.frame(costOfDebt)
}

#costOfEquity("WOW")

#browse_rba_series("Australian government 10 year")
costOfEquity <- function(ticker,exchange = "AX"){

  riskFreeRate =  tail(read_rba(cur_hist = "current", series_id = "FCMYGBAG10D"), n = 1)[,"value"]/100

  statistics <- defaultKeyStatistics(ticker, exchange)
  beta = statistics$betaraw
  costOfEquity = riskFreeRate + beta*(expectedMarketReturn - riskFreeRate)

  data.frame(costOfEquity)
}

#WeightedAverageofCostCapital
WACC <- function (ticker, exchange = "AX"){

  balanceSheet <- balanceSheetHistory(ticker,exchange)[1,]
  incomeStatement <- incomeStatementHistory(ticker,exchange)[1,]
  balanceSheet[is.na(balanceSheet)] = 0
  taxRate = incomeStatement$incomeTaxExpenseraw /incomeStatement$incomeBeforeTaxraw

  debtValue =  (balanceSheet$shortLongTermDebtraw + balanceSheet$longTermDebtraw)*historical_exchange_rates("USD",
                                                                                                           to = "AUD",
                                                                                              start_date = Sys.Date(),
                                                                                             end_date = Sys.Date())[,2]

  stockPrice = data.frame(getSymbols(paste(ticker,".",exchange,sep = ""),src = 'yahoo',
                        from=Sys.Date(),
                        auto.assign = FALSE))[1,4]
  equityValue = defaultKeyStatistics(ticker, exchange)[,"sharesOutstandingraw"]*stockPrice

  value = equityValue + debtValue
  equityWeight = equityValue/value
  debtWeight = debtValue/value
  WACC = costOfDebt(ticker)*debtWeight*(1-taxRate) + costOfEquity(ticker)*equityWeight
  names(WACC) = "WACC"
  WACC
}

#WACC("WOW")


