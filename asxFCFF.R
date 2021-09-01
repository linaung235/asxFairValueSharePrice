#################
# Htin
# Date 19210826
# Calculate the free cash flow to firm
# Output FCFF name fix
#################

library(priceR)
source("yahoofinV2.R")

#This function takes ticker and currency as inputs, and produces Free Cash Flow to Firm as output

asxFCFF <- function(ticker,currency){

  balanceSheet <- balanceSheetHistory(ticker,"AX")
  cashFlow <- cashflowStatementHistory(ticker,"AX")
  incomeStatement <- incomeStatementHistory(ticker,"AX")
  statementDate = balanceSheet$enddate
  exchangeRateWithDate = historical_exchange_rates("USD", to = currency,
                                                          start_date = statementDate,
                                                            end_date = statementDate)
  exchangeRate = exchangeRateWithDate[,2]
  ebit = incomeStatement$ebitraw*exchangeRate
  taxRate = incomeStatement$incomeTaxExpenseraw /incomeStatement$incomeBeforeTaxraw
  ebitAftertax = ebit*(1-taxRate)
  depreciation = cashFlow$depreciationraw*exchangeRate
  cap_expenditures = -1*cashFlow$capitalExpendituresraw*exchangeRate
  netWorkingCapital = balanceSheet$totalCurrentAssetsraw*exchangeRate - balanceSheet$totalCurrentLiabilitiesraw*exchangeRate
  changeInNetWorkingCapital = netWorkingCapital - lead(netWorkingCapital, default = 0)
  FCFF = ebitAftertax + depreciation - cap_expenditures - changeInNetWorkingCapital

  data.frame(statementDate,FCFF)[1:3,]
}

#Running the FCFF function with data

#asxFCFF("WOW","AUD")


