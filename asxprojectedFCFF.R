#################
# Htin
# Date 20210831
# Forecast the Free Cash Flow to Firm 
#################
source("yahoofinV2.R")
source("asxWACCV1.R")
library("dplyr")


#Calculate average revenue growth rate for the last 3 years.
#averageRevenueGrowthRate("WOW")
averageRevenueGrowthRate <- function(ticker, exchange = "AX"){
  
  incomeStatement <-  incomeStatementHistory(ticker, exchange)
  totalRevenue = incomeStatement$totalRevenueraw
  revenueGrowthRate = totalRevenue / lead(totalRevenue, default = 0)-1
  is.na(revenueGrowthRate) <- sapply(revenueGrowthRate, is.infinite)
  averageRevenueGrowthRate = mean(revenueGrowthRate, na.rm = TRUE)
  
  averageRevenueGrowthRate
}

#Calculate average net income margin.
#averageNetIncomeMargin("WOW")
averageNetIncomeMargin <- function(ticker, exchange = "AX"){
  
  incomeStatement <- incomeStatementHistory(ticker,exchange)
  totalRevenue = incomeStatement$totalRevenueraw
  netIncome = incomeStatement$netIncomeraw
  netIncomeMargin = netIncome / totalRevenue
  averageNetIncomeMargin = mean(netIncomeMargin[1:3], na.rm = TRUE)
  
  averageNetIncomeMargin
}

#Calculate FCFF as percentage of net income and choose the lowest percentage as being conservative.
#minimumFCFFMargin("WOW")
minimumFCFFMargin <- function(ticker, exchange = "AX"){
  
  FCFF <- asxFCFF(ticker, currency = "USD")[1:3,]
  incomeStatement <- incomeStatementHistory(ticker, exchange)[1:3,]
  FCFFmargin = FCFF[,2] / incomeStatement$netIncomeraw
  
  min(FCFFmargin)
}

#Calculate projected FCFF using averageRevenueGrowthRate, averageNetIncomeMargin and minimumFCFFMargin.
#asxprojectedFCFF
#inputs-parameters :ticker, projectedYears
#asxprojectedFCFF("BHP", 3)
asxprojectedFCFF <- function(ticker, projectedYears = 3){
  #get 4 years incomestatement given a ticker
  incomeStatement <- incomeStatementHistory(ticker, "AX")
  #calculate averageRevenueGrowthRate for 3 years from incomestatement
  averageRevenueGrowthRate = averageRevenueGrowthRate(ticker)
  #get totalRevenue for the last financial year from incomestatement
  totalRevenue = incomeStatement[,"totalRevenueraw"]
  lastTotalRevenue = totalRevenue[1]
  finYears = as.numeric(format(incomeStatement[, "enddate"], format = "%Y"))
  lastFinancialYear = finYears[1]
  seqProjectedYears = (lastFinancialYear+1):(lastFinancialYear+projectedYears)
  #calculate projectedRevenue for projectedYears using averageRevenueGrowthRate and totalRevenue from the last financial year.
  seqYearInAdvance = 1:projectedYears
  projectedRevenue = lastTotalRevenue*(1+averageRevenueGrowthRate(ticker))^seqYearInAdvance
  #projectedRevenue = data.frame(finYear = seqProjectedYears, 
                                #Revenue = lastTotalRevenue*(1+averageRevenueGrowthRate(ticker))^seqYearInAdvance,  
                                #projected = T)
  #projectedRevenue = rbind(data.frame(Revenue = totalRevenue, finYear = finYears, projected = F)
                           #, projectedRevenue)
  projectedRevenue#[order(projectedRevenue$finYear, decreasing = T), ]
  #calculate projectedNetIncome using projectedRevenue and averageNetIncomeMargin.
  projectedNetIncome = projectedRevenue*averageNetIncomeMargin(ticker)
  #calculate projectedFCFF using projectedNetIncome.
  projectedFCFF = data.frame(finYear = seqProjectedYears,
                             advanceYear = seqYearInAdvance,
                             revenue = projectedRevenue,
                             netIncome = projectedNetIncome,
                             FCFF = projectedNetIncome*minimumFCFFMargin(ticker),
                             WACC = WACC(ticker),
                             projected = T)
  projectedFCFF
}

#Calculate terminal value using last projected FCFF, WACC and perpetual FCFF growth rate 
#Inputs parameters: ticker
#asxterminalValue("BHP", 3)
asxterminalValue <- function(ticker, projectedYears = 3){
  #get FCFF for last projected year from asxprojectedFCFF
  lastProjectedFCFF = tail(asxprojectedFCFF(ticker, projectedYears)[,"FCFF"], n = 1)
  #get perpetual growth rate
  perpetualGrowthRate = 0.02 #Australia average GDP growth rate for last 10 years(2011-2020) is 2.358%.
                            #Source https://www.macrotrends.net/countries/AUS/australia/gdp-growth-rate 
  #get wacc for given company
  WACC = WACC(ticker)
  #calculate terminal value at last projected year.
  terminalValue = lastProjectedFCFF*(1+perpetualGrowthRate)/(WACC - perpetualGrowthRate)
  names(terminalValue) = "terminalValue"
  terminalValue
}










 
 
  







                                                                 