# loading S&P 500 Index data from Yahoo Finance
library(quantmod)
# look up stock symbol from Yahoo Finance
getSymbols("SPY", from='2007-01-03',to='2020-02-15')
head(SPY)

# subsetting data into calibration and backtesting data
# data is roughly split into two 5-year periods
calibration = SPY["2010-02-12/2015-02-12"]
backtest = SPY["2015-02-13/2020-02-14"]

##############################################################################
# calibration of strategy
# to how strategy works
lineChart(calibration, theme = "white", subset = "2012::2015")
addRSI(n=14,maType="EMA")
addSMA(n = 50, col = "black")
addSMA(n = 200, col = "blue")

# depiction of death cross
lineChart(SPY, theme = "white", subset = "2007::2009")
addSMA(n = 50, col = "black")
addSMA(n = 200, col = "blue")

# n starts from 201 because we are using 200-day SMA as one of our indicators
# golden cross/death cross used so we need to compare yesterday's price to today's price
# which is why n needs to be 201
n = 201

# need to calibrate oversold threshold for RSI
# the default is where RSI < 30 is considered to be oversold
# but sometimes less conservative values of 40 or 50 could lead to better performance

# treat RSI < 30 as oversold level
# generate signal, 1 means buy, 0 means hold, -1 means sell
signal_in_30 = c()
signal_in_30[1:(n+1)] = 0

# save closing price
price_in = Cl(calibration)

# stock holdings each day
stock_in_30 = c()
stock_in_30[1:(n+1)] = 0

# cash holding each day
cash_in_30 = c()
# assume initial cash holding of 1000000
cash_in_30[1:(n+1)] = 1000000

# generate technical indicators
# 14-day RSI
rsi_in = RSI(price_in, n=14, maType = EMA)

# 50-day SMA
sma50_in = SMA(price_in, n = 50)

# 200-day SMA
sma200_in = SMA(price_in, n = 200)

# golden cross strategy using longer term SMA indicators to ride longer-term trends
# use of RSI to continuously buy in the midst of longer-term bullish trend by detecting oversold levels
for (i in (n+1): length(price_in)){
  if (sma50_in[i-1] < sma200_in[i-1] && sma50_in[i] > sma200_in[i]){ 
    signal_in_30[i] = 1
  } else if (sma50_in[i-1] > sma200_in[i-1] && sma50_in[i] < sma200_in[i]){
    signal_in_30[i] = -1
  } else if (sma50_in[i] > sma200_in[i] && rsi_in[i] < 30){
    signal_in_30[i] = 1
  } else { 
    signal_in_30[i] = 0
  }
}

signal_in_30 = reclass(signal_in_30,price_in)

# trade based on yesterday's signal
trade_in_30 = lag(signal_in_30)

# count number of transactions made
count_in_30 = c()
count_in_30[1:(n+1)] = 0

# assume 1000 units bought each time there is a buy signal
# all units sold each time there is a sell signal
for (i in (n+2): length(price_in)){
  # no borrowing, commission of 0.3% per transaction
  if (trade_in_30[i] == 1 && cash_in_30[i-1] >= 1000*price_in[i]*1.003){
    stock_in_30[i] = stock_in_30[i-1] + 1000
    cash_in_30[i] = cash_in_30[i-1] - 1000*price_in[i]*1.003
    count_in_30[i] = count_in_30[i-1] + 1
    # no short-selling, commission of 0.3% per transaction
  } else if (trade_in_30[i] == -1 && stock_in_30[i-1] > 0){
    stock_in_30[i] = 0
    cash_in_30[i] = cash_in_30[i-1] + stock_in_30[i-1]*price_in[i]*0.997
    count_in_30[i] = count_in_30[i-1] + 1
    # hold otherwise
  } else {
    stock_in_30[i] = stock_in_30[i-1]
    cash_in_30[i] = cash_in_30[i-1]
    count_in_30[i] = count_in_30[i-1]
  }
}
stock_in_30 = reclass(stock_in_30,price_in)
cash_in_30 = reclass(cash_in_30,price_in)
count_in_30 = reclass(count_in_30,price_in)

# to get total value of cash and stock holdings
equity_in_30  = c()
equity_in_30[1:(n+1)] = 1000000

# to calculate daily returns based on equity
return_in_30 = c()
return_in_30[1:(n+1)] = 0

for (i in (n+1): length(price_in)){
  equity_in_30[i] = stock_in_30[i] * price_in[i] + cash_in_30[i]
  return_in_30[i] = equity_in_30[i]/equity_in_30[i-1]-1
}
equity_in_30 = reclass(equity_in_30,price_in)
return_in_30 = reclass(return_in_30,price_in)

# treat RSI < 40 as oversold level
# generate signal, 1 means buy, 0 means hold, -1 means sell
signal_in_40 = c()
signal_in_40[1:(n+1)] = 0

# stock holdings each day
stock_in_40 = c()
stock_in_40[1:(n+1)] = 0

# cash holding each day
cash_in_40 = c()
# assume initial cash holding of 1000000
cash_in_40[1:(n+1)] = 1000000

# golden cross strategy using longer term SMA indicators to ride longer-term trends
# use of RSI to continuously buy in the midst of longer-term bullish trend by detecting oversold levels
for (i in (n+1): length(price_in)){
  if (sma50_in[i-1] < sma200_in[i-1] && sma50_in[i] > sma200_in[i]){ 
    signal_in_40[i] = 1
  } else if (sma50_in[i-1] > sma200_in[i-1] && sma50_in[i] < sma200_in[i]){
    signal_in_40[i] = -1
  } else if (sma50_in[i] > sma200_in[i] && rsi_in[i] < 40){
    signal_in_40[i] = 1
  } else { 
    signal_in_40[i] = 0
  }
}

signal_in_40 = reclass(signal_in_40,price_in)

# trade based on yesterday's signal
trade_in_40 = lag(signal_in_40)

# count number of transactions made
count_in_40 = c()
count_in_40[1:(n+1)] = 0

for (i in (n+2): length(price_in)){
  # no borrowing, commission of 0.3% per transaction
  if (trade_in_40[i] == 1 && cash_in_40[i-1] >= 1000*price_in[i]*1.003){
    stock_in_40[i] = stock_in_40[i-1] + 1000
    cash_in_40[i] = cash_in_40[i-1] - 1000*price_in[i]*1.003
    count_in_40[i] = count_in_40[i-1] + 1
    # no short-selling, commission of 0.3% per transaction
  } else if (trade_in_40[i] == -1 && stock_in_40[i-1] > 0){
    stock_in_40[i] = 0
    cash_in_40[i] = cash_in_40[i-1] + stock_in_40[i-1]*price_in[i]*0.997
    count_in_40[i] = count_in_40[i-1] + 1
    # hold otherwise
  } else {
    stock_in_40[i] = stock_in_40[i-1]
    cash_in_40[i] = cash_in_40[i-1]
    count_in_40[i] = count_in_40[i-1]
  }
}
stock_in_40 = reclass(stock_in_40,price_in)
cash_in_40 = reclass(cash_in_40,price_in)
count_in_40 = reclass(count_in_40,price_in)

# to get total value of cash and stock holdings
equity_in_40  = c()
equity_in_40[1:(n+1)] = 1000000

# to calculate daily returns based on equity
return_in_40 = c()
return_in_40[1:(n+1)] = 0

for (i in (n+1): length(price_in)){
  equity_in_40[i] = stock_in_40[i] * price_in[i] + cash_in_40[i]
  return_in_40[i] = equity_in_40[i]/equity_in_40[i-1]-1
}
equity_in_40 = reclass(equity_in_40,price_in)
return_in_40 = reclass(return_in_40,price_in)

# treat RSI < 50 as oversold level
# generate signal, 1 means buy, 0 means hold, -1 means sell
signal_in_50 = c()
signal_in_50[1:(n+1)] = 0

# stock holdings each day
stock_in_50 = c()
stock_in_50[1:(n+1)] = 0

# cash holding each day
cash_in_50 = c()
# assume initial cash holding of 1000000
cash_in_50[1:(n+1)] = 1000000

# golden cross strategy using longer term SMA indicators to ride longer-term trends
# use of RSI to continuously buy in the midst of longer-term bullish trend by detecting oversold levels
for (i in (n+1): length(price_in)){
  if (sma50_in[i-1] < sma200_in[i-1] && sma50_in[i] > sma200_in[i]){ 
    signal_in_50[i] = 1
  } else if (sma50_in[i-1] > sma200_in[i-1] && sma50_in[i] < sma200_in[i]){
    signal_in_50[i] = -1
  } else if (sma50_in[i] > sma200_in[i] && rsi_in[i] < 50){
    signal_in_50[i] = 1
  } else { 
    signal_in_50[i] = 0
  }
}

signal_in_50 = reclass(signal_in_50,price_in)

# trade based on yesterday's signal
trade_in_50 = lag(signal_in_50)

# count number of transactions made
count_in_50 = c()
count_in_50[1:(n+1)] = 0

for (i in (n+2): length(price_in)){
  # no borrowing, commission of 0.3% per transaction
  if (trade_in_50[i] == 1 && cash_in_50[i-1] >= 1000*price_in[i]*1.003){
    stock_in_50[i] = stock_in_50[i-1] + 1000
    cash_in_50[i] = cash_in_50[i-1] - 1000*price_in[i]*1.003
    count_in_50[i] = count_in_50[i-1] + 1
    # no short-selling, commission of 0.3% per transaction
  } else if (trade_in_50[i] == -1 && stock_in_50[i-1] > 0){
    stock_in_50[i] = 0
    cash_in_50[i] = cash_in_50[i-1] + stock_in_50[i-1]*price_in[i]*0.997
    count_in_50[i] = count_in_50[i-1] + 1
    # hold otherwise
  } else {
    stock_in_50[i] = stock_in_50[i-1]
    cash_in_50[i] = cash_in_50[i-1]
    count_in_50[i] = count_in_50[i-1]
  }
}
stock_in_50 = reclass(stock_in_50,price_in)
cash_in_50 = reclass(cash_in_50,price_in)
count_in_50 = reclass(count_in_50,price_in)

# to get total value of cash and stock holdings
equity_in_50  = c()
equity_in_50[1:(n+1)] = 1000000

# to calculate daily returns based on equity
return_in_50 = c()
return_in_50[1:(n+1)] = 0

for (i in (n+1): length(price_in)){
  equity_in_50[i] = stock_in_50[i] * price_in[i] + cash_in_50[i]
  return_in_50[i] = equity_in_50[i]/equity_in_50[i-1]-1
}
equity_in_50 = reclass(equity_in_50,price_in)
return_in_50 = reclass(return_in_50,price_in)

# compare performance for all 3 threshold values
library(PerformanceAnalytics)
compare_RSI = cbind(return_in_30, return_in_40, return_in_50)
# metrics to evaluate strategy
tab.perf.in = table.Arbitrary(compare_RSI,
                                 metrics=c(
                                   "Return.cumulative",
                                   "Return.annualized",
                                   "SharpeRatio.annualized",
                                   "maxDrawdown"),
                                 metricsNames=c(
                                   "Cumulative Return",
                                   "Annualized Return",
                                   "Annualized Sharpe Ratio",
                                   "Max Drawdown"))
tab.perf.in
# since RSI < 30 as oversold level gives the best performance, to go with that

##############################################################################
# backtesting of strategy
signal = c()
signal[1:(n+1)] = 0

price = Cl(backtest)

stock = c()
stock[1:(n+1)] = 0

cash = c()
cash[1:(n+1)] = 1000000

rsi = RSI(price, n=14, maType = EMA)

sma50 = SMA(price, n = 50)
sma200 = SMA(price, n = 200)

for (i in (n+1): length(price)){
  if (sma50[i-1] < sma200[i-1] && sma50[i] > sma200[i]){ 
    signal[i] = 1
  } else if (sma50[i-1] > sma200[i-1] && sma50[i] < sma200[i]){
    signal[i] = -1
  } else if (sma50[i] > sma200[i] && rsi[i] < 30){
    signal[i] = 1
  } else { 
    signal[i] = 0
  }
}

signal = reclass(signal,price)

trade = lag(signal)

count = c()
count[1:(n+1)] = 0

# 1000 units to be bought each time there is a buy signal
# all units sold each time there is a sell signal
# 0.3% commission charged per transaction
# no shortselling allowed
for (i in (n+2): length(price)){
  if (trade[i] == 1 && cash[i-1] >= 1000*price[i]*1.003){
    stock[i] = stock[i-1] + 1000
    cash[i] = cash[i-1] - 1000*price[i]*1.003
    count[i] = count[i-1]+1
  } else if (trade[i] == -1 && stock[i-1] > 0){
    stock[i] = 0
    cash[i] = cash[i-1] + stock[i-1]*price[i]*0.997
    count[i] = count[i-1]+1
  } else {
    stock[i] = stock[i-1]
    cash[i] = cash[i-1]
    count[i] = count[i-1]+1
  }
}
stock = reclass(stock,price)
cash = reclass(cash,price)
count = reclass(count, price)

equity  = c()
equity[1:(n+1)] = 1000000

return = c()
return[1:(n+1)] = 0

for (i in (n+1): length(price)){
  equity[i] = stock[i] * price[i] + cash[i]
  return[i] = equity[i]/equity[i-1]-1
}
equity = reclass(equity,price)
return = reclass(return,price)

library(PerformanceAnalytics)
charts.PerformanceSummary(return, main="Performance of SMA-RSI strategy")
chart_Series(equity, main="equity")
chart_Series(cash/1000000, main="cash")

tab.perf.out = table.Arbitrary(return,
                               metrics=c(
                                 "Return.cumulative",
                                 "Return.annualized",
                                 "SharpeRatio.annualized",
                                 "maxDrawdown"),
                               metricsNames=c(
                                 "Cumulative Return",
                                 "Annualized Return",
                                 "Annualized Sharpe Ratio",
                                 "Max Drawdown"))
tab.perf.out

# buy and hold
# assume to buy maximum number of units at the start
stock_b = c()
stock_b[1:(n+1)] = 0
stock_b[(n+1):length(price)] = as.integer(1000000/(price[n+1]*1.003))

cash_b = c()
cash_b[1:(n+1)] = 1000000
cash_b[(n+1):length(price)] = 1000000 - stock_b[n+1]*price[n+1]*1.003

stock_b = reclass(stock_b,price)
cash_b = reclass(cash_b,price)

equity_b  = c()
equity_b[1:(n+1)] = 1000000

return_b = c()
return_b[1:(n+1)] = 0

for (i in (n+1): length(price)){
  equity_b[i] = stock_b[i] * price[i] + cash_b[i]
  return_b[i] = equity_b[i]/equity_b[i-1]-1
}
equity_b = reclass(equity_b,price)
return_b = reclass(return_b,price)

tab.perf.b.out = table.Arbitrary(return_b,
                                 metrics=c(
                                   "Return.cumulative",
                                   "Return.annualized",
                                   "SharpeRatio.annualized",
                                   "maxDrawdown"),
                                 metricsNames=c(
                                   "Cumulative Return",
                                   "Annualized Return",
                                   "Annualized Sharpe Ratio",
                                   "Max Drawdown"))
tab.perf.b.out

compare = cbind(return, return_b)
charts.PerformanceSummary(compare, main="SMA-RSI vs Buy-Hold")
tab.perf.compare = table.Arbitrary(compare,
                                 metrics=c(
                                   "Return.cumulative",
                                   "Return.annualized",
                                   "SharpeRatio.annualized",
                                   "maxDrawdown"),
                                 metricsNames=c(
                                   "Cumulative Return",
                                   "Annualized Return",
                                   "Annualized Sharpe Ratio",
                                   "Max Drawdown"))
tab.perf.compare

##############################################################################
# how does strategy match up against buy and hold in the shorter term of 2 years?
shorter_term = SPY["2018-02-14/2020-02-14"]

signal_s = c()
signal_s[1:(n+1)] = 0

price_s = Cl(shorter_term)

stock_s = c()
stock_s[1:(n+1)] = 0

cash_s = c()
cash_s[1:(n+1)] = 1000000

rsi_s = RSI(price_s, n=14, maType = EMA)

sma50_s = SMA(price_s, n = 50)
sma200_s = SMA(price_s, n = 200)

for (i in (n+1): length(price_s)){
  if (sma50_s[i-1] < sma200_s[i-1] && sma50_s[i] > sma200_s[i]){ 
    signal_s[i] = 1
  } else if (sma50_s[i-1] > sma200_s[i-1] && sma50_s[i] < sma200_s[i]){
    signal_s[i] = -1
  } else if (sma50_s[i] > sma200_s[i] && rsi_s[i] < 30){
    signal_s[i] = 1
  } else { 
    signal_s[i] = 0
  }
}

signal_s = reclass(signal_s,price_s)

trade_s = lag(signal_s)

count_s = c()
count_s[1:(n+1)] = 0

# 1000 units to be bought each time there is a buy signal
# all units sold each time there is a sell signal
# 0.3% commission charged per transaction
# no shortselling allowed
for (i in (n+2): length(price_s)){
  if (trade_s[i] == 1 && cash_s[i-1] >= 1000*price_s[i]*1.003){
    stock_s[i] = stock_s[i-1] + 1000
    cash_s[i] = cash_s[i-1] - 1000*price_s[i]*1.003
    count_s[i] = count_s[i-1]+1
  } else if (trade_s[i] == -1 && stock_s[i-1] > 0){
    stock_s[i] = 0
    cash_s[i] = cash_s[i-1] + stock_s[i-1]*price_s[i]*0.997
    count_s[i] = count_s[i-1]+1
  } else {
    stock_s[i] = stock_s[i-1]
    cash_s[i] = cash_s[i-1]
    count_s[i] = count_s[i-1]+1
  }
}
stock_s = reclass(stock_s,price_s)
cash_s = reclass(cash_s,price_s)
count_s = reclass(count_s, price_s)

equity_s  = c()
equity_s[1:(n+1)] = 1000000

return_s = c()
return_s[1:(n+1)] = 0

for (i in (n+1): length(price_s)){
  equity_s[i] = stock_s[i] * price_s[i] + cash_s[i]
  return_s[i] = equity_s[i]/equity_s[i-1]-1
}
equity_s = reclass(equity_s,price_s)
return_s = reclass(return_s,price_s)

charts.PerformanceSummary(return_s, main="Performance of SMA-RSI strategy")
chart_Series(equity_s, main="equity")
chart_Series(cash_s/1000000, main="cash")

tab.perf.out_s = table.Arbitrary(return_s,
                               metrics=c(
                                 "Return.cumulative",
                                 "Return.annualized",
                                 "SharpeRatio.annualized",
                                 "maxDrawdown"),
                               metricsNames=c(
                                 "Cumulative Return",
                                 "Annualized Return",
                                 "Annualized Sharpe Ratio",
                                 "Max Drawdown"))
tab.perf.out_s

# buy and hold
# assume to buy maximum number of units at the start
stock_b_s = c()
stock_b_s[1:(n+1)] = 0
stock_b_s[(n+1):length(price_s)] = as.integer(1000000/(price_s[n+1]*1.003))

cash_b_s = c()
cash_b_s[1:(n+1)] = 1000000
cash_b_s[(n+1):length(price_s)] = 1000000 - stock_b_s[n+1]*price_s[n+1]*1.003

stock_b_s = reclass(stock_b_s,price_s)
cash_b_s = reclass(cash_b_s,price_s)

equity_b_s  = c()
equity_b_s[1:(n+1)] = 1000000

return_b_s = c()
return_b_s[1:(n+1)] = 0

for (i in (n+1): length(price_s)){
  equity_b_s[i] = stock_b_s[i] * price_s[i] + cash_b_s[i]
  return_b_s[i] = equity_b_s[i]/equity_b_s[i-1]-1
}
equity_b_s = reclass(equity_b_s,price_s)
return_b_s = reclass(return_b_s,price_s)

tab.perf.b.out_s = table.Arbitrary(return_b_s,
                                 metrics=c(
                                   "Return.cumulative",
                                   "Return.annualized",
                                   "SharpeRatio.annualized",
                                   "maxDrawdown"),
                                 metricsNames=c(
                                   "Cumulative Return",
                                   "Annualized Return",
                                   "Annualized Sharpe Ratio",
                                   "Max Drawdown"))
tab.perf.b.out_s

compare_s = cbind(return_s, return_b_s)
charts.PerformanceSummary(compare_s, main="SMA-RSI vs Buy-Hold")
tab.perf.compare_s = table.Arbitrary(compare_s,
                                   metrics=c(
                                     "Return.cumulative",
                                     "Return.annualized",
                                     "SharpeRatio.annualized",
                                     "maxDrawdown"),
                                   metricsNames=c(
                                     "Cumulative Return",
                                     "Annualized Return",
                                     "Annualized Sharpe Ratio",
                                     "Max Drawdown"))
tab.perf.compare_s

##############################################################################
# how does the strategy perform during the recession?
recession = SPY["2007-06-04/2009-12-30"]

signal_r = c()
signal_r[1:(n+1)] = 0

price_r = Cl(recession)

stock_r = c()
stock_r[1:(n+1)] = 0

cash_r = c()
cash_r[1:(n+1)] = 1000000

rsi_r = RSI(price_r, n=14, maType = EMA)

sma50_r = SMA(price_r, n = 50)
sma200_r = SMA(price_r, n = 200)

for (i in (n+1): length(price_r)){
  if (sma50_r[i-1] < sma200_r[i-1] && sma50_r[i] > sma200_r[i]){ 
    signal_r[i] = 1
  } else if (sma50_r[i-1] > sma200_r[i-1] && sma50_r[i] < sma200_r[i]){
    signal_r[i] = -1
  } else if (sma50_r[i] > sma200_r[i] && rsi_r[i] < 30){
    signal_r[i] = 1
  } else { 
    signal_r[i] = 0
  }
}

signal_r = reclass(signal_r,price_r)

trade_r = lag(signal_r)

count_r = c()
count_r[1:(n+1)] = 0

# 1000 units to be bought each time there is a buy signal
# all units sold each time there is a sell signal
# 0.3% commission charged per transaction
# no shortselling allowed
for (i in (n+2): length(price_r)){
  if (trade_r[i] == 1 && cash_r[i-1] >= 1000*price_r[i]*1.003){
    stock_r[i] = stock_r[i-1] + 1000
    cash_r[i] = cash_r[i-1] - 1000*price_r[i]*1.003
    count_r[i] = count_r[i-1]+1
  } else if (trade_r[i] == -1 && stock_r[i-1] > 0){
    stock_r[i] = 0
    cash_r[i] = cash_r[i-1] + stock_r[i-1]*price_r[i]*0.997
    count_r[i] = count_r[i-1]+1
  } else {
    stock_r[i] = stock_r[i-1]
    cash_r[i] = cash_r[i-1]
    count_r[i] = count_r[i-1]+1
  }
}
stock_r = reclass(stock_r,price_r)
cash_r = reclass(cash_r,price_r)
count_r = reclass(count_r, price_r)

equity_r  = c()
equity_r[1:(n+1)] = 1000000

return_r = c()
return_r[1:(n+1)] = 0

for (i in (n+1): length(price_r)){
  equity_r[i] = stock_r[i] * price_r[i] + cash_r[i]
  return_r[i] = equity_r[i]/equity_r[i-1]-1
}
equity_r = reclass(equity_r,price_r)
return_r = reclass(return_r,price_r)

charts.PerformanceSummary(return_r, main="Performance of SMA-RSI strategy")
chart_Series(equity_r, main="equity")
chart_Series(cash_r/1000000, main="cash")

tab.perf.out_r = table.Arbitrary(return_r,
                                 metrics=c(
                                   "Return.cumulative",
                                   "Return.annualized",
                                   "SharpeRatio.annualized",
                                   "maxDrawdown"),
                                 metricsNames=c(
                                   "Cumulative Return",
                                   "Annualized Return",
                                   "Annualized Sharpe Ratio",
                                   "Max Drawdown"))
tab.perf.out_r

# buy and hold
# assume to buy maximum number of units at the start
stock_b_r = c()
stock_b_r[1:(n+1)] = 0
stock_b_r[(n+1):length(price_r)] = as.integer(1000000/(price_r[n+1]*1.003))

cash_b_r = c()
cash_b_r[1:(n+1)] = 1000000
cash_b_r[(n+1):length(price_r)] = 1000000 - stock_b_r[n+1]*price_r[n+1]*1.003

stock_b_r = reclass(stock_b_r,price_r)
cash_b_r = reclass(cash_b_r,price_r)

equity_b_r  = c()
equity_b_r[1:(n+1)] = 1000000

return_b_r = c()
return_b_r[1:(n+1)] = 0

for (i in (n+1): length(price_r)){
  equity_b_r[i] = stock_b_r[i] * price_r[i] + cash_b_r[i]
  return_b_r[i] = equity_b_r[i]/equity_b_r[i-1]-1
}
equity_b_r = reclass(equity_b_r,price_r)
return_b_r = reclass(return_b_r,price_r)

tab.perf.b.out_r = table.Arbitrary(return_b_r,
                                   metrics=c(
                                     "Return.cumulative",
                                     "Return.annualized",
                                     "SharpeRatio.annualized",
                                     "maxDrawdown"),
                                   metricsNames=c(
                                     "Cumulative Return",
                                     "Annualized Return",
                                     "Annualized Sharpe Ratio",
                                     "Max Drawdown"))
tab.perf.b.out_r

compare_r = cbind(return_r, return_b_r)
charts.PerformanceSummary(compare_r, main="SMA-RSI vs Buy-Hold")
tab.perf.compare_r = table.Arbitrary(compare_r,
                                     metrics=c(
                                       "Return.cumulative",
                                       "Return.annualized",
                                       "SharpeRatio.annualized",
                                       "maxDrawdown"),
                                     metricsNames=c(
                                       "Cumulative Return",
                                       "Annualized Return",
                                       "Annualized Sharpe Ratio",
                                       "Max Drawdown"))
tab.perf.compare_r