require(quantmod)
require(gdata)

createXTS = function(obj) {
    xts(obj[,2:7], order.by = as.Date(obj$Date))
}

I = read.xls("IGE.xls")
IGE = createXTS(I); rm(I)
S = read.xls("SPY.xls")
SPY = createXTS(S); rm(S)

computeDailyReturn = function(closePrices) {
    N = length(closePrices)
    (closePrices[2:N] - closePrices[1:(N-1)]) / closePrices[1:(N-1)]
}

# this is the sharpe ratio for a year, considering the number of trading days
# in a year. see the discussion on page 45 for the multiplicative square root
# in the calculation.
sharpeRatio = function(prices, annumRiskFreeRate = 0.04, tradingDays = 252) {
    # first compute the daily returns:
    adjclose = as.numeric(prices[,6])
    dailyret = computeDailyReturn(adjclose)

    # excess daily returns assuming the risk free rate  per annum and
    # trading days in a year:
    excessret = dailyret - annumRiskFreeRate / tradingDays
    sqrt(tradingDays) * mean(excessret) / sd(excessret) # sharpe ratio
}


hedgedSharpe = function(prices1, prices2, annumRiskFreeRate = 0.04, tradingDays = 252) {

    if (nrow(prices1) != nrow(prices2)) {
        stop("prices sets dont have matching number of rows!")
    }

    # first compute the daily returns:
    adjclose1 = as.numeric(prices1[,6]); adjclose2 = as.numeric(prices2[,6])
    dailyret1 = computeDailyReturn(adjclose1); dailyret2 = computeDailyReturn(adjclose2)

    netReturns = (dailyret1 - dailyret2) / 2.0
    sqrt(tradingDays) * mean(netReturns) / sd(netReturns) # sharpe ratio
}

#srLong = sharpeRatio(IGE) # this is a long strategy, of buying on 2001 and staying with it until 2007.
#srLongShortNeutral = hedgedSharpe(IGE, SPY) # this is a long short (neutral) strategy.

computeMaxDrawdownAndDuration = function(cumret) {
    N = length(cumret)

    highwatermark = rep(0, N)
    drawdown = rep(0, N)
    drawdownduration = rep(0, N)

    for (i in 2:N) {
        highwatermark[i] = max(highwatermark[i - 1], cumret[i])
        drawdown[i] = (1 + cumret[i]) / (1 + highwatermark[i]) - 1 # drawdown each day
        if (drawdown[i] == 0) {
            drawdownduration[i] = 0
        } else {
            drawdownduration[i] = drawdownduration[i - 1] + 1
        }
    }

    c(min(drawdown), max(drawdownduration))
}

maxDrawdown = function(prices1, prices2) {
    # calculate the high watermark - maximum cumulative return of
    # the strategy up to that time (the close of each day).
    # first compute the daily returns:
    adjclose1 = as.numeric(prices1[,6]); adjclose2 = as.numeric(prices2[,6])
    dailyret1 = computeDailyReturn(adjclose1); dailyret2 = computeDailyReturn(adjclose2)

    netReturns = (dailyret1 - dailyret2) / 2.0
    cumulativeCompoundedReturn = cumprod(1 + netReturns) - 1
    plot(cumulativeCompoundedReturn, type='l')
    computeMaxDrawdownAndDuration(cumulativeCompoundedReturn)
}

mds = maxDrawdown(IGE, SPY)
