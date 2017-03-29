require(quantmod)

load("ige-data.RData")

# obtain the numerical indexes for a date range
getDateRangeIdxs = function(prices, dateval) {
    dstr = paste("/", dateval, sep='') # get all prices up to this date
    sset = prices[dstr, which.i=T] # subset of indexes
    N = length(sset)
    sset[-N] # subset of indexes excluding the last one: ex-date
}

# compute the corrections for splits and dividends and compare with Yahoo's data
adjustPrices = function(prices, dividends, splits) {
    ret = as.numeric(prices[,4]) # this is the unadjusted close price
    multipliers = rep(1.0, length(ret)) # multiplier array

    ############## adjust for dividends ##############
    divIndex = index(dividends) # times when dividends were payed out

    for(i in seq(1, nrow(dividends))) {
        idxs = getDateRangeIdxs(prices, divIndex[i])

        # the multiplier here is the close price of the previous day of ex-date:
        closeprice = as.numeric(prices[idxs[length(idxs)], 4])
        mult = (closeprice - as.numeric(dividends[i])) / closeprice

        multipliers[idxs] = multipliers[idxs] * mult
    }

    ############## adjust for splits ##############
    splitIndex = index(splits)

    for(i in seq(1, nrow(splits))) {
        idxs = getDateRangeIdxs(prices, splitIndex[i])
        multipliers[idxs] = multipliers[idxs] * as.numeric(splits[i])
    }

    ret * multipliers
}

adj = adjustPrices(IGE, dividends, splits)
#print(all.equal(as.numeric(IGE[,6]), adj))
#print(as.numeric(IGE[,6] - adj))
#print(head(adj, 20))
#print(tail(adj, 20))
