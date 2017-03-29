require(quantmod)

load("ige-data.RData")

computeReturns = function(prices, origin=6) {
    # origin controls the originating column index.
    # by default 6 will use the previous date adj close price
    (prices[, 6] - lag(prices[, origin], k=1)) / lag(prices[, 6], k=1) * 100
}

data = IGE['2002']
rets = computeReturns(data)
std = sd(rets, na.rm=T)
plot(index(data), rets, t = 'o', xlab = 'Time', ylab = 'Returns (%)', ylim=c(-4*std - 1,4*std + 1))
print(std)
abline(h=4*std, col='red')
abline(h=-4*std, col='red')
