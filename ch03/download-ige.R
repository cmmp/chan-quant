require(quantmod)

getSymbols.yahoo("IGE", from='2001-01-01', env = parent.frame())
dividends = getDividends("IGE")
splits = getSplits("IGE")

save(IGE, dividends, splits, file='ige-data.RData')
