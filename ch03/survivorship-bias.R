# these are sample portfolios mentioned on page 42.
# we simply compute their returns to show how survivorship bias can affect results.

avgreturn = function(xstart, xend) {
    mean((xend - xstart) / xstart) * 100
}

x = c(0.2188, 0.3125, 0.4063, 0.5, 0.6875, 0.7188, 0.75, 0.75, 0.75)
y = c(0.125, 0.49, 0.11, 0.33, 0.2, 0.8, 0.35, 0.17, 0.2188)

x2 = c(0.3125, 0.8438, 0.875, 0.875, 0.9583, 1.0156, 1.0625, 1.125, 1.2813, 1.3438)
y2 = c(0.49, 0.44, 27.9, 0.05, 2.5, 3.0688, 0.81, 0.88, 9.475, 0.25)

cat("Average return for portfolio without survivorship bias: ", avgreturn(x, y), "%\n", sep = '')
cat("Average return for portfolio with survivorship bias: ", avgreturn(x2, y2), "%\n", sep = '')

