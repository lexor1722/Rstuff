
hist(df$NETII_TOTALASSETS, breaks=20, plot = TRUE)
quantile(df$NETII_TOTALASSETS,  probs = c(2, 5, 10, 25, 50,75,90, 95, 98, NA)/100, na.rm = TRUE)

h2 <- hist(df$PROFIT_EQUITY, breaks=15, plot = FALSE)
quantile(df$PROFIT_EQUITY,  probs = c(2, 5, 10, 25, 50,75,90, 95, 98, NA)/100, na.rm = TRUE)

h3 <- hist(log(df$LLRES_GROSSL), breaks=15, plot = FALSE)
quantile(log(df$LLRES_GROSSL),  probs = c(2, 5, 10, 25, 50,75,90, 95, 98, NA)/100, na.rm = TRUE)

h4 <- hist(log(df$EQUITY_TOTALASSETS), breaks=15, plot = FALSE)
quantile(log(df$EQUITY_TOTALASSETS),  probs = c(2, 5, 10, 25, 50,75,90, 95, 98, NA)/100, na.rm = TRUE)

h5 <- hist(df$OTHEROPINC_TOTALASSETS, breaks=15, plot = FALSE)
quantile(log(df$OTHEROPINC_TOTALASSETS),  probs = c(2, 5, 10, 25, 50,75,90, 95, 98, NA)/100, na.rm = TRUE)

h6 <- hist(log(df$INTINC_INTEXP), breaks=15, plot = FALSE)
quantile(log(df$INTINC_INTEXP),  probs = c(2, 5, 10, 25, 50,75,90, 95, 98, NA)/100, na.rm = TRUE)

h7 <- hist(log(df$TOTALASSETS), breaks=15, plot = FALSE)
quantile(log(df$TOTALASSETS),  probs = c(2, 5, 10, 25, 50,75,90, 95, 98, NA)/100, na.rm = TRUE)
