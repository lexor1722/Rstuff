

library(XLConnect)

xs <- readWorksheetFromFile("C:/Users/d80070/Documents/VTB/InputData/Copy of analysis_ratios_banks_internal_vs_external.xlsx", 
                            sheet="RESULT")

outputDir <- paste0("C:/Users/d80070/Documents/VTB/Output/")
###
var1 <- '1 - Loan Loss Reserves / Gross Loans'
delta1 <- as.vector((xs$bil_1_prod - xs$bil_1_dev) / xs$bil_1_dev)
h1 <- hist(delta1, breaks=70, xlab="", main="Loan loss reserves / Gross loans (PROD - DEV)/DEV", col="lightblue", xlim=c(-1,1))
dev.copy(png, paste0(outputDir,'1 - LLR & Gross Loans.png'))
dev.off()

x <- xs$bil_1_prod
y <- xs$bil_1_dev
xymin <- min(range(x), range(y) )
xymax <- max(range(x), range(y) )
which(y == max(y))
s1 <- plot(x, y, xlab="Prod", ylab="Dev", xlim=c(xymin,xymax), ylim=c(xymin,xymax), main=var1, pch=4)
dev.copy(png, paste0(outputDir,"sc_", '1 - LLR & Gross Loans.png'))
dev.off()
####
var3 <- '3 - Equity / Total Assets'

delta3 <- as.vector((xs$bil_3_prod - xs$bil_3_dev) / xs$bil_3_dev)
h3 <- hist(delta3, breaks=20, xlab="", main="Equity / Total assets (PROD - DEV)/DEV", col="lightblue")
dev.copy(png, paste0(outputDir,'3 - Equity & T.Assets.png'))
dev.off()


x <- xs$bil_3_prod
y <- xs$bil_3_dev
xymin <- min(range(x), range(y) )
xymax <- max(range(x), range(y) )

s3 <- plot(x, y, xlab="Prod", ylab="Dev", xlim=c(xymin,xymax), ylim=c(xymin,xymax), main=var3, pch=4)
dev.copy(png, paste0(outputDir,"sc_", '3 - Equity & T.Assets.png'))
dev.off()
which(y == max(y))
####
var7 <- '7 - Net interest Income / Total assets'
delta7 <- as.vector((xs$bil_7_prod - xs$bil_7_dev) / xs$bil_7_dev)
h7 <- hist(delta7, breaks=50, xlab="", main="Net interest Income / Total assets (PROD - DEV)/DEV", col="lightblue", xlim=c(-0.5,0.5))
dev.copy(png, paste0(outputDir,'7 - NII & T.Assets.png'))
dev.off()

x <- xs$bil_7_prod
y <- xs$bil_7_dev
xymin <- min(range(x), range(y) )
xymax <- max(range(x), range(y) )
which(y == max(y))
s7 <- plot(x, y, xlab="Prod", ylab="Dev", xlim=c(xymin,xymax), ylim=c(xymin,xymax), main=var7, pch=4)
dev.copy(png, paste0(outputDir,"sc_", '7 - NII & T.Assets.png'))
dev.off()

####
var10 <- '10 - Profit Before Tax / Equity'
delta10 <- as.vector((xs$bil_10_prod - xs$bil_10_dev) / xs$bil_10_dev)
h110 <- hist(delta10, breaks=50, xlab="", main="Profit before tax / Equity (PROD - DEV)/DEV", col="lightblue", xlim=c(-3.5,1))
dev.copy(png, paste0(outputDir,'10 - PBT & Equity.png'))
dev.off()

x <- xs$bil_10_prod
y <- xs$bil_10_dev
xymin <- min(range(x), range(y) )
xymax <- max(range(x), range(y) )
which(y == max(y))
s10 <- plot(x, y, xlab="Prod", ylab="Dev", xlim=c(xymin,xymax), ylim=c(xymin,xymax), main=var10, pch=4)
dev.copy(png, paste0(outputDir,"sc_", '10 - PBT & Equity.png'))
dev.off()
###

var12 <- '12 - Other Operating Income / Total Assets'
delta12 <- as.vector((xs$bil_12_prod - xs$bil_12_dev) / xs$bil_12_dev)
h12 <- hist(delta12, breaks=50, xlab="", main="Other operating income / Total assets (PROD - DEV)/DEV", col="lightblue", xlim=c(-2,1))
dev.copy(png, paste0(outputDir,'12 - OOI & T.Assets.png'))
dev.off()

x <- xs$bil_12_prod
y <- xs$bil_12_dev
xymin <- min(range(x), range(y) )
xymax <- max(range(x), range(y) )
which(y == max(y))
s12 <- plot(x, y, xlab="Prod", ylab="Dev", xlim=c(xymin,xymax), ylim=c(xymin,xymax), main=var12, pch=4)
dev.copy(png, paste0(outputDir,"sc_", '12 - OOI & T.Assets.png'))
dev.off()

###
var18 <- "18 - Int Income / Int Expense"
delta18 <- as.vector((xs$bil_18_prod - xs$bil_18_dev) / xs$bil_18_dev)
hist(delta18, breaks=50, xlab="", main="Interest income / Interest expense (PROD - DEV)/DEV", col="lightblue", xlim=c(-0.2,0.5))
dev.copy(png, paste0(outputDir,'18 - Interest income & Interest expense.png'))
dev.off()

x <- xs$bil_18_prod
y <- xs$bil_18_dev
xymin <- min(range(x), range(y) )
xymax <- max(range(x), range(y) )
which(y == max(y))
s18 <- plot(x, y, xlab="Prod", ylab="Dev", xlim=c(xymin,xymax), ylim=c(xymin,xymax), main=var18, pch=4)
dev.copy(png, paste0(outputDir,"sc_", '18 - Interest income & Interest expense.png'))
dev.off()

#total assets EUR
var19 <- "19 - Total Assets (EUR?)"
delta19 <- as.vector((xs$bil_19_prod - xs$bil_19_devEUR) / xs$bil_19_devEUR)
hist(delta19, breaks=50, xlab="", main="Total Assets (EUR?) (PROD - DEV)/DEV", col="lightblue")
dev.copy(png, paste0(outputDir,'19 - Total Assets.png'))
dev.off()

x <- xs$bil_19_prod
y <- xs$bil_19_devEUR
xymin <- min(range(x), range(y) )
xymax <- max(range(x), range(y) )

#remove outliers in dev samples
xymax <- 1e8
which(y == max(y))
s19 <- plot(x, y, xlab="Prod", ylab="Dev", xlim=c(xymin,xymax), ylim=c(xymin,xymax), main=var19, pch=4)
dev.copy(png, paste0(outputDir,"sc", '19 - Total Assets.png'))
dev.off()

