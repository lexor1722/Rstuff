#-----------------------------------------------------------------------------------------------------------------------
#
# Analyze Validation Samples
#
#-----------------------------------------------------------------------------------------------------------------------
# 
#****f* 
#
# NAME
# processUnivariateData
# 
# AUTHOR
# T. Carlisle (d-fine)
# 
# CREATION DATE
# 09.07.2018
# 
# DESCRIPTION 
# 
#
# SYNOPSIS
# 
# INPUTS
#
# RESULT
# Histograms generated for each column and saved to .png.
# Summary() information saved to .txt also.
#
#****
#-----------------------------------------------------------------------------------------------------------------------

source('11_Library/ProcessUnivariateData.R')
source('11_Library/PNGOpenCloseLayoutFunctions.R')
  
df <- readRDS(paste0(StrImportDirectory,"/02_ValidationSamples/01_Output/", "prod_sample_banks.rds"))

riskFactors = c("NETII_TOTALASSETS"	,"PROFIT_EQUITY","TOTALASSETS")
logRiskFactors = c("LLRES_GROSSL","EQUITY_TOTALASSETS", "OTHEROPINC_TOTALASSETS",
               "INTINC_INTEXP")#,	"TOTALASSETS")

outputDir <- paste0(StrRatingValidationDirectory,"/01_ValidationSamples/01_Output/")

# write(paste0(outputDir,"prod_sample_banks", '.png')) 
# write()
# pngClose()



 #write histograms for all specified columns
for (riskFactor in riskFactors) {

  dev.new()

   filepath <- paste0(outputDir, riskFactor)
   #dev.print(png(paste0(filepath, '.png'))
   pngOpen(paste0(filepath, '.png'))
   
   hist(df[,riskFactor],
        main=riskFactor, xlab="", breaks=20, plot = TRUE, col="blue")
   pngClose()
    dev.off

   #quantile information
   q <- quantile(df[,riskFactor],  probs = c(2, 5, 10, 25, 50,75,90, 95, 98, NA)/100, na.rm = TRUE)
   s <- summary(df[,riskFactor])
   plotStatistics <- cbind(t(q), t(s))
   write.csv(plotStatistics, paste0(filepath, ".csv"))
}

#write histograms for all specified columns
for (logRiskFactor in logRiskFactors) {
  
  dev.new()
  
  filepath <- paste0(outputDir, logRiskFactor)
  #dev.print(png(paste0(filepath, '.png'))
  pngOpen(paste0(filepath, '.png'))
  
  hist(log(df[,logRiskFactor]),
       main=paste0("log(",logRiskFactor,")"), xlab="", breaks=20, plot = TRUE, col="blue")
  pngClose()
  dev.off
  
  #Get quantile & summary info
  q <- quantile(df[,logRiskFactor],  probs = c(2, 5, 10, 25, 50,75,90, 95, 98, NA)/100, na.rm = TRUE)
  s <- summary(df[,logRiskFactor])
  plotStatistics <- cbind(t(q), t(s))
  write.csv(plotStatistics, paste0(filepath, ".csv"))
}
graphics.off()