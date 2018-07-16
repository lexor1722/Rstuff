source(paste0(RatingStrLibraryDirectory,'/CalculateNumGiniCoefficients.R'))

#-----------------------------------------------------------------------------------------------------------------------
#
# d-fine's Bootstrapping for Gini routine. 
#
#-----------------------------------------------------------------------------------------------------------------------
#
#****f* 04_RatingValidation/RunGiniOnVars
# 
# NAME
# RunGiniOnVars
# 
# AUTHOR
# Dr. Robin Lamboll (d-fine)
# 
# CREATION DATE
# 20/6/2018
# 
# DESCRIPTION
# This bootstraps the existing data by resampling to make a larger data set and calculates the Gini coefficient of that larger dataset
# by assuming that the default probability is accurately assessed by external PDs. 
# 
# SYNOPSIS
# RunGiniOnVars(ExternalRating,DfAllMasterPD,AllData)
# 
# INPUTS
# ExternalRating:  external rating assessment of default probability
# DfAllMasterPD: The probability of default corresponding to this external rating
# LstInternalFactors: These are the factors which we want to calculate the Gini coefficient of
# IntBootstrapNum: How many times is the bootstraping calculation repeated? 
# NumPortfolioMultiplier: The size of the portfolio we bootstrap is this number times the original size. 
# StrPlaceToSave: Location where output is saved to. 

# 
# RESULT
# A list containing: 
#
# MtxGiniValuesMean: Mean Gini values for each category
# MtxGiniValuesSD: Standard deviation of the Gini
# MtxGiniValuesQuantiles: Quantiles of the Gini
# MtxCalibValsMean: The mean of the externally-predicted default rates for each category
# MtxCalibValsSD: Standard deviation of the externally-predicted default rates
# MtxCalibValsQuantiles: Quantiles for the externally-predicted default rates
# USES
# Backtesting
# 
#****
#-----------------------------------------------------------------------------------------------------------------------

RunGiniOnVars <- function(ExternalRating,DfAllMasterPD,LstInternalFactors,IntBootstrapNum=500,NumPortfolioMultiplier=1,StrPlaceToSave=NULL,
                          ){
  #Requires a file in the library - assume we are in the working directory, even if the file isn't. 
  
  #List of variables to calculate the Gini coefficient of. Must include an ExternalRating and an InternalRating. 
  DfGiniVars <- data.frame(ExternalRating,LstInternalFactors)
  
  
  #Define a function to perform a single iteration so that we can apply over this to find statistics on Gini coefs. 
  RunBootStrapIter <- function(DfGiniVars,DfAllMasterPD){
    DfSampledData <- DfGiniVars[sample(nrow(DfGiniVars), size=nrow(DfGiniVars)*NumPortfolioMultiplier, replace=TRUE),]
    
   #Create a model of defaults
    LstSimulatedDefaults <- runif(nrow(DfSampledData), min = 0, max = 1)<DfAllMasterPD[DfSampledData$ExternalRating]
    MtxGiniValues <-  apply(as.matrix(DfSampledData), 2, NumGiniCoefficient,defaults=LstSimulatedDefaults)
    #Calibration of internal ratings to default rates
    LstExtDefaultRates <- aggregate(LstSimulatedDefaults,by=list(DfSampledData$InternalRating),FUN=mean, na.rm=TRUE)
    
    return(list(MtxGiniValues,LstExtDefaultRates))
  }
  MtxGiniValues <- matrix(NA,IntBootstrapNum,length(DfGiniVars))
  MtxCalibVals <- matrix(NA,IntBootstrapNum,length(unique(DfGiniVars$InternalRating)))
  for (i in (1:IntBootstrapNum)){
    returnList <- RunBootStrapIter(DfGiniVars,DfAllMasterPD)
    MtxGiniValues[i,] <- returnList[[1]]
    MtxCalibVals[i,] <- t(returnList[[2]]$x)
  }
  
  #Outputs:
  MtxGiniValuesMean <- apply(MtxGiniValues,2,mean)
  CalibHeadings <- t(returnList[[2]]$Group.1)
  MtxCalibValsMean <- apply(MtxCalibVals,2,mean)
  #We must correct the standard deviation calculated by the square root of the inflation in the number of samples 
  MtxGiniValuesSD <- apply(MtxGiniValues,2,sd)*NumPortfolioMultiplier^0.5
  MtxCalibValsSD <- apply(MtxCalibVals,2,sd)*NumPortfolioMultiplier^0.5
  MtxGiniValuesQuantiles <- apply(MtxGiniValues,2,quantile,probs=c(0.05,0.25,0.5,0.75,0.95))
  MtxCalibValsQuantiles <- apply(MtxCalibVals,2,quantile,probs=c(0.05,0.25,0.5,0.75,0.95))
  
  OutputList <- list(MtxGiniValuesMean,MtxGiniValuesSD,MtxGiniValuesQuantiles,MtxCalibValsMean,MtxCalibValsSD,MtxCalibValsQuantiles)
  #Format this for the desired output. 
  if(!is.null(StrPlaceToSave)){
    lapply(OutputList, function(x) write.table( data.frame(x), StrPlaceToSave, append= T, sep=',' ))
  }
  
  return(OutputList)
  
}

#-----------------------------------------------------------------------------------------------------------------------
#
# d-fine's ROC plotting
#
#-----------------------------------------------------------------------------------------------------------------------
#
#****f* 04_RatingValidation/plotROC
# 
# NAME
# plotROC
# 
# AUTHOR
# Dr. Robin Lamboll (d-fine)
# 
# CREATION DATE
# 20/6/2018
# 
# DESCRIPTION
# This illustrates the Gini coefficient for a single variable by plotting the ROC curve. 
# The Gini coefficient is also calculated
# 
# SYNOPSIS
# plotROC(scores,defaults)
# 
# INPUTS
# scores:   a list of scores (the relevant performance factor), which must sort into the correct order 
#           (preferably use numbers). 
# defaults: whether or not that company defaults, binary. 
# 
# RESULT
# plots the ROC curve, returns the Gini coefficient. 
# USES
# 
# 
#****
#-----------------------------------------------------------------------------------------------------------------------
plotROC <- function(scores,defaults,StrPlaceToSave=NULL){
  LstScores <- as.matrix(sort(unique(scores),decreasing =TRUE))
  LstDefaultsByScore <- apply(LstScores,1,function(x) sum(defaults[scores==x]))
  LstTotalByScore <- apply(LstScores,1,function(x) sum(scores==x))
  
  # What cumulative fraction of results have defaulted yet? Starts at 0.
  LstFractionDefault <- c(0,1/sum(LstDefaultsByScore)*cumsum(LstDefaultsByScore))
  # What cumulative fraction of results have not defaulted yet? Starts at 0.
  LstFractionNotDefault <- c(0,1/sum(LstTotalByScore-LstDefaultsByScore)*cumsum(LstTotalByScore-LstDefaultsByScore))
  
  #The Gini coefficient is given by twice the difference between these and the 1:1 relationship
  NumGiniCoef <- -1 + sum((LstFractionDefault[-1] + LstFractionDefault[-length(LstFractionDefault)]) * 
  (LstFractionNotDefault[-1] - LstFractionNotDefault[-length(LstFractionNotDefault)]))
  
  #Plot the values
  xlab <- "False positives rate"
  ylab <- "True positives rate"
  fontFactor <- 1.4
  plot(LstFractionNotDefault,LstFractionDefault,type="n",main='ROC curve',xlab = xlab,ylab=ylab,cex.main=fontFactor,cex.axis=fontFactor,cex.lab=fontFactor)
  lines(LstFractionNotDefault,LstFractionDefault,type='S',lwd=2)
  abline(a=0,b=1,col="red",lwd=2)
  text(x=0.8,y=0.1,paste("Gini coefficient = ", round(NumGiniCoef, digits = 3)),cex=fontFactor)
  if(!is.null(StrPlaceToSave)){
    jpeg(StrPlaceToSave)
    plot(LstFractionNotDefault,LstFractionDefault,type="n",main='ROC curve',xlab = xlab,ylab=ylab,cex.main=fontFactor,cex.axis=fontFactor,cex.lab=fontFactor)
    lines(LstFractionNotDefault,LstFractionDefault,type='S',lwd=2)
    abline(a=0,b=1,col="red",lwd=2)
    dev.off()
  }
  return(NumGiniCoef)
}
