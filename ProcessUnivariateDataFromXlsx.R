#-----------------------------------------------------------------------------------------------------------------------
#
# Process Univariate Data from xlsx
#
#-----------------------------------------------------------------------------------------------------------------------
# 
#****f* processUnivariateDataFromXlsx
#
# NAME
# processUnivariateDataFromXlsx
# 
# AUTHOR
# T. Carlisle (d-fine)
# 
# CREATION DATE
# 09.07.2018
# 
# DESCRIPTION 
# Reads in a .xlsx workbook and generates histograms for each column in the specified 
# worksheet, and saved to file(s). The summary() function is also run on all columns, 
# saved as a textfile.
#
# SYNOPSIS
# processUnivariateDataFromXlsx(file, sheetName, outputDir)
# 
# INPUTS
# file : xlsx Workbook
# sheet : sheet of workbook containing the columns
# outputDir : where to save the output, default is the working directory.
#
# RESULT
# Histograms generated for each column and saved to .png.
# Summary() information saved to .txt also.
#
#****
#-----------------------------------------------------------------------------------------------------------------------

library("XLConnect")
source("11_Library/PNGOpenCloseLayoutFunctions.R")

processUnivariateDataFromXlsx <- function(file, sheet,outputDir=NULL){

  
  if(!is.null(outputDir)){
    outputDir <- file.path(outputDir,"\\")
  }
  
  xs <- readWorksheetFromFile(file=file, sheet=sheet)
  
  summary <- summary(xs)
  
  file <- paste0(tools::file_path_sans_ext(file),".txt")
  
  #write summary to .txt
  write(summary, paste0(tools::file_path_sans_ext(file),".txt"), sep="\t")
  
  for (colname in names(xs)) {
    
    dev.new()
    
    hist(xs[,which(names(xs)==colname)], 
         main=colname, xlab="")
    
    filepath <- paste0(outputDir, colname)
    pngOpen(paste0(filepath, '.png')) 
    
    quantiles <- quantile(xs,  probs = c(2, 5, 10, 25, 50,75,90, 95, 98, NA)/100, na.rm = TRUE)
    filepath <- paste0(outputDir, colname)
    write(quantiles, paste0(filepath, ".txt"))
  }
}
