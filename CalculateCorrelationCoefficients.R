#-----------------------------------------------------------------------------------------------------------------------
#
# d-fine's analysis of correlations in data
#
#-----------------------------------------------------------------------------------------------------------------------
#
#****f* correlationAnalysis
# 
# NAME
# correlationAnalysis
# 
# AUTHOR
# Dr. Robin Lamboll (d-fine)
# 
# CREATION DATE
# 27/6/2018
# 
# DESCRIPTION
# Finds correlations between variables 
# 
# SYNOPSIS
# correlationResults(DfDataToCorrelate,LstHeadersToCorrelate)
# 
# INPUTS
# DfDataToCorrelate: a table of data, where you want to find correlations between different columns
# LstHeadersToCorrelate: if desired, the column headings (other columns will be ignored)
# 
# RESULT
#  Matrix of correlations with NULLs to cover unnecessary duplication. 
# USES
# 
# 
#****
#-----------------------------------------------------------------------------------------------------------------------

correlationResults <- function(DfDataToCorrelate,LstHeadersToCorrelate=NULL,StrPlaceToSave=NULL){
  
  if (!is.null(LstHeadersToCorrelate)){
    DfDataToCorrelate <- select(DfDataToCorrelate,LstHeadersToCorrelate)
  }
  
  #Perform correlation analysis
  MtxCorrelResult <- cor(DfDataToCorrelate,method="pearson")

  #Clear the lower triangle for ease of reading. 
  MtxFilteredTableOfCorels <- matrix(MtxCorrelResult, nrow=nrow(MtxCorrelResult))
  MtxFilteredTableOfCorels[!upper.tri(MtxCorrelResult)] <- list(NULL)
  MtxFilteredTableOfCorels <- matrix(MtxFilteredTableOfCorels,nrow=nrow(MtxCorrelResult))
  if (!is.null(StrPlaceToSave)){
    write.csv(MtxFilteredTableOfCorels,file=StrPlaceToSave, na = "",  row.names = FALSE)
  }
  
  return(MtxFilteredTableOfCorels)
  
}