#-----------------------------------------------------------------------------------------------------------------------
#
# Multi-Histogram plotter (side-by-side)
#
#-----------------------------------------------------------------------------------------------------------------------
# 
#****f* multihistAdjacentPlot
#
# NAME
# multiHistAdjacentPlot
# 
# AUTHOR
# Dr. Timothy Carlisle (d-fine)
# 
# CREATION DATE
# 02.07.2018
# 
# DESCRIPTION
# Plots histograms side-by-side on a single set of axes
# 
# SYNOPSIS
# multiHistAdjacentPlot(x, xLabel, yLabel, title, colours)
# 
# INPUTS
# x : a list of lists containing two datasets, each to be plotted as histograms
# xlab : x axis label
# ylab : y axis label
# axes : whether to draw axes (boolean, default=TRUE)
# main : Histogram Title
# col : Vector of colors for histograms (one color per dataset)
# legendTitle : Legend Title
# legendPosition : Either as text e.g. 'bottomright' or vector
# 
# RESULT
# One plot containing side-by-side histograms
#
#****
#-----------------------------------------------------------------------------------------------------------------------

library(plotrix)
 
multiHistAdjacentPlot <- function(x, beside=TRUE, plot=TRUE,
                                  xlim = NULL, ylim = NULL, main = NULL, axes = TRUE,
                                  xlab = NULL, ylab = NULL, col = c("green", "blue"),
                                  legendTitle = NULL, legendPosition = 'topright'){

  h1 <- barplot(x, beside=beside, xlim =range(0,x), plot=FALSE)
  #highestCount = max(h1[[2]])
  
  result <- barplot(x, beside=beside,  xlim =range(0,x), plot=plot,
                     space=c(0,1.0),xlab=xlab, axes=axes, ylab=ylab, main=main, col=col) 
  
  legend(legendPosition, legend=c("yes","no"), title=legendTitle, fill=col, par(xpd=TRUE), box.lty = 0)
  #box(which="plot", lty="solid")
  
  return(result)
  }