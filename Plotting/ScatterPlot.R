#-----------------------------------------------------------------------------------------------------------------------
#
# Scatter plotter
#
#-----------------------------------------------------------------------------------------------------------------------
# 
#****f* ScatterPlot
#
# NAME
# ScatterPlot
# 
# AUTHOR
# T. Carlisle (d-fine)
# 
# CREATION DATE
# 03.07.2018
# 
# DESCRIPTION
# Makes a basic scatter plot
# 
# SYNOPSIS
# ScatterPlot(c(1,3,6,8),c(1,3,6,9), xlim=range(0,10), ylim=range(0,12), xlab="Scores", type="l")
# 
# INPUTS
# x : the x coordinates of points in the plot, or a single plotting structure (x,y).
# y : the y coordinates of points in the plot, optional if x is an appropriate structure.
# type : type of plot e.g. "l" for line plot. Scatter plot is default.
# xlim : range of x axis  
# ylim : range of y axis
# xlab : x axis label
# ylab : y axis label
# main : title of plot
# sub : sub-title of plot (below x axis)
# axes : plot axes (boolean) default=TRUE
# cex : size of data points, default=1.0
# colours : colour of points/line etc.
# 
# RESULT
# One plot of the desired type 
#
# 
#****
#-----------------------------------------------------------------------------------------------------------------------

ScatterPlot <- function(x, y = NULL, type = "p",  xlim = NULL, ylim = NULL,
                          xlab = NULL, ylab = NULL, main = NULL, sub = NULL, 
                            axes = TRUE, cex=1.0, col = "black", log=log){
  
  result <- plot(x=x, y=y, type=type,  xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, main=main, 
                 sub=sub, axes=axes, cex=cex, col=col,log=log,pch=16)
  
  return (result)
}
