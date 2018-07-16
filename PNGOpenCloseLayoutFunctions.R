#
#Contains a list of plotting functions that will be used in this project. 

#-----------------------------------------------------------------------------------------------------------------------
#
# PNG Open 
#
#-----------------------------------------------------------------------------------------------------------------------
#
#****f* Library/PNGOpen
# 
# NAME
# pngOpen
# 
# AUTHOR
# Dr Philipp Gerhold, Dr. Robin Lamboll (d-fine)
# 
# CREATION DATE
# 02/7/2018
# 
# DESCRIPTION
# Opens a png file and ensures that all print operations go here until the close png function is called. 
# 
# SYNOPSIS
# pngOpen(fileLocation=place to write the png file to)
# 
# INPUT 
# fileLocation: where the file will be saved. Includes the name of the file
# sx: optional, width of the png in cm
# sy: optional, height of the png in cm
# 
# RESULT
# no return
# USES
# 
# 
#****
#-----------------------------------------------------------------------------------------------------------------------
pngOpen <- function(fileLocation, sx=16.2, sy=14) {
  png(filename = fileLocation, width = sx, height = sy, units = "cm",res=200)
}

#-----------------------------------------------------------------------------------------------------------------------
#
# PNG Close 
#
#-----------------------------------------------------------------------------------------------------------------------
#
#****f* Library/PNGClose
# 
# NAME
# pngClose
# 
# AUTHOR
# Dr Philipp Gerhold, Dr. Robin Lamboll (d-fine)
# 
# CREATION DATE
# 02/7/2018
# 
# DESCRIPTION
# Closes the PNG file
# 
# SYNOPSIS
# pngOpen(fileLocation=place to write the png file to)
# 
# INPUT 
# (No input requried)
# 
# RESULT
# no return
# USES
# 
# 
#****
#-----------------------------------------------------------------------------------------------------------------------

pngClose <- function() {dev.off()}

