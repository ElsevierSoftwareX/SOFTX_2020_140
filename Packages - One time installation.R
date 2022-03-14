
# One-time R packages installation
# R users need to install the necessary libraries to be able to run any developed R-based software/codes. 
# Accordingly, the CMIP6-D&A users should run the following short R code **ONLY** once prior to the first 
# time running of the CMIP6-D&A:
  
# Packages: One time installation:

install.packages("ncdf4")
install.packages("RGtk2")
install.packages("rattle")
options(guiToolkit = "RGtk2")
install.packages("gWidgetsRGtk2", dep=TRUE)

library(ncdf4)
library(RGtk2)
library(rattle)
library(tcltk)
library(gWidgets)

