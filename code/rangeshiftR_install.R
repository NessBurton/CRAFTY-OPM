
# from instructions here:
# https://rangeshifter.github.io/RangeshiftR-tutorials/installing.html

install.packages("Rcpp")
install.packages("devtools")
install.packages("Rdpack")
# Error in inDL(x, as.logical(local), as.logical(now), ...) : unable to load shared object 'C:/Users/vanessa.burton.sb/Documents/R/win-library/4.0/xml2/libs/x64/xml2.dll':
# LoadLibrary failure:  This program is blocked by group policy. For more information, contact your system administrator.

library(Rcpp)
library(devtools)
library(Rdpack)

Rcpp::evalCpp("2+2")

devtools::install_github("https://github.com/RangeShifter/RangeShiftR-package", ref = "main")

library(RangeShiftR)