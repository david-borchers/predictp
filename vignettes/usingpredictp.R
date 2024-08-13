## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----vismodel-----------------------------------------------------------------
library(predictp)
data("vismodel")

## ----sumvismodel--------------------------------------------------------------
vismodel

## ----viscov-------------------------------------------------------------------
data("eg_viscov")
data("eg_viscov_withpos")
eg_viscov
eg_viscov_withpos

## ----cdfdataframe-------------------------------------------------------------
data("cdfdataframe")
cdfdataframe

## ----tagmodel-----------------------------------------------------------------
data("tagmodel")

## ----sumtagmodel--------------------------------------------------------------
tagmodel

## ----tagcov-------------------------------------------------------------------
data("eg_tagcov")
eg_tagcov

## ----psee, warning=FALSE------------------------------------------------------
p = psee(eg_viscov, eg_tagcov, vismodel, tagmodel)

## ----fig.show='hold'----------------------------------------------------------
# Make a smooth interpolating spline function for the CDF
depthcdf = splinefun(cdfdataframe$depth,cdfdataframe$cdf)
# ... and plot it:
x = seq(0,3,length=50)
plot(x,depthcdf(x),type="l",xlab="Depth (x)",ylab=expression({F^0}(x)),cex.lab=0.75)
points(cdfdataframe$depth,cdfdataframe$cdf)
plot(x,depthcdf(x,deriv=1),type="l",xlab="Depth (x)",ylab=expression({f^0}(x)),cex.lab=0.75)

## ----plot_withpos-------------------------------------------------------------
plotdetfn(vismodel,eg_viscov_withpos[1,])

## ----plot---------------------------------------------------------------------
plotdetfn(vismodel,eg_viscov[1,])

## ----plot_noCI----------------------------------------------------------------
plotdetfn(vismodel,eg_viscov_withpos[1,],addCI=FALSE)
plotdetfn(vismodel,eg_viscov[1,],addCI=FALSE)

