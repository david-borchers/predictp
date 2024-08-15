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

## ----plotvis_withpos----------------------------------------------------------
plotdetfn(vismodel,eg_viscov_withpos[1,])

## ----plotvis------------------------------------------------------------------
plotdetfn(vismodel,eg_viscov[1,])

## ----plotvis_noCI-------------------------------------------------------------
plotdetfn(vismodel,eg_viscov_withpos[1,],addCI=FALSE)
plotdetfn(vismodel,eg_viscov[1,],addCI=FALSE)

## ----plotavail.CDF------------------------------------------------------------
data("eg_tagcov")
data("tagmodel")
depths = seq(0,3,length=21)
n = nrow(eg_tagcov)
CDF = matrix(rep(NA,n*length(depths)),nrow=n)
CDF0 = plotavail(depths,tagmodel=NULL,covdf=NULL,what="CDF",doplot=FALSE)
for(i in 1:n) {
  CDF[i,] = plotavail(depths,tagmodel=tagmodel,covdf=eg_tagcov[i,],what="CDF",doplot=FALSE)$F
}
ylim.F = range(CDF0$F0,CDF)
plot(depths,CDF0$F0,xlab="Depth (x)",ylab=expression(F(x)),ylim=ylim.F,type="l")
for(i in 1:n) lines(depths,CDF[i,],lty=2,col=i)

## ----plotavail.pdf------------------------------------------------------------
pdf = matrix(rep(NA,n*length(depths)),nrow=n)
pdf0 = plotavail(depths,tagmodel=NULL,covdf=NULL,what="PDF",doplot=FALSE)
for(i in 1:n) {
  pdf[i,] = plotavail(depths,tagmodel=tagmodel,covdf=eg_tagcov[i,],what="PDF",doplot=FALSE)$f
}
ylim.f = range(pdf0$f0,pdf)
plot(depths,pdf0$f0,xlab="Depth (x)",ylab=expression(f(x)),ylim=ylim.f,type="l")
for(i in 1:n) lines(depths,pdf[i,],lty=2,col=i)

