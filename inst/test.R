data("eg_tagcov")
data("eg_viscov")
data("vismodel")
data("tagmodel")

p = psee11(eg_viscov,eg_tagcov,vismodel,tagmodel)

plt = plotdetfn(vismodel,eg_viscov)


# calculate approximate marginal probability, as a gross check that p above is right:
data("cdfdataframe") # get the CDF
depthcdf = splinefun(cdfdataframe$depth,cdfdataframe$cdf)
tagcov = eg_tagcov
tagcov$id = rep(0,nrow(tagcov))
pred2m = predict(tagmodel,type="response",se.fit=FALSE,newdata=tagcov)
x = plt$depth
nx = length(x)
fx = rep(pred2m,nx) * depthcdf(x,deriv=1) / depthcdf(2)
crudep = mean(plt$p*fx)*3
# % error:
100*(crudep - p)/p
# within 2%, which gives me confidence that p is correct

par(mfrow=c(1,2))
# plot smoothed CDF, together with values from cdfdataframe
depth = seq(0,3,length=100)
plot(depth,depthcdf(depth),type="l",xlab="Depth",ylab="CDF",main="Tielman et al. CDF smoothed",cex.main=0.75)
points(cdfdataframe$depth,cdfdataframe$cdf)
# and then the associated PDF
plot(depth,depthcdf(depth,deriv=1),type="l",xlab="Depth",ylab="PDF",main="Tielman et al. PDF smoothed",cex.main=0.75)
par(mfrow=c(1,1))


# Check help example code works
# -----------------------------
data("eg_tagcov")
data("eg_viscov")
data("eg_viscov_withpos")
data("vismodel")
data("tagmodel")
eg_viscov
p = psee1(eg_viscov,eg_tagcov,vismodel,tagmodel) # marginalising over pos
eg_viscov_withpos
p_withpos = psee1(eg_viscov_withpos,eg_tagcov,vismodel,tagmodel) # marginalising over pos
p;p_withpos


# Check some arbitrary cases
# -----------------------------
viscov1 = data.frame(pos="Centre",secchi=8,SS=0,cloud=0,hdglare="1")
tagcov1 = data.frame(periodfac="0",diy=110)
p1 = psee1(viscov1,tagcov1,vismodel,tagmodel)

viscov2 = data.frame(pos="Near",secchi=1,SS=2,cloud=2,hdglare="2")
tagcov2 = data.frame(periodfac="0",diy=110)
p2 = psee1(viscov2,tagcov2,vismodel,tagmodel)

viscov3 = data.frame(pos="Near",secchi=1,SS=2,cloud=2,hdglare="2")
tagcov3 = data.frame(periodfac="3",diy=20)
p3 = psee1(viscov3,tagcov3,vismodel,tagmodel)

p1;p2;p3


# Repeat arbitrary cases, but marginalising over pos
# --------------------------------------------------
viscov1m = data.frame(secchi=8,SS=0,cloud=0,hdglare="1")
tagcov1 = data.frame(periodfac="0",diy=110)
p1m = psee1(viscov1m,tagcov1,vismodel,tagmodel)

viscov2m = data.frame(secchi=1,SS=2,cloud=2,hdglare="2")
tagcov2 = data.frame(periodfac="0",diy=110)
p2m = psee1(viscov2m,tagcov2,vismodel,tagmodel)

viscov3m = data.frame(secchi=1,SS=2,cloud=2,hdglare="2")
tagcov3 = data.frame(periodfac="3",diy=20)
p3m = psee1(viscov3m,tagcov3,vismodel,tagmodel)

p1;p2;p3
p1m;p2m;p3m

testdf = data.frame(secchi=c(8,1,1),SS=c(0,2,3),cloud=c(0,2,4),hdglare=c("1","2","3"))
testdf

p = rep(NA,nrow(testdf))
for(i in 1:nrow(testdf)) {
  p[i] = psee1(testdf[i,],tagcov3,vismodel,tagmodel)
}

