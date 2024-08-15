#' @title Make dataframe for visibility model prediction.
#'
#' @description
#'
#' Make dataframe with \code{depth} column containing \code{depths}, from
#' a single-row dataframe (\code{covdf}) that does not contain a \code{depth}
#' column.
#'
#' @param covdf A single-row data frame of explanatory variables for \code{model}.
#' @param depths The depths to appear in the \code{depth} column of the output
#' dataframes.
#'
#' @examples
#' data("eg_viscov")
#' data("vismodel")
#' p = plotdetfn(vismodel,eg_viscov)
#'
#' @returns
#' If the input dataframe has a column named \code{pos}, a single
#' dataframe is output, with \code{depths} in its \code{depth} column, and the
#' other columns being identical copies of teh single-row input dataframe
#' \code{covdf}.
#'
#' If the input dataframe does not a column named \code{pos}, then a list is
#' output, containing three dataframes like that for the case above, each with
#' a column \code{pos} that contains either "Centre", "Near" or "Far". These
#' dataframes are named $centre, $near and $far. This is so that we can predict
#' for each of these three levels of \code{pos} and then marginalise the
#' predictions over \code{pos}.
#'
#'@export makepredf
#'
makepredf = function(covdf,depths) {
  ndepths = length(depths)
  if(is.element("pos",names(covdf))) {
    predf = data.frame(
      depth = depths,
      pos=rep(covdf$pos,ndepths),
      secchi = rep(covdf$secchi,ndepths),
      SS = rep(covdf$SS,ndepths),
      cloud = rep(0,ndepths),
      hdglare = rep(covdf$hdglare,ndepths)
    )
    return(predf)
  }else {
    predf.Centre = data.frame(
      depth = depths,
      pos=rep("Centre",ndepths),
      secchi = rep(covdf$secchi,ndepths),
      SS = rep(covdf$SS,ndepths),
      cloud = rep(0,ndepths),
      hdglare = rep(covdf$hdglare,ndepths)
    )
    predf.Near = data.frame(
      depth = depths,
      pos=rep("Near",ndepths),
      secchi = rep(covdf$secchi,ndepths),
      SS = rep(covdf$SS,ndepths),
      cloud = rep(0,ndepths),
      hdglare = rep(covdf$hdglare,ndepths)
    )
    predf.Far = data.frame(
      depth = depths,
      pos=rep("Far",ndepths),
      secchi = rep(covdf$secchi,ndepths),
      SS = rep(covdf$SS,ndepths),
      cloud = rep(0,ndepths),
      hdglare = rep(covdf$hdglare,ndepths)
    )
    return(list(centre=predf.Centre,near=predf.Near,far=predf.Far))
  }
}

#' @title P(see|depth) at a range of depths.
#'
#' @description
#'
#'  Calculates p(see|depth) at a range of depths.
#'
#' @param model A gam object defining p(see|depth).
#' @param covdf A single-row data frame of explanatory variables for \code{model}.
#' @param ndepths The number of depths at which to calculate p(see|depth). Depths
#' are evenly spaced from 0 to \code{dmax}
#' @param dmax The maximum depth to consider
#' @param addCI If TRUE, lower and upper 95 percent confidence intervals are calculated.
#'
#' @examples
#' data("eg_viscov")
#' data("vismodel")
#' p = plotdetfn(vismodel,eg_viscov)
#'
#' @returns Returns a list containing the estimated detection probability at each
#' depth (\code{p}), the lower and upper 95% CI values at these depths (\code{lcl}
#' and \code{ucl}), and the depths at which the probabilities were calculated
#' (\code{depth).
#'
#' @export pseex
#'
pseex = function(model,covdf,ndepths=50,dmax=3,addCI=TRUE) {
  # first construct the prediction dataframe for all depths
  depths = seq(0,dmax,length=ndepths)
  # do this differently depending on whether "pos" is in covdf or not
  lcl = NA
  ucl = NA
  if(is.element("pos",names(covdf))) {
    predf = makepredf(covdf,depths)
    # then predict on the linear predictor scale
    predvis = predict(model,type="link",se.fit=TRUE,newdata=predf)
    # then extract the linear predictor and probability values
    est = predvis$fit
    if(addCI){
      lower = predvis$fit - 1.96*predvis$se.fit
      upper = predvis$fit + 1.96*predvis$se.fit
      lcl = plogis(lower)
      ucl = plogis(upper)
    }
    p = plogis(est)
  }else {
    if(addCI) warning("Variance and CI calculation when marginalising over `pos` is not yet implemented.")
    predlist = makepredf(covdf,depths)
    # then predict on the linear predictor scale. for each value of pos
    predvis.Centre = predict(model,type="link",se.fit=TRUE,newdata=predlist$centre)
    predvis.Near = predict(model,type="link",se.fit=TRUE,newdata=predlist$near)
    predvis.Far = predict(model,type="link",se.fit=TRUE,newdata=predlist$far)
    # then do a weighted sum over the probability for the pos levels (i.e., marginalise over pos)
    p.Centre = plogis(predvis.Centre$fit)
    p.Near = plogis(predvis.Near$fit)
    p.Far = plogis(predvis.Far$fit)
    p = p.Centre*1/5 + p.Near*2/5 + p.Far*2/5
  }

  # Finally return the estimated probability and upper- and lower- CIs
  return(list(p=p, lcl=lcl, ucl=ucl, depth=depths))
}



#' @title Plots a detection function.
#'
#' @description
#'
#'  Plots a detection function.
#'
#' @param model A gam object defining p(see|depth).
#' @param covdf A single-row data frame of explanatory variables for \code{model}.
#' @param ndepths The number of depths at which to calculate p(see|depth). Depths
#' are evenly spaced from 0 to \code{dmax}
#' @param dmax The maximum depth to consider
#' @param addCI If TRUE 95 percent CI lines are added, UNLESS \code{pseex} returns
#' NAs for $lcl and $ucl (which it currently does when marginalising over variable pos).
#' @param ... Arguments to \code{plot}, other than \code{ylim}, \code{xlab},
#' \code{ylab}, which are hardwired.
#'
#' @returns Invisibly returns a data frame with \code{depth}, \code{p}, \code{lcl}, \code{ucl}.
#'
#' @examples
#' data("eg_viscov")
#' data("vismodel")
#' plotdetfn(vismodel,eg_viscov)
#'
#' @export plotdetfn
#'
plotdetfn = function(model,covdf,ndepths=50,dmax=3,addCI=TRUE,...) {
  est = pseex(model,covdf,ndepths=ndepths,dmax=dmax,addCI=addCI)
  # then do the plotting
  if(addCI & !is.na(est$lcl[1]) & !is.na(est$ucl[1])) ylim = range(est$lcl,est$ucl)
  else ylim = range(0,est$p)
  plot(est$depth,est$p,type="l",ylim=ylim,xlab="Depth",ylab="p(see)",...)
  # add confidence intervals if
  if(!is.na(est$lcl[1]) & addCI) lines(est$depth,est$lcl,lty=2)
  if(!is.na(est$ucl[1]) & addCI) lines(est$depth,est$ucl,lty=2)

  txt = c(paste("pos: ",covdf$pos),paste("secchi: ",covdf$secchi),paste("SS: ",covdf$SS),
          paste("cloud: ",covdf$cloud),paste("hdglare: ",covdf$hdglare))
  legend("topright",legend=txt,bty="n",cex=0.75)

  invisible(data.frame(depth=est$depth, p=est$p, lcl=est$lcl, ucl=est$ucl))
}


#' @title Plots an availability function.
#'
#' @description
#'
#'  Plots an availability function.
#'
#' @param depths Depths at which to evaluate the availability function.
#' @param tagmodel A gam object defining p(depth<=2). If missing then interpolated
#' values of the CDF of Tielman et al. (2013) are returned.
#' @param covdf A single-row data frame of explanatory variables for \code{tagmodel}.
#' This is ignored if no \code{tagmodel} is passed.
#' @param what If "CDF" then the cumulative distribution function is plotted, else
#' probability density function is plotted.
#' @param doplot If TRUE, plotting is done, else only values are calculated (in
#' which case you need to use an assignment (see examples) to get hold of the
#' values), else you get nothing from the function.
#' @param ... Arguments to \code{plot}, other than \code{type}, \code{xlab},
#' \code{ylab}, which are hardwired.
#'
#' @returns Invisibly returns a data frame with columns \code{$depth}, and the
#' cdf or pdf (called \code{$F0), \code{$F), \code{$f0), or \code{$f),
#' depending what you asked for}
#'
#' @examples
#' data("eg_tagcov")
#' data("tagmodel")
#' depths = seq(0,3,length=21)
#' # To plot and print the cdf (F0) of Tielman et al. (2013):
#' par(mfrow=c(2,2))
#' F0 = plotavail(depths,tagmodel=NULL,covdf=NULL,what="CDF",type="l")
#' F0
#' # To plot and print the scaled cdf (F) for availability:
#' F = plotavail(depths,tagmodel=tagmodel,covdf=eg_tagcov[1,],what="CDF",type="l")
#' F
#' # To plot and print the pdf (f0) of Tielman et al. (2013):
#' f0 = plotavail(depths,tagmodel=NULL,covdf=NULL,what="PDF",type="l")
#' f0
#' # To plot and print the scaled pdf (f) for availability:
#' f = plotavail(depths,tagmodel=tagmodel,covdf=eg_tagcov[1,],what="PDF",type="l")
#' f
#' @export plotavail
#'
plotavail = function(depths,tagmodel=NULL,covdf=NULL,what="CDF",doplot=TRUE,...) {

  data("cdfdataframe")
  depthcdf = splinefun(cdfdataframe$depth,cdfdataframe$cdf)

  if(is.null(tagmodel)) {
    if(what=="CDF") {
      y = depthcdf(depths)
      if(doplot) plot(depths,depthcdf(depths),xlab="Depth (x)",ylab=expression({F^0}(x)),...)
      invisible(list(depth=depths, F0=y))
    }else {
      y = depthcdf(depths,deriv=1)
      if(doplot) plot(depths,y,xlab="Depth (x)",ylab=expression({f^0}(x)),...)
      invisible(list(depth=depths, f0=y))
    }
  }else {
    # Make sure that tagcov has a column with covariate "id" that is valid.
    # It is not used, so it does not matter what it is, it just needs to be
    # a valid level to avoid warning messages
    # (tagmodel$model$id contains the original $id data)
    covdf$id = rep(tagmodel$model$id[1],nrow(covdf))
    tagterms = predict(tagmodel,type="terms",se.fit=FALSE,newdata=covdf,terms=c("diy","periodfac","s(diy)"))
    taglp = as.numeric(attr(tagterms,"constant")) + sum(tagterms)
    pred2m = plogis(taglp)

    if(what=="CDF") {
      # calculate scaled CDF of x, scaling by prob(x<2) divided by Tielman's prob(x<2)
      # depthcdf(x) is Tielman's CDF(x) and depthcdf(x, deriv=1) is
      y = rep(pred2m,length(depths)) * depthcdf(depths) / depthcdf(2)
      if(doplot) plot(depths,y,xlab="Depth (x)",ylab=expression(F(x)),...)
      invisible(list(depth=depths, F=y))
    }else {
      # calculate scaled PDF of x, scaling by prob(x<2) divided by Tielman's prob(x<2)
      # depthcdf(x) is Tielman's CDF(x) and depthcdf(x, deriv=1) is
      y = rep(pred2m,length(depths)) * depthcdf(depths,deriv=1) / depthcdf(2)
      if(doplot) plot(depths,y,xlab="Depth (x)",ylab=expression(f(x)),...)
      invisible(data.frame(depth=depths, f=y))
    }
  }
}


#' @title Predict a single detection probability.
#'
#' @description
#'  Predicts detection probability when a dataframe with one row containing a
#'  single set of visibility covariates is input.
#'
#'  NB: The terms of \code{tagmodel} that are used for prediction are hardwired
#'  into this function, so passing a different \code{tagmod} will likely give
#'  incorrect predictions or make the function fall over.
#'
#' @param viscov A single-row data frame of explanatory variables for \code{vismodel}.
#' @param tagcov A single-row data frame of explanatory variables for \code{tagmodel}.
#' @param vismodel A gam object defining p(see|depth).
#' @param tagmodel A gam object defining p(depth<2).
#'
#' @examples
#' data("eg_tagcov")
#' data("eg_viscov")
#' data("eg_viscov_withpos")
#' data("vismodel")
#' data("tagmodel")
#' eg_viscov
#' p = psee1(eg_viscov[1,],eg_tagcov[1,], vismodel,tagmodel) # marginalising over pos
#' eg_viscov_withpos
#' p_withpos = psee1(eg_viscov_withpos[1,],eg_tagcov[1,],vismodel,tagmodel) # marginalising over pos
#' p;p_withpos
#'
#' @returns The marginal probability of detecting an animal.
#'
#' @export psee1
#'
psee1 = function(viscov,tagcov,vismodel,tagmodel) {

  # Create the depth CDF (and PDF)
  data("cdfdataframe") # get the CDF
  # Make a smooth interpolating spline function for the CDF
  depthcdf = splinefun(cdfdataframe$depth,cdfdataframe$cdf)

  # Make sure that tagcov has a column with covariate "id" that is valid.
  # It is not used, so it does not matter what it is, it just needs to be
  # a valid level to avoid warning messages
  # (tagmodel$model$id contains the original $id data)
  tagcov$id = rep(tagmodel$model$id[1],nrow(tagcov))

  # construct the function to be integrated:
  infun = function(x,vismodel,tagmodel,viscov,tagcov,depthcdf) {
#    # replicate viscov row as many times as the length of x
#    nx = length(x)
#    vcov = viscov[rep(seq_len(nrow(viscov)), each = nx), ]
#    # add depth (x) to vcov
#    vcov$depth = x
    # calculate p(see|x):
    if(is.element("pos",names(viscov))) {
      vcov = makepredf(viscov,x) # make a single dataframe, with specifierd pos level
      psee.x = predict(vismodel,type="response",se.fit=FALSE,newdata=vcov)
    }else{ # predict at each value of pos and then marginalise
      predlist = makepredf(viscov,x) # make 3 dataframes. one with each level of pos
      p.Centre = predict(vismodel,type="response",se.fit=FALSE,newdata=predlist$centre)
      p.Near = predict(vismodel,type="response",se.fit=FALSE,newdata=predlist$near)
      p.Far = predict(vismodel,type="response",se.fit=FALSE,newdata=predlist$far)
      psee.x = p.Centre*1/5 + p.Near*2/5 + p.Far*2/5
    }
    # calcuate prob(x<2), zero-ing out the random effects:
#    pred2m = predict(tagmodel,type="response",se.fit=FALSE,newdata=tagcov,newdata.guaranteed=FALSE)

    # HERE IS WHERE THE TERMS OF tagmodel ARE HARDWIRED:
    tagterms = predict(tagmodel,type="terms",se.fit=FALSE,newdata=tagcov,terms=c("diy","periodfac","s(diy)"))
    taglp = as.numeric(attr(tagterms,"constant")) + apply(tagterms,1,sum)
    pred2m = plogis(taglp)

    # calculate scaled PDF of x, scaling by prob(x<2) divided by Tielman's prob(x<2)
    # depthcdf(x) is Tielman's CDF(x) and depthcdf(x, deriv=1) is
    fx = rep(pred2m,length(x)) * depthcdf(x,deriv=1) / depthcdf(2)
    return(psee.x*fx)
  }

  # Do the integration:
  psee = integrate(infun,0,3, vismodel=vismodel, tagmodel=tagmodel,
                   viscov=viscov, tagcov=tagcov, depthcdf=depthcdf)

  return(psee$value)
}


#' @title Predict detection probabilities.
#'
#' @description
#'
#'  Predicts detection probability when a dataframe with one or more rows
#'  containing a sets of visibility covariates is input. The function just
#'  loops through each row calling \code{psee1} with each row.
#'
#'  NB: The terms of \code{tagmodel} that are used for prediction are hardwired
#'  into this function, so passing a different \code{tagmod} will likely give
#'  incorrect predictions or make the function fall over.

#'
#' @param viscov A data frame of explanatory variables for \code{vismodel}, each
#' row giving the covariates associated with one detection.
#' @param tagcov A data frame of explanatory variables for \code{tagmodel}, each
#' row giving the covariates associated with one detection (the same detection as
#' for the corresponding row of \code{viscov}.
#' @param vismodel A gam object defining p(see|depth).
#' @param tagmodel A gam object defining p(depth<2).
#'
#' @examples
#' data("eg_tagcov")
#' data("eg_viscov")
#' data("eg_viscov_withpos")
#' data("vismodel")
#' data("tagmodel")
#' eg_viscov
#' p = psee(eg_viscov,eg_tagcov, vismodel,tagmodel) # marginalising over pos
#' eg_viscov_withpos
#' p_withpos = psee(eg_viscov_withpos,eg_tagcov,vismodel,tagmodel) # marginalising over pos
#' p;p_withpos
#'
#' @returns A vector of marginal probabilities of detecting animals with visibility
#' covariates as specified by the rows of \code{viscov}.
#'
#' @export psee
#'
psee = function(viscov,tagcov,vismodel,tagmodel) {
  n = nrow(viscov)
  p = rep(NA,n)
  for(i in 1:n) {
    p[i] = psee1(viscov[i,],tagcov[i,],vismodel,tagmodel)
  }
  return(p)
}
