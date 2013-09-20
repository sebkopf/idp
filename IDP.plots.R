#######################################
# Code for isodat file parser (IDP)   #
# Plotting interface                  #
# Copyright 2013 Sebastian Kopf       #
# seb.kopf@gmail.com                  #
#######################################

#################
# Zoom Handlers #
#################

IDP.plotClickHandler<-function(idp, h) {
  if (identical(h$x[1], h$x[2]) && identical(h$y[1], h$y[2]))
    print("click") # don't do anything though
  else {
    fileInfo<-pn.getAllInfo(idp$gui$pn)
    
    # reverse transformation for coordinates
    xRevFunc <- tag(idp$gui$win, "settings")$plotOptions$xUnits$revFuncs[[tag(idp$gui$win, "settings")$plotOptions$xUnits$value]] 
    
    # see if we want to zoom in y or not outside the buffer zone
    if ( abs(diff(h$y))/diff(fileInfo$plotInfo$ylim[[length(fileInfo$plotInfo$ylim)]]) > tag(idp$gui$win, "settings")$plotOptions$zoomBuffer)
      ylim <- c(min(h$y), max(h$y))
    else
      ylim <- fileInfo$plotInfo$ylim[[length(fileInfo$plotInfo$ylim)]] # same as previous
    xlim <- c(min(do.call(xRevFunc, list(h$x))), max(do.call(xRevFunc, list(h$x))))
    
    idp$gui$bestfitActive['active']<-FALSE # no longer best fit
    fileInfo$plotInfo$ylim <- c(fileInfo$plotInfo$ylim, list(ylim))
    fileInfo$plotInfo$xlim <- c(fileInfo$plotInfo$xlim, list(xlim))
    IDP.plot(idp, fileInfo = fileInfo)
  }
  #cat("brush ", h$x, h$y, "\n")
}

IDP.plotRightClickHandler<-function(idp, h) {
  # reset zoom
  IDP.zoomReset(idp)
}

# reset zoom
IDP.zoomReset<-function(idp) {
  fileInfo<-pn.getAllInfo(idp$gui$pn)
  if (!is.null(fileInfo$plotInfo)) {
    idp$gui$bestfitActive['active']<-TRUE # back to best fit (FIXME, double fires, block handler first!)
    fileInfo$plotInfo$ylim <- fileInfo$plotInfo$ylim[1]
    fileInfo$plotInfo$xlim <- fileInfo$plotInfo$xlim[1]
    IDP.plot(idp, fileInfo = fileInfo)
  }
}

# zoom back one earlier level
IDP.zoomBack<-function(idp) {
  fileInfo<-pn.getAllInfo(idp$gui$pn)
  if (!is.null(fileInfo$plotInfo) && length(fileInfo$plotInfo$ylim) > 1) {
    fileInfo$plotInfo$ylim <- fileInfo$plotInfo$ylim[1:(length(fileInfo$plotInfo$ylim) - 1)]
    fileInfo$plotInfo$xlim <- fileInfo$plotInfo$xlim[1:(length(fileInfo$plotInfo$xlim) - 1)]
    IDP.plot(idp, fileInfo = fileInfo)    
  }
}

# best zom
IDP.zoomBest<-function(idp, xlim = NULL, zoomMin = TRUE, zoomMax = TRUE) {
  fileInfo<-pn.getAllInfo(idp$gui$pn)
  if (!is.null(fileInfo$plotInfo)) {
    if (is.null(xlim))
      xlim <- tail(fileInfo$plotInfo$xlim, 1)[[1]]
    ylim <- tail(fileInfo$plotInfo$ylim, 1)[[1]]
    bestYlim <- IDP.findSignalLimits(idp, data=fileInfo$data, xlim=xlim)# get best fit on this interval
  
    if (zoomMin)
      ylim[1] <- bestYlim[1]
    if (zoomMax)
      ylim[2] <- bestYlim[2]
      
    # store new positions
    fileInfo$plotInfo$xlim <- c(fileInfo$plotInfo$xlim, list(xlim))
    fileInfo$plotInfo$ylim <- c(fileInfo$plotInfo$ylim, list(ylim)) 
    IDP.plot(idp, fileInfo = fileInfo)
  }
}

# zoom in and out
IDP.zoomIn<-function(idp) {
  IDP.zoom(idp, tag(idp$gui$win, "settings")$plotOptions$zoomIn)
}

IDP.zoomOut<-function(idp) {
  IDP.zoom(idp, tag(idp$gui$win, "settings")$plotOptions$zoomOut)
}

IDP.zoom<-function(idp, zoom) {
  fileInfo<-pn.getAllInfo(idp$gui$pn)
  if (!is.null(fileInfo$plotInfo)) {
    idp$gui$bestfitActive['active']<-FALSE # no longer best fit
    ymin <- fileInfo$plotInfo$ylim[[length(fileInfo$plotInfo$ylim)]][1]
    ymax <- ymin + zoom*diff(fileInfo$plotInfo$ylim[[length(fileInfo$plotInfo$ylim)]])
    fileInfo$plotInfo$ylim <- c(fileInfo$plotInfo$ylim, list(c(ymin, ymax)))
    fileInfo$plotInfo$xlim <- c(fileInfo$plotInfo$xlim, fileInfo$plotInfo$xlim[length(fileInfo$plotInfo$xlim)])
    IDP.plot(idp, fileInfo = fileInfo)
  }
}

# zoom left and right
# move is either +1 or -1
IDP.zoomMove<-function(idp, move){
  fileInfo<-pn.getAllInfo(idp$gui$pn)
  if (!is.null(fileInfo$plotInfo)) {
    xlim <- tail(fileInfo$plotInfo$xlim, 1)[[1]]
    xlim <- xlim + move*tag(idp$gui$win, "settings")$plotOptions$zoomMove*diff(xlim)
    IDP.zoomBest(idp, xlim = xlim, zoomMin = idp$gui$bestfitActive['active'], zoomMax = idp$gui$bestfitActive['active']) 
  }
}

##################
# Data functions #
##################

# # find apex index of peak that belongs to the x coordinate (return NULL if no peak associated with this)
# CP.findPeak<-function(cpgui, x) {
#   if (!is.null(tag(cpgui$gobj, "data")$time)) {# time signal
#     xs<-tag(cpgui$gobj, "data")$time
#     peaks<-tag(cpgui$gobj,"peaks")[c("startRT","endRT", "PI")]
#   } else { # no time signal
#     xs<-1:nrow(tag(cpgui$gobj,"data")) 
#     peaks<-tag(cpgui$gobj,"peaks")[c("SI","EI","PI")]
#   }
#   return(peaks[which(peaks[[1]]<=x & peaks[[2]]>=x),"PI"])
# }
# 
# # find index from RT (or if index passed in, index is returned)
# CP.findIndexFromRT<-function(cpgui, RT) {
#   if (!is.null(tag(cpgui$gobj, "data")$time)) {# time signal
#     diff<-abs(tag(cpgui$gobj, "data")$time-RT)
#     return (which(diff==min(diff))) 
#   } else # no time signal
#     return (RT) # this is index
# }

# find the max and min in an interval (returns ylim = c(ymin, ymax))
# xlim, by default the last stack entry in the current plot tab
# data, by default the data in the current plot tab
IDP.findSignalLimits<-function (idp, xlim = NULL, data = NULL) {
  if (is.null(data))
    data <- pn.getAllInfo(idp$gui$pn)$data

  if (is.null(xlim))
    xlim <- tail(pn.getAllInfo(idp$gui$pn)$plotInfo$xlim, 1)[[1]]
  
  interval<-which(data$time>=xlim[1] & data$time<=xlim[2]) # indices interval
  
  # plot options
  plotOptions <- tag(idp$gui$win, "settings")$plotOptions 
  
  # mins and maxes
  ymin = c(min(data$mass2[interval]) + plotOptions$trace2$offset, min(data$mass3[interval]) + plotOptions$trace3$offset)
  ymax = c(max(data$mass2[interval]) + plotOptions$trace2$offset, max(data$mass3[interval]) + plotOptions$trace3$offset)
  
  if (plotOptions$trace2$on && !plotOptions$trace3$on)
    ylim <- c(ymin[1], ymax[1]) # just trace 2
  else if (!plotOptions$trace2$on && plotOptions$trace3$on)
    ylim <- c(ymin[2], ymax[2]) # trace 2 and trace 3
  else 
    ylim <- c(min(ymin), max(ymax)) # both
  
  return (ylim)
}

############
# Plotting #
############


# idp$settings$plotOptions<-list(
#   xUnits = list(value = 1, labels = c("s", "min"), funcs = c(function(x) x, function(x) x/60)),
#   yUnits = list(value = 1, labels = c("mV", "V"), funcs = c(function(x) x, function(x) x/1000)),
#   trace2 = list(on = TRUE, color="black", offset=200), #offset in mV
#   trace3 = list(on = TRUE, color="green", offset=0),
#   baseMarker = list(on = TRUE, color="red"),
#   apexMarker = list(on = TRUE, color="red"),
#   edgeMarker = list(on = TRUE, color="blue"))

IDP.plot <- function(idp, fileInfo = NULL, ...) {
  if (is.null(fileInfo))
    fileInfo<-pn.getAllInfo(idp$gui$pn) # by default plot current tab
  
  plotInfo <- fileInfo$plotInfo
  plotOptions <- tag(idp$gui$win, "settings")$plotOptions 
  
  # initial data constraints
  if (is.null(plotInfo)) {
    xlim <- c(min(fileInfo$data$time), max(fileInfo$data$time)) # initial xlim
    ylim <- IDP.findSignalLimits(idp, xlim = xlim, data = fileInfo$data) # initial ylim
    plotInfo <- list(
      xlim = list(xlim), # initial limits
      ylim = list(ylim)) # initial limits
  }
  
  # transformation functions
  xFuncs <- plotOptions$xUnits$funcs[[plotOptions$xUnits$value]] 
  #yFuncs <- plotOptions$yUnits$funcs[[plotOptions$yUnits$value]] # (just do mV for now)
  
  # plot
  plot(0,0, type="l", xlim=do.call(xFuncs, plotInfo$xlim[length(plotInfo$xlim)]), ylim=plotInfo$ylim[[length(plotInfo$ylim)]], 
       xlab=paste(plotOptions$labels$x, " [", plotOptions$xUnits$labels[[plotOptions$xUnits$value]], "]", sep=""), 
       ylab=paste(plotOptions$labels$y, " [", plotOptions$yUnits$labels[[plotOptions$yUnits$value]], "]", sep=""), ...)
  
  # FIXME - implement me
  #if (!is.null(peaks)) { # plot the peaks on top of the chromatogram
  #  if (peakDelimiters) { # plot the vertical lines
  #    abline(v=peaks$apexX, col="red", lty=2)
  #    abline(v=peaks$startX, col="blue", lty=2)
  #    abline(v=peaks$endX, col="blue", lty=2)
  #  }
  #  segments(peaks$startX,peaks$startY,peaks$endX,peaks$endY, col="red")
  #}
  
  # mass 2
  if (plotOptions$trace2$on)
    lines(do.call(xFuncs, list(fileInfo$data$time)), (fileInfo$data$mass2 + plotOptions$trace2$offset), col= plotOptions$trace2$color) 
  
  # mass 3
  if (plotOptions$trace2$on)
    lines(do.call(xFuncs, list(fileInfo$data$time)), (fileInfo$data$mass3 + plotOptions$trace3$offset), col= plotOptions$trace3$color) 
  
  # store plotting information
  pn.storeInfo(idp$gui$pn, list(plotInfo = plotInfo))
}

# plotting function for a chromatogram
# outputs in the currently active plotting device
# (can also be used indpendently from the user interface)
# ... arguments passed on to plot (e.g. main or other plotting parameters)
spectrum.plot<-function(x, y, peaks=NULL, xlim=NULL, ylim=NULL, xlab="x", ylab="y", peakDelimiters=TRUE, ...){
  if (is.null(xlim)) xlim<-c(min(x), max(x))
  if (is.null(ylim)) ylim<-c(min(y), max(y))
  plot(0,0, type="l", xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, ...)
  if (!is.null(peaks)) { # plot the peaks on top of the chromatogram
    if (peakDelimiters) { # plot the vertical lines
      abline(v=peaks$apexX, col="red", lty=2)
      abline(v=peaks$startX, col="blue", lty=2)
      abline(v=peaks$endX, col="blue", lty=2)
    }
    segments(peaks$startX,peaks$startY,peaks$endX,peaks$endY, col="red")
  }
  lines(x,y)
}




# General zoom handler for any zoomable plots in the notebook
# plotinfo of the plot needs defined
# - zoomable = TRUE
# - zoomHandler = plotting function that returns a plotinfo object
idp.zoomHandler<-function(plotsNotebook, h) {
  plotinfo<-plots.getInfo(plotsNotebook)
  if (!is.null(plotinfo$zoomable) && plotinfo$zoomable==TRUE) { # can zoom
    if (!is.null(plotinfo$clicked) && plotinfo$clicked==TRUE) {
      plotinfo$xlim<-c(min(plotinfo$x1, h$x), max(plotinfo$x1, h$x))
      plotinfo$ylim<-c(min(plotinfo$y1, h$y), max(plotinfo$y1, h$y))
      plotinfo$clicked<-FALSE
      
      # RUN zoom Handler, i.e. the appropriate plotting function
      plotinfo<-do.call(plotinfo$zoomHandler, args=list(plotinfo=plotinfo))
    } else {
      plotinfo$x1<-h$x
      plotinfo$y1<-h$y
      plotinfo$clicked<-TRUE
    }
    plots.storeInfo(plotsNotebook, plotinfo, reset=TRUE)
  }
}

# general unzoom handler for any zoomable plots in the notebook
# - zoomable = TRUE
# - zoomHandler = plotting function that returns a plotinfo object
# - xlimInit, ylimInit
idp.unzoomHandler<-function(plotsNotebook) {
  plotinfo<-plots.getInfo(plotsNotebook)
  if (!is.null(plotinfo$zoomable) && plotinfo$zoomable==TRUE) { # can zoom
    plotinfo$xlim<-plotinfo$xlimInit
    plotinfo$ylim<-plotinfo$ylimInit
    plotinfo<-do.call(plotinfo$zoomHandler, args=list(plotinfo=plotinfo))
    plots.storeInfo(plotsNotebook, plotinfo, reset=TRUE)
  }
}

# reset zoom handlers
idp.resetZoomHandler<-function(plotsNotebook){
  plotinfo<-plots.getInfo(plotsNotebook)
  if (!is.null(plotinfo$zoomable) && plotinfo$zoomable==TRUE) { # can zoom
    plotinfo$clicked<-FALSE
    plots.storeInfo(plotsNotebook, plotinfo, reset=TRUE)
  }
}