#############
# Aux Plots #
#############

IDP.plotRefs <- function(idp, peakTable = NULL) {
  if (is.null(peakTable))
    peakTable<-pn.getAllInfo(idp$gui$pn)$peakTable
  
  # activate refs plot
  visible(idp$gui$info.graph)<-TRUE
  if (!is.null(peakTable)) {
    refsTable<-subset(peakTable, RefPeak == TRUE, select=c("PeakNr", "Rt", "rR3H2v2H2"))
    refsTable$dDvariation <- iso.RtoDx(refsTable$rR3H2v2H2, mean(refsTable$rR3H2v2H2))
    refsTable$seq<-1:nrow(refsTable)
    p<-ggplot(refsTable, aes(seq, dDvariation)) + geom_point(size=3, shape=21, fill="gray", color="black") + 
      scale_x_continuous(breaks=refsTable$seq, labels=paste("Peak Nr:", refsTable$PeakNr, "\nRT:", refsTable$Rt)) +
      labs(y="dD [permil] vs mean R", x="") + theme_bw() + 
      theme(legend.position="bottom", axis.text.x = element_text(angle = 60, hjust = 1)) 
    print(p)
  } else {
    plot.new()
    text(0.4, 0.5, labels="No peak table.")
  }
}

#################
# Zoom Handlers #
#################

IDP.plotClickHandler<-function(idp, h) {
  if (identical(h$x[1], h$x[2]) && identical(h$y[1], h$y[2])) {
    if (IDP.getSettings(idp, "mode")=="ModeInfo") {
      # reverse transformation for coordinates
      xRevFunc <- IDP.getSettings(idp, "plotOptions")$xUnits$revFuncs[[IDP.getSettings(idp, "plotOptions")$xUnits$value]] 
      x <- do.call(xRevFunc, list(h$x[1]))
      
      # find closest peak
      peakTable<-pn.getAllInfo(idp$gui$pn)$peakTable
      if (!is.null(peakTable) && !is.empty(index<-which(peakTable$Start<=x & peakTable$End>=x))) {
        peakNr <- peakTable[index,"PeakNr"]
        svalue(tag(idp$gui$win, "dataTable"), index=TRUE)<-which(tag(idp$gui$win, "dataTable")[]["Peak Nr.\n"]==peakNr)
        IDP.showInfo(idp, paste("Peak #", peakNr, " selected. RT = ", 
                                peakTable[index, "Rt"], " s. Amp2 = ", peakTable[index, "Amp2"], " mV. dD (vs SMOW) = ", 
                                peakTable[index, "d2H1H"], " permil.", sep=""), timer=3, okButton=FALSE)
      }
    }
  } else {
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
  
  pn.reactivatePlot(idp$gui$pn)
  plotInfo <- fileInfo$plotInfo
  plotOptions <- IDP.getSettings(idp, "plotOptions")
  
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
  
  # labels
  if (plotOptions$markRefs) {
    refPeaks<-subset(fileInfo$peakTable[,c("Rt", "RefPeak", "Amp2", "BGD2")], RefPeak==TRUE)
    if (nrow(refPeaks) > 0) {
      refPeaks$signal<-refPeaks$Amp2 + refPeaks$BGD2 + plotOptions$trace2$offset
      text(do.call(xFuncs, list(refPeaks$Rt)), refPeaks$signal, labels="*", col="black", srt=90, adj=c(-0.5,0.65), cex=0.9)
    }
  }
  
  # peak delimiters
  if (nrow(fileInfo$peakTable) > 0 ) { # plot the peaks on top of the chromatogram
    if (plotOptions$apexMarker$on)
      abline(v = do.call(xFuncs, list(fileInfo$peakTable$Rt)), col = plotOptions$apexMarker$color, lty=2)
    if (plotOptions$edgeMarker$on) {
      abline(v = do.call(xFuncs, list(fileInfo$peakTable$Start)), col = plotOptions$edgeMarker$color, lty=2)
      abline(v = do.call(xFuncs, list(fileInfo$peakTable$End)), col= plotOptions$edgeMarker$color, lty=2)
    }
    #implement me: baseline
    #segments(peaks$startX,peaks$startY,peaks$endX,peaks$endY, col="red")
  }

  # store plotting information
  pn.storeInfo(idp$gui$pn, list(plotInfo = plotInfo))
}

