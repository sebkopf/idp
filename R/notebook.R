#################
# PLOT NOTBEOOK #
#################

#FOR TESTING PURPOSES
#win<-gwindow("blub")
#pn.GUI(gframe(cont=win, horizontal=FALSE), win)

# make GUI forplot notbook
# new plot objs = list() object defining what kind of parameters are on a plot object by default
# - the load handlers are just passed the currently selected plot object for doing whatever they want with it
# add event handlers to the plot as needed, currently supported: "droptarget", "Clicked", "Rightclick", "MouseMotion"
# --> pass like this plotEventHandlers=list(droptarget=fun, Clicked=fun)
pn.GUI<-function(container, window, newPlotObj=NULL, 
                 newPlotObjLoadHandler=NULL, plotObjLoadHandler=NULL, plotEventHandlers=list(),
                 enablePlotLabel=TRUE, enableMenuButtons=TRUE, startWithTab=TRUE){
  
  pn<-list() # plots notebook object
  pn$win<-window
  pn$enablePlotLabel<-enablePlotLabel
  
  # actions to interact with the plots
  #FIXME: figure out how to make keyboard accelerators work (should be key.accel="Control-n" and parent=win for gaction but always fails, not sure why)
  #NOTE: as of august 2013, the keyboard accelerators were not implemented for RGtk2
  pn$actions<-list(
    aNewPlot = list(label="New Plot", icon="gtk-page-setup", handler=function(...) pn.newPlotTab(pn, tabObj=newPlotObj, eventHandlers=plotEventHandlers, loadHandler=newPlotObjLoadHandler, label=paste("Plot", length(pn$plot.nb)+1, sep="")) ),
    aClosePlot = list(label="Close Plot", icon="gtk-cancel", handler=function(...) pn.deletePlotTab(pn, loadHandler=plotObjLoadHandler)), 
    aSavePlot = list(label="Save Plot", icon="gtk-save-as", handler=function(...) pn.savePlotGUI(pn, index=svalue(pn$plot.nb))), 
    aPrintPlot = list(label="Print Plot", icon="gtk-print", handler=function(...) pn.printPlot(pn, index=svalue(pn$plot.nb))), 
    aSaveAll = list(label="Save All", icon="gtk-harddisk", handler=function(...) pn.savePlotGUI(pn)))
  if (enableMenuButtons) {
    pn$buttons.grp<-ggroup(cont=container, horizontal=TRUE)
    addSpring(pn$buttons.grp)
    for (act in pn$actions)
      gbutton(action=gaction(act$label, icon=act$icon, handler=act$handler), cont=pn$buttons.grp)
  }
  
  # plots notebook
  pn$plot.nb <- gnotebook(cont=container, expand=TRUE)
  pn$plot.nb.changedHandler<-addHandlerChanged(pn$plot.nb, handler=function(h,...) pn.selectPlotTab(pn, h$pageno, loadHandler=plotObjLoadHandler))
  if (startWithTab)
    pn.newPlotTab(pn, tabObj=newPlotObj, label="Plot1", loadHandler=newPlotObjLoadHandler, eventHandlers=plotEventHandlers)
  
  return(pn)
}

# save handler
# save the plot with the provided index
# if none is provided, save all plots
pn.savePlotGUI<-function(pn, index=NULL){
  if (is.null(index)) { # save all plots
    f=gfile("Select the folder where to save all the plots.", type="selectdir", cont=pn$win)
  } else { # save index plot
    f=gfile("Select where to save this graph.", type="save", cont=pn$win, 
            initialfilename = paste(format(Sys.time(),format="%Y%m%d"),"_", names(pn$plot.nb)[index],".pdf", sep=""),
            filter = list("PDF Files" = list(patterns=c("*.pdf")), "All files" = list(patterns = c("*"))))
  }
  
  if (!is.na(f)){
    grp<-ggroup(cont=(w<-gwindow("Save plot as pdf", width=200, height=100, spacing=30)), horizontal=FALSE, expand=TRUE)
    dlggrp<-glayout(container=grp, spacing=10)
    dlggrp[1,1]<-glabel("Width [inches]:",con=dlggrp)
    dlggrp[1,2]<-(width <- gedit(8,container=dlggrp, coerce.with=as.numeric))
    
    dlggrp[2,1]<-glabel("Height [inches]:",con=dlggrp)
    dlggrp[2,2]<-(height <- gedit(6,container=dlggrp, coerce.with=as.numeric))
    
    #dlggrp[3,1]<-glabel("Unit:",con=dlggrp)
    #dlggrp[3,2]<-(units <- gcombobox(c("in","cm","mm"),container=dlggrp))
    
    gbutton("save", cont=grp, handler=function(h,...) {
      if (is.null(index)) { # save all
        for (i in 1:length(pn$plot.nb)) 
          pn.savePlot(pn, i, file.path(f, paste(format(Sys.time(),format="%Y%m%d"),"_", names(pn$plot.nb)[i],".pdf", sep="")), width=svalue(width), height=svalue(height))
      } else { # save just the current
        if (length(grep("\\.pdf$", f))==0) f<-paste(f,".pdf",sep="") # ensure .pdf ending
        pn.savePlot(pn, index, f, width=svalue(width), height=svalue(height))
      }
      pn.reactivatePlot(pn) # reactivate previously active plot
      dispose(w)
    })
  }
}

# save the plot with the given index
pn.savePlot<-function(pn, index, file, width=8, height=6) {
  pn.activatePlot(pn, index)
  dev.copy2pdf(file=file, width=width, height=height) # copy graph
}

# print the plot with the given index
pn.printPlot<-function(pn, index, width=8, height=6) {
  if (exists("win.print")) { # on windows, go print
    pn.activatePlot(pn, index)
    win.print(width=width, height=height) # launches print interface
    pn.activatePlot(pn, index) # reactivate graphics device
  } else 
    gmessage("Sorry, direct printing is not yet supported on Linux/MacOS.\nPlease save the plot as a pdf and print from there.")
}

# make new plot tab
# provide more detailed plot object if keeping other parametrs is desired
# add event handlers to the plot as needed, currently supported: "droptarget", "Clicked", "Changed", "Rightclick", "MouseMotion", "RightlickMousePopupmenu" 
# --> pass like this plotEventHandlers=list(droptarget=fun, clicked=fun)
pn.newPlotTab<-function(pn, tabObj=NULL, label="Plot", loadHandler=NULL, eventHandlers=list()) {
  # block handlers
  blockHandler(pn$plot.nb, pn$plot.nb.changedHandler)
  
  # make new tab
  grp<-ggroup(cont=pn$plot.nb, horizontal=FALSE, label=label)
  if (pn$enablePlotLabel)
    addHandlerKeystroke(gedit(label, cont=grp), handler=function(h,...) {pn.changePlotTabName(pn, svalue(h$obj))})
  gg<-ggraphics(cont=grp)
  blockHandler(obj=gg) # disable automatic 2nd mouse button popup handler (for save and copy)
  
  # event handlers
  if (!is.null(eventHandlers$droptarget))
    adddroptarget(gg, targetType="object", handler=eventHandlers$droptarget)
  if (!is.null(eventHandlers$Clicked))
    addHandlerClicked(gg, handler=eventHandlers$Clicked)
  if (!is.null(eventHandlers$Changed))
    addHandlerChanged(gg, handler=eventHandlers$Changed)
  if (!is.null(eventHandlers$Rightclick))
    addHandlerRightclick(gg, handler=eventHandlers$Rightclick)
  if (!is.null(eventHandlers$MouseMotion))
    addHandlerMouseMotion(gg, handler=eventHandlers$MouseMotion)
  if (!is.null(eventHandlers$RightlickMousePopupmenu))
    add3rdMousePopupmenu(obj=gg, menulist=eventHandlers$RightlickMousePopupmenu)
  
  # make new object
  if (is.null(tabObj))
    tabObj<-list() # new object
  tabObj$gg<-gg # store the graphics object
  
  if (length(pn$plot.nb) == 1) 
    tag(pn$plot.nb, "tabs")<-list()
  tag(pn$plot.nb, "tabs")[[length(pn$plot.nb)]]<-tabObj # add new object
  
  # load
  if (!is.null(loadHandler))
    do.call(loadHandler, list(obj=tabObj))
  
  # unblock handlers
  unblockHandler(pn$plot.nb, pn$plot.nb.changedHandler)
}

# change plot tab name (not stored in object)
pn.changePlotTabName<-function(pn, label) {
  names(pn$plot.nb)[svalue(pn$plot.nb)]<-label
}

# delete plot
# (deletes specific plot if index is passed in otherwise just the currently selected ones)
pn.deletePlotTab<-function(pn, index=NULL, loadHandler=NULL) {
  if (!is.null(index))
    svalue(pn$plot.nb)<-index
  else
    index<-svalue(pn$plot.nb)
  blockHandler(pn$plot.nb, pn$plot.nb.changedHandler) # block changed handler
  dispose(pn$plot.nb) #remove plot
  tag(pn$plot.nb, "tabs")[[index]]<-NULL # remove plot object
  unblockHandler(pn$plot.nb, pn$plot.nb.changedHandler) # unblock changed handler
  pn.selectPlotTab(pn, svalue(pn$plot.nb), loadHandler=loadHandler)
}

# set plot tab specifrically
pn.setPlotTab<-function(pn, plotI, loadHandler=NULL) {
  svalue(pn$plot.nb)<-plotI
  pn.selectPlotTab(pn, plotI, loadHandler=loadHandler)
}

#select plot
pn.selectPlotTab<-function(pn, plotI, loadHandler=NULL) {
  if (plotI<=length(tag(pn$plot.nb, "tabs"))) { # make sure this is not when adding a new plot #FIXME
    pn.activatePlot(pn, plotI)
    if (!is.null(loadHandler))
      do.call(loadHandler, list(obj=pn.getPlotTabParam(pn, plotI)))
  }
}

# activate graphics widget of a plot index
pn.activatePlot<-function(pn, plotI) {
  gg<-pn.getPlotTabParam(pn, plotI, params="gg") # set ggraphics visible
  visible(gg)<-TRUE
}

# reactivate currently selected graphics widget
pn.reactivatePlot<-function(pn) {
  pn.activatePlot(pn, svalue(pn$plot.nb))
}

#get plot properti(es) for a tab
#params as c("test", "test2")
pn.getPlotTabParam<-function(pn, index, params=NULL) {
  if (is.null(params))
    return (tag(pn$plot.nb, "tabs")[[index]])
  else if (length(params)==1)
    return (tag(pn$plot.nb, "tabs")[[index]][[params]])
  else
    return (tag(pn$plot.nb, "tabs")[[index]][params])
}

# get all plot tab objs
pn.getAllPlotTabObjs<-function(pn) return (tag(pn$plot.nb, "tabs"))

# get all plot tab names
pn.getAllPlotTabNames<-function(pn) return(names(pn$plot.nb))

# get them for the selected tab
pn.getSelectedPlotTabParam<-function(pn, params=NULL) return (pn.getPlotTabParam(pn, svalue(pn$plot.nb), params=params))

# set plot properti(es)
# params as list
pn.setPlotTabParam<-function(pn, index, params) {
  for (var in names(params))
    tag(pn$plot.nb, "tabs")[[index]][var]<-params[var]
}

# set them for the selected tab
pn.setSelectedPlotTabParam<-function(pn, params) pn.setPlotTabParam(pn, svalue(pn$plot.nb), params)

# utility function for storing user information within the current tab (with id "plotinfo")
# info = list of parameters
pn.storeInfo<-function(pn, info, reset=FALSE) {
  if (reset)
    plotinfo<-list()
  else
    plotinfo<-pn.getSelectedPlotTabParam(pn, params="plotinfo")
  for (name  in names(info))
    plotinfo[name]<-info[name]
  pn.setSelectedPlotTabParam(pn, list(plotinfo=plotinfo)) # save plot parameter
}

# utility function for retrieving all user information from the current tab
pn.getAllInfo<-function(pn) return(pn.getSelectedPlotTabParam(pn, params="plotinfo"))

# utility function for retrieving parts of the user information from the current tab
pn.getInfo<-function(pn, fields) return(pn.getAllInfo(pn)[fields])

