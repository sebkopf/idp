##################################
# PACKAGED Isodat File Processor #
# 2013-09-22 by SKOPF           #
##################################




#######################################
# Code for isodat file parser (IDP)   #
# Main file (launch, loading, saving) #
# Copyright 2013 Sebastian Kopf       #
# seb.kopf@gmail.com                  #
#######################################

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this program and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

# launch the program
IDP<-function() {
  IDP.source(dir="/Users/SKOPF/Dropbox/Tools/software/R/idp", sourcefile="IDP.R")
  IDP.start(load = "idp")
  return()
}

# launch the program in development mode (repackages)
IDP.dev<-function() {
  IDP.package()
  IDP.source(dir="/Users/SKOPF/Dropbox/Tools/software/R/idp", sourcefile="IDP.R")
  obj <- IDP.start(load = "idpdev", askReload = FALSE)
  return(obj)
}

###########################
# Change program settings #
###########################

# change settings on the idp object
IDP.changeSettings<-function(idp, settings) {
  #FIXME - implement me
  return (idp)
}

####################################
# Source, start and initialization #
####################################

# requried libraries and sourcing source files
IDP.source <- function(dir, sourcefile) {
  # required libraries
  library(psych)
  library(gWidgets)
  library(ggplot2)
  library(gridExtra)
  library(reshape2)
  library(plyr)
  library(scales)
  library(stringr)
  library(RGtk2)
  
  # gui toolkit
  options("guiToolkit"="RGtk2")
  
  # make sure the right id function (from the widgets package) is always used
  id <- gWidgets::id
  
  # sourcing program file
  source(file.path(dir, sourcefile))
}

# start the program
# [idp] = previous instance of the program OR name of the global variable to look for
IDP.start<-function(load = NULL, askReload = TRUE) {
  # initialization
  if (identical(class(load), "list"))
    idp <- load # an existing instance was passed along (FIXME: do more checking here?)
  else if (identical(class(load), "character") && 
             exists(load, envir=.GlobalEnv) && 
             (askReload == FALSE || gconfirm("A previous instance of the Isodat Processor is saved in this workspace. Would you like to load it?"))) {
    idp <- get(load, envir=.GlobalEnv) # load from a global variable
  } else
    idp <- IDP.init() # initialize a new instance
  
  # update gvar setting if one is given
  if (identical(class(load), "character"))
    idp$settings$gvar <- load
    
  # update object instance  
  idp <- IDP.update(idp) # update instance
  
  # launch gui
  idp <- IDP.gui(idp)
  
  return (idp)
}

# initialize an idp instance
IDP.init<-function() {
  idp<-list(
    settings = list(),
    gui = list(),
    data = list())
  
  idp$settings$version <- 0.1 # version number
  idp$settings$gvar <- "idp" # global variable name
  idp$settings$options <- list( # which options are available
    saveToWorkspace = TRUE) # button to save IDP to workspace
  idp$settings$mode <- "ModeInfo" # options: ModeInfo, ModeAdd, ModeEdit, ModeDel, ModeStds
  idp$settings$fileDirectory <- getwd() # which directory to start the file browser in
  idp$settings$fileBrowserWidth <- 300 # starting width of file browser
  
  # plotting options
  idp$settings$plotOptions<-list(
    labels = list(x="Time", y="Signal"), 
    xUnits = list(value = 1, ids = c("XaxisSec", "XaxisMin"), labels = c("s", "min"), funcs = c(function(x) x, function(x) x/60), revFuncs = c(function(x) x, function(x) x*60)),
    yUnits = list(value = 1, ids = c("YaxismV", "YaxisV"), labels = c("mV", "V"), funcs = c(function(x) x, function(x) x/1000)), # y units not currently implemented
    trace2 = list(on = TRUE, color="black", offset=200), #offset in mV
    trace3 = list(on = TRUE, color="dark green", offset=0),
    baseMarker = list(on = TRUE, color="red"),
    apexMarker = list(on = TRUE, color="red"),
    edgeMarker = list(on = TRUE, color="blue"),
    zoomBuffer = 0.05,  # when it will be considered to be a y zoom or just x zoom (currently 5%)
    zoomIn = 0.5, # how much to zoom in (50%)
    zoomOut = 2, # how much to zoom out (100%)
    zoomMove = 0.2 # how much to move interval (20%) 
  )
  
  # FIXME: implement ordering here
  idp$settings$peakTableColumns<-data.frame(
    Column=c("Filename" ,"Peak Nr." ,"Component" ,"Master Peak" ,"Ref. Name" ,"Start" ,"Rt" ,"End" ,"Width" ,"Ampl. 2" ,"Ampl. 3" ,"BGD 2" ,"BGD 3" ,"Area All" ,"Area 2" ,"Area 3" ,"rArea All" ,"rArea 2" ,"rArea 3" ,"R 3H2/2H2" ,"rR 3H2/2H2" ,"rd 3H2/2H2" ,"d 3H2/2H2" ,"DeltaDelta 3H2/2H2" ,"R 2H/1H" ,"d 2H/1H" ,"AT% 2H/1H"),
    Units=c("" ,"" ,"" ,"" ,"" ,"[s]" ,"[s]" ,"[s]" ,"[s]" ,"[mV]" ,"[mV]" ,"[mV]" ,"[mV]" ,"[Vs]" ,"[Vs]" ,"[Vs]" ,"[mVs]" ,"[mVs]" ,"[mVs]" ,"" ,"" ,"[per mil] vs. ref" ,"[per mil] vs. VSMOW" ,"" ,"" ,"[per mil] vs. VSMOW" ,"[%]"),
    Type=c(rep("character", 5), rep("numeric", 22)),
    Show=TRUE, stringsAsFactors=FALSE)
  return (idp)
}

#######################
# Saving to workspace #
#######################

# saves idp instance to the gvar stored in it
IDP.save <- function(idp) {
  save <- IDP.init()
  save$settings <- tag(idp$gui$win, "settings") # get newest settings back from window instance
  # FIXME: also save data
  # Note: not saving any of the gui components (no need to keep the pointers)
  assign(idp$settings$gvar, save, envir=.GlobalEnv)
}

##########################
# Versioning and Updates #
##########################

# get all versions of the software
IDP.getVersions<-function() {
  # versions data frame
  versions<-data.frame(version=numeric(), date=character(), released=logical(), codename=character(), stringsAsFactors=F)
  versions[nrow(versions)+1,]<-list(version=0.1, date="8/22/2013", released=FALSE, codename="Rumpelstilzchen")
  #versions[nrow(versions)+1,]<-list(version=0.2, date="8/22/2013", released=TRUE, codename="Wurst")
  #versions[nrow(versions)+1,]<-list(version=0.3, date="8/22/2013", released=FALSE, codename="Test")
  # label
  versions$label<-paste("version ", versions$version, " (", versions$date, ", codename ", versions$codename, ")", sep="")
  versions[which(!versions$released),]$label <- paste(versions[which(!versions$released),]$label, "- DEV VERSION (unreleased)")
  return (versions)
}

# get version
IDP.getVersionInfo<-function(idp) {
  return (subset(IDP.getVersions(), version==idp$settings$version, select="label"))
}

# update an idp instance to the newest version
IDP.update<-function(idp) {
  for (version in subset(IDP.getVersions(), version > idp$settings$version)$version) {
    cat("Updating IDP instance from version", idp$settings$version, "to version", version, "...")
    idp<-do.call(paste("IDP.updateTo.v", version, sep=""), args=list(idp))
    cat(" complete.\n")
    idp$settings$version <- version
  }
  return (idp)
}

# update functions
IDP.updateTo.v0.2<-function(idp) {
  return (idp)
}

################
# Installation #
################

# install the necessary packages
IDP.install<-function() {
  local({# set mirror to berkely
    r <- getOption("repos")
    r["CRAN"] <- "http://cran.cnr.berkeley.edu/"
    options(repos = r)
  }) 
  cat("Installing required packages.\n")
  install.packages("ggplot2", depen=TRUE) # for advanced plotting, includes reshape2 and plyr, is quick
  install.packages("plyr", depen=TRUE)
  install.packages("psych", depen=TRUE) # for reading the from the clipboard, fairly quick
  install.packages("gWidgets", depen=TRUE) # for user interfaces, quick
  update.packages(ask=FALSE)
  cat("All required packages installed.\n")
}

###################
# Code management #
###################

# package IDP
IDP.package<-function(filename="IDP.R", dir="/Users/SKOPF/Dropbox/Tools/software/R/idp") {
  funcsdir<-"/Users/SKOPF/Dropbox/Tools/software/R/funcs"
  funcsfiles<-c("SKGUILIB.R", "SKDATALIB.R", "SKUTILLIB.R", "SKPLOTLIB.R")
  idpdir<-dir
  idpfiles<-c("IDP.main.R", "IDP.gui.R", "IDP.plots.R", "IDP.read.R", "IDP.table.R")
  files<-c(file.path(idpdir, idpfiles), file.path(funcsdir, funcsfiles))
  
  header<-paste(
    "##################################\n",
    "# PACKAGED Isodat File Processor #\n",
    "# ", format(Sys.time(), "%Y-%m-%d"), " by SKOPF           #\n",
    "##################################\n\n", sep="")
  for (i in 1:length(files)) 
    files[i]<-readChar(files[i], file.info(files[i])$size)
  cat(header, files, file=file.path(dir, filename), sep="\n\n\n")
}




#######################################
# Code for isodat file parser (IDP)   #
# User Interface (GUI)                #
# Copyright 2013 Sebastian Kopf       #
# seb.kopf@gmail.com                  #
#######################################

##################
# Info messaging #
##################

# show an info message
# type - styling of the mssage, info, error, question, warning are the standard ones
# timer - time in seconds until message disappears automatically
# okButton - whether there is an ok button or not
IDP.showInfo<-function(idp, msg, type="question", timer=2, okButton=TRUE) {
  idp$gui$infoBar$setMessageType(type)
  idp$gui$infoLabel$setText(msg)
  idp$gui$infoBar$show()
  if (!okButton)
    idp$gui$infoOkButton$hide()  
  else
    idp$gui$infoOkButton$show()
  if (!is.null(timer)) {
    Sys.sleep(timer)
    IDP.hideInfo(idp)
  }
}

# hide info bar
IDP.hideInfo<-function(idp) {
  idp$gui$infoBar$hide()
}

############
# Main GUI #
############

# launch the user interface
IDP.gui<-function(idp) {
  
  ### main window
  idp$gui$win<-gwindow(paste("Isodate File Processor -", IDP.getVersionInfo(idp)), width=1280, height=640, visible=FALSE)
  
  ### storing settings for easier modification
  tag(idp$gui$win, "settings")<-idp$settings
  
  ### major divisions
  wingrp<-ggroup(horizontal=FALSE, expand=TRUE, cont=idp$gui$win, spacing=0)
    parent <- getToolkitWidget(wingrp)$getParent() # get automatic toplevel gtkHBox generated by gwindow
    parent['border-width']<-0 # remove border
  idp$gui$navgrp<-ggroup(horizontal=FALSE, cont=wingrp, spacing=0) # navigation group
  gl<-ggroup(horizontal=FALSE, expand=TRUE) # left column
    files.grp<-ggroup(horizontal=FALSE, expand=TRUE, cont=gl) # left column file browser group
    addSpring(gl)
    fileInfo.grp<-ggroup(horizontal=FALSE, expand=TRUE, cont=gl) # left column for file info
    fileInfo.grpGTK<-getToolkitWidget(fileInfo.grp) # gtk object
    fileInfo.grpGTK['border-width']<-5 # border with
    # add another bottom group here if desired
    grt<-ggroup(horizontal=TRUE, expand=TRUE) # right top
      plot.grp<-ggroup(horizontal=FALSE, expand=TRUE, container=grt) # plot grp in right top
      #topnav.grp<-ggroup(horizontal=FALSE, container=grt) # navigation bar in right top
      #size(topnav.grp)<-c(50, 100)
    grb<-ggroup(horizontal=TRUE, expand=TRUE) # right bottom
      tag(idp$gui$win, "tableGrp")<-ggroup(horizontal=FALSE, expand=TRUE, container=grb) # table grp in bottom right
      #bottomnav.grp<-ggroup(horizontal=FALSE, container=grb) # navigation bar in right bottom
      #size(bottomnav.grp)<-c(50, 100)
  gr<-gpanedgroup(grt, grb, horizontal=FALSE, expand=TRUE) # right column
  gall<-gpanedgroup(gl, gr, horizontal=TRUE, container=wingrp, expand=TRUE, handler=function(h,...) print(svalue(gall))) # total window group
  
  ### files browser
  tag(idp$gui$win, "fileBrowserParentGrp")<-files.grp
  tag(idp$gui$win, "fileBrowserGrp")<-NULL

  # all the items of the file browser
  fileBrowser.items <- function(path = NULL, user.data=NULL) {
    topleveldir <- is.null(path)
    if (topleveldir) 
      path <- tag(idp$gui$win, "settings")$fileDirectory # start with file directory from the settings
    files <- file.info(dir(path=path, full.names=TRUE))[,c(1,2,3)] # get all files and folders
    files <- data.frame(Name=dir(path=path), Dir=files[,2], stringsAsFactors=FALSE)
    files <- files[union(which(files$Dir), grep("\\.cf$", files$Name)),] # only folders and .cf files
    files <- files[order(-files$Dir, files$Name),] # sort by folder vs file and then name
    if (topleveldir) # add "..." at the beginning (FIXME: could make sure that it's not added if at root but too lazy now)
      files <- rbind(data.frame(Name="...", Dir=FALSE), files)
    return(files)
  }
  
  # functions to determine which items have sub items and which icons to use
  fileBrowser.hasOffspring <- function(children,user.data=NULL, ...)return(children$Dir) # which items have subdirectories
  fileBrowser.icons <- function(children,user.data=NULL, ...) {
    x <- rep("gtk-new", length=nrow(children))
    x[which(children$Name=="...")] <- "gtk-directory" # top level folder
    x[children$Dir] <- "gtk-directory" # real folders
    return(x)
  }
  
  # function to remake the file tree when switching folders (FIXME: somehow looses ability for tree expansion...maybe something about how the tree is added?)
  fileBrowser.gui<-function() {
    if (!is.null(tag(idp$gui$win, "fileBrowserGrp")))
      delete(tag(idp$gui$win, "fileBrowserParentGrp"), tag(idp$gui$win, "fileBrowserGrp"))
    tag(idp$gui$win, "fileBrowserGrp") <- ggroup(expand=TRUE)
    add(tag(idp$gui$win, "fileBrowserParentGrp"), tag(idp$gui$win, "fileBrowserGrp"), expand=TRUE)
    tree<-gtree(fileBrowser.items, fileBrowser.hasOffspring, icon.FUN = fileBrowser.icons, container=tag(idp$gui$win, "fileBrowserGrp"), expand=TRUE, handler=function(h,...) {
      if (is.null(subpath <- svalue(h$obj[]))) { 
        path <- tag(idp$gui$win, "settings")$fileDirectory
        file <- svalue(h$obj)
      } else {
        path <- file.path(tag(idp$gui$win, "settings")$fileDirectory, do.call("file.path", args=as.list(subpath[-length(subpath)])))
        file <- subpath[length(subpath)]
      }
      
      newpath <- file.path(path, file) # assemble new path  
      if (identical(file, "...")) { # except: top level directory
        pathparts <- strsplit(path, .Platform$file.sep)[[1]]
        newpath <- do.call("file.path", args=as.list(pathparts[-length(pathparts)]))
      } 
        
      if (file.info(newpath)$isdir) { # open directory
        tag(idp$gui$win, "settings")$fileDirectory <- newpath
        fileBrowser.gui()
      } else # open file
        IDP.openIsodatFile(idp, path, file)
    })
    size(tree)<-c(idp$settings$fileBrowserWidth,500)
  }
  # --> file browser initialization is at the end because it needs other idp objects to be fully initialized
  
  ### file info group
  fileInfo.layout <- glayout(cont = fileInfo.grp, expand=TRUE)
  fileInfo.layout[1, 1] <- "File:"
  fileInfo.layout[1, 2] <- (idp$gui$fileInfo$Filename <- glabel("", cont=fileInfo.layout))
  fileInfo.layout[2, 1] <- "GC:"
  fileInfo.layout[2, 2] <- (idp$gui$fileInfo$GCprogram <- glabel("", cont=fileInfo.layout))
  fileInfo.layout[3, 1] <- "AS:"
  fileInfo.layout[3, 2] <- (idp$gui$fileInfo$ASprogram <- glabel("", cont=fileInfo.layout))
  fileInfo.layout[4, 1] <- "MS:"
  fileInfo.layout[4, 2] <- (idp$gui$fileInfo$MSprogram <- glabel("", cont=fileInfo.layout))
  fileInfo.layout[5, 1] <- "H3:"
  fileInfo.layout[5, 2] <- (idp$gui$fileInfo$H3factor <- glabel("", cont=fileInfo.layout))
  
  ### plot grp
  idp$gui$pn <- pn.GUI(plot.grp, idp$gui$win, enablePlotLabel=FALSE, enableMenuButtons=FALSE, startWithTab=FALSE,
                       plotObjLoadHandler=function(obj) IDP.loadIsodatFileTab(idp, obj$plotinfo),
                       plotEventHandlers=list(
                          Changed = function(h,...) IDP.plotClickHandler(idp, h),
                          Doubleclick = function(h,...) IDP.plotDoubleClickHandler(idp, h),
                         Rightclick = function(h,...) IDP.plotRightClickHandler(idp, h))) 
    
  ### table grp
  tag(idp$gui$win, "dataTable") <- gtable(IDP.getEmptyPeakTable(idp), expand=TRUE, cont=tag(idp$gui$win, "tableGrp"))
  
  ### menu and toolbar navigation actions
  nav.xml <- IDP.getNavXML()
  nav.actions <-
    list(## name, icon, label , accelerator , tooltip , callback
      list ("IDP" , NULL , "_IDP" , NULL , NULL , NULL ) ,
      list ("SaveToWorkspace" , "gtk-home" , "Save To Workspace" , "<ctrl>H" ,"Save settings and data to workspace" , function(...) { 
        IDP.showInfo(idp, "Saving to workspace...", timer=1, okButton=FALSE)
        IDP.save(idp)
        IDP.showInfo(idp, "Isodat File Processor settings and data succesfully saved to workspace.", timer=2, okButton=FALSE)
        } ) , 
      list ("Quit", "gtk-quit", "Quit", NULL, "Quit program", function(...) { if (gconfirm("Are you sure you want to quit?")) dispose(idp$gui$win) } ),
      list ("File" , NULL , "_File" , NULL , NULL , NULL ) , 
      list ("OpenFile" , "gtk-open" , "Open File" , "<ctrl>O" ,"Open isodat file" , function(...) message("sorry, file selection dialog not implemented yet") ) , 
      list ("CloseFile" , "gtk-close" , "Close File" , "<ctrl>W" ,"Close isodat file" , idp$gui$pn$actions$aClosePlot$handler ) , 
      list ("CloseAll" , "gtk-stop" , "Close all Files" , "<ctrl><shift>W" ,"Close all isodat files" , function(...) gmessage("sorry, not implemented yet") ) ,
      list ("ExportExcel" , "gtk-save-as" , "Export to Excel" , "<ctrl>X" , "Export raw data and peak table to excel" , function(...) gmessage("sorry, not implemented yet") ) , 
      list ("ExportAll" , "gtk-harddisk" , "Export all to Excel" , "<ctrl><shift>X" , "Export raw data and peak table for all files" , function(...) gmessage("sorry, not implemented yet") ) , 
      list ("Plot" , NULL , "_Plot" , NULL , NULL , NULL ) , 
      list ("SavePlot", "gtk-save-as", "Save as PDF", "<ctrl>S", "Save chromatogram as PDF", idp$gui$pn$actions$aSavePlot$handler ),
      list ("SaveAll", "gtk-harddisk", "Save all", "<ctrl><shift>S", "Save all chromatograms as PDFs", idp$gui$pn$actions$aSaveAll$handler ),
      list ("PrintPlot", "gtk-print", "Print", NULL, "Print chromatogram", idp$gui$pn$actions$aPrintPlot$handler ),
      list ("Help" , "gtk-info" ,"Help" , NULL , NULL , function(...) gmessage("sorry, not implemented yet") ) ,
      list ("View" , NULL , "_View" , NULL , NULL , NULL ) ,
      list ("ZoomFull", "gtk-zoom-fit", "Unzoom", NULL, "Unzoom", function(h,...) IDP.zoomReset(idp) ),
      list ("ZoomBack", "gtk-undo", "Undo Zoom", "<ctrl>Z", "Return to previous zoom", function(h,...) IDP.zoomBack(idp) ),
      list ("ZoomIn", "gtk-zoom-in", "Zoom in", "<ctrl>P", "Zoom in", function(h,...) IDP.zoomIn(idp) ), # fix me, bertter short cuts?
      list ("ZoomOut", "gtk-zoom-out", "Zoom out", "<ctrl>M", "Zoom out", function(h,...) IDP.zoomOut(idp) ), # fix me, better short cuts?
      list ("MoveIntervalB", "gtk-go-back", "Move Left", "<ctrl><shift>Left", "Move visible window to the left", function (h,...) IDP.zoomMove(idp, -1)), # FIXME: shortcuts don't work
      list ("MoveIntervalF", "gtk-go-forward", "Move Right", "Right", "Move visible window to the right", function (h,...) IDP.zoomMove(idp, +1)), # FIXME: shortcuts don't work
      list ("SetAxes", "gtk-page-setup", "Set Axes", NULL, "Set axis coordinates", function(h,...) print("set axes") ), # fix me, better short cuts?
      list ("Edit" , NULL , "_Edit" , NULL , NULL , NULL ) ,
      list ("Mode" , NULL , "_Mode" , NULL , NULL , NULL ) ,
      list ("Settings" , NULL , "_Settings" , NULL , NULL , NULL ) , 
      list ("XUnits" , NULL , "_X-axis Units" , NULL , NULL , NULL ) ,
      list ("EditSettings" , "gtk-preferences" , "Edit Settings" , NULL , NULL , function(...) {} ) , 
      list ("TableColumns", "gtk-properties", "Table Columns", NULL, "Select which peak table columns are displayed", function(...) {
        dlg <- gbasicdialog(title="Select visible columns for the peak table", handler = function(h,...) {
            tag(idp$gui$win, "settings")$peakTableColumns <- tbl[] # save updated peak table column settings
            delete(tag(idp$gui$win, "tableGrp"), tag(idp$gui$win, "dataTable")) # delete previous table
            tag(idp$gui$win, "dataTable")<-gtable(IDP.getEmptyPeakTable(idp), expand=TRUE, cont=tag(idp$gui$win, "tableGrp")) # remake current table
            IDP.loadPeakTable(idp, pn.getAllInfo(idp$gui$pn)$peakTable)
          })
        size(dlg)<-c(500,500)
        tbl <- table.toggleTable(ggroup(cont=dlg, expand=TRUE), tag(idp$gui$win, "settings")$peakTableColumns, "Show")
        visible(dlg, set=TRUE) ## show dialog
        }),
      list ("DeletePeak" , "gtk-cancel" , "Delete Peak", "<ctrl>D", "Delete selected peak (<ctrl>D)" , function(...) {gmessage("sorry, not implemented yet")}),
      list ("CopyTable" , "gtk-copy" , "Copy Table", "<ctrl>C", "Copy the peak table to the clipboard." , function(...) IDP.copyPeakTable(idp)),
      list ("Recalculate" , "gtk-execute" , "Recalculate", "<ctrl>R", "Recalculate the isotopic composition based on the standards picked." , function(...) IDP.recalculatePeakTable(idp)),
      list ("Revert" , "gtk-revert-to-saved" , "Discard All", NULL, "Discard all changes and return to original peak table from data file." , function(...) IDP.revertPeakTable(idp))
    )
  
  action_group <- gtkActionGroup ( "FileGroup" )
  action_group$addActions( nav.actions )
  
  ### special actions
  # full screen (toggle)
  fullscreen_act<-gtkToggleAction("FullScreen", "Full Screen", "Make application full screen", stock.id="gtk-fullscreen")
  gSignalConnect (fullscreen_act , "toggled" , function ( action ) {
    if(fullscreen_act ['active'] )
      getToolkitWidget(idp$gui$win)$fullscreen ( )
    else
      getToolkitWidget(idp$gui$win)$unfullscreen ( )
  } )
  action_group$addActionWithAccel(fullscreen_act, "<control>F")
  
  # switch between multiple options [generic implementation]
  optionsSwitch<-function(options, signals, action) {
    for (name in names(options)) {
      gSignalHandlerBlock(options[[name]], signals[[name]])
      options[[name]]['active'] <- identical(name, action$name)
      gSignalHandlerUnblock(options[[name]], signals[[name]])
    }
    return(action$name)
  }
  
  # switching x axis unit
  xaxisActs<-list(
    XaxisSec = gtkToggleAction("XaxisSec", "Seconds", "Show graph in seconds"),
    XaxisMin = gtkToggleAction("XaxisMin", "Minutes", "Show graph in minuts")
  )
  xaxisSignals<-list()
  for (name in names(xaxisActs)) {
    xaxisSignals[[name]] <- gSignalConnect (xaxisActs[[name]] , "toggled", function(action) {
        opt <- optionsSwitch(xaxisActs, xaxisSignals, action)
        tag(idp$gui$win, "settings")$plotOptions$xUnits$value <- which(idp$settings$plotOptions$xUnits$ids == opt)
        IDP.plot(idp) # replot
      })
    action_group$addAction(xaxisActs[[name]])
  }
  
    
  # best fit (toggle)
  idp$gui$bestfitActive<-gtkToggleAction("BestFit", "Best Fit", "Fit to tallest peak", stock.id="gtk-zoom-100")
  gSignalConnect (idp$gui$bestfitActive , "toggled" , function ( action ) {
    if (idp$gui$bestfitActive['active'])
      IDP.zoomBest(idp)
  } )
  action_group$addActionWithAccel(idp$gui$bestfitActive, "<control>B")
  
  # switch between modes
  modeacts<-list(
    ModeInfo = gtkToggleAction("ModeInfo", "Information", "Select peaks for information", stock.id="gtk-leave-fullscreen"),
    ModeAdd = gtkToggleAction("ModeAdd", "Add Peak", "Add peaks", stock.id="gtk-add"),
    ModeEdit = gtkToggleAction("ModeEdit", "Edit Peak", "Edit peaks", stock.id="gtk-redo"),
    ModeStds = gtkToggleAction("ModeStds", "Choose Stds", "Choose isotopic standards", stock.id="gtk-about"))
  modeact_IDs<-list()
  for (name in names(modeacts))
    modeact_IDs[[name]] <- gSignalConnect (modeacts[[name]] , "toggled" , function(action) tag(idp$gui$win, "settings")$mode<-optionsSwitch(modeacts, modeact_IDs, action))
  action_group$addActionWithAccel(modeacts$ModeInfo, "<control>I")
  action_group$addActionWithAccel(modeacts$ModeAdd, "<control>A")
  action_group$addActionWithAccel(modeacts$ModeEdit, "<control>E")
  action_group$addActionWithAccel(modeacts$ModeStds, NULL) #FIXME (control - C taken by copy peak table)
  
  
  ### assemble menu
  uimanager <- gtkUIManagerNew() 
  uimanager$insertActionGroup ( action_group , 0)
  id <- uimanager$addUiFromString ( nav.xml )
  gtkNavgrp <- getToolkitWidget(idp$gui$navgrp) # get navgrp gtk object
  menubar <- uimanager$getWidget ( "/menubar" ) # menubar 
  gtkNavgrp$packStart ( menubar , FALSE )
  toolbar <- uimanager$getWidget ( "/toolbar" ) # toolbar
  gtkNavgrp$packStart ( toolbar , FALSE )
  getToolkitWidget(idp$gui$win)$addAccelGroup ( uimanager$getAccelGroup ( ) ) # add keyboard triggers
 
  ### add information bar
  idp$gui$infoBar <- gtkInfoBar (show=FALSE) 
  idp$gui$infoBar$setNoShowAll (TRUE)
  idp$gui$infoLabel <- gtkLabel ( "Warning , Warning")
  idp$gui$infoBar$setMessageType("question") 
  idp$gui$infoBar$getContentArea()$add(idp$gui$infoLabel)
  idp$gui$infoOkButton <-idp$gui$infoBar$addButton(button.text = "gtk-ok", response.id = GtkResponseType['ok'])
  gSignalConnect(idp$gui$infoBar, "response", function(infoBar, resp.id) IDP.hideInfo(idp))
  getToolkitWidget(wingrp)$packStart(idp$gui$infoBar, expand=FALSE)
  
  ### initialize file browser (this late so it has access to the different objects created later)
  fileBrowser.gui()
  
  ### make window visible after it's completely initialized
  optionsSwitch(modeacts, modeact_IDs, list(name=idp$settings$mode)) # select right option from the start
  optionsSwitch(xaxisActs, xaxisSignals, list(name=idp$settings$plotOptions$xUnits$ids[[idp$settings$plotOptions$xUnits$value]])) # select right xaxis from the start
  visible(idp$gui$win)<-TRUE # window initially invisble to make plotting right away possible (without the margins error)
  
  return (idp)
}

# get toolbar and navigation structure
IDP.getNavXML<-function() {
  ### menu and toolbar navigation structure
  nav.xml<-'<ui>
    <menubar name="menubar">
  <menu name = "IDP" action="IDP">
  <menuitem action="SaveToWorkspace" />
  <menuitem action="Help"/>
  <menuitem action="Quit" />
  </menu>
  <menu name ="FileMenu" action ="File">
  <menuitem action ="OpenFile" />
  <menuitem action ="CloseFile" />
  <menuitem action ="CloseAll" />
  <separator/>
  <menuitem action ="ExportExcel" />
  <menuitem action ="ExportAll" />
  </menu>
  <menu name="PlotMenu" action="Plot">
  <menuitem action="SavePlot"/>
  <menuitem action="SaveAll"/>
  <separator/>
  <menuitem action="PrintPlot"/>
  </menu>
  <menu name="Edit" action="Edit">
  <menu name="Mode" action="Mode">
  <menuitem action ="ModeInfo"/>
  <menuitem action ="ModeAdd"/>
  <menuitem action ="ModeEdit"/>
  <menuitem action ="ModeStds"/>
  </menu>
  <menuitem action ="DeletePeak"/>
  </menu>
  <menu name="View" action="View">
    <menuitem action="ZoomBack"/>
    <menuitem action="ZoomFull"/>
    <menuitem action="ZoomIn"/>
    <menuitem action="ZoomOut"/>
    <menuitem action="BestFit"/>
    <menuitem action="MoveIntervalB"/>
    <menuitem action="MoveIntervalF"/>
    <menuitem action="SetAxes"/>
    <separator/>
    <menuitem action="FullScreen"/>
  </menu>
  <menu name="Settings" action="Settings">
    <menu name="XUnits" action="XUnits">
        <menuitem action="XaxisSec"/>
        <menuitem action="XaxisMin"/>
    </menu>
  <menuitem action="EditSettings"/>
  <menuitem action="TableColumns"/>
  </menu>
  </menubar>
  <toolbar name ="toolbar">
    <toolitem action = "ZoomFull"/>
    <toolitem action = "ZoomIn"/>
    <toolitem action = "ZoomOut"/>
    <toolitem action = "BestFit"/>
    <toolitem action = "MoveIntervalB"/>
    <toolitem action = "MoveIntervalF"/>
    <toolitem action ="SetAxes"/>
    <separator/>
    <toolitem action ="ModeInfo"/>
    <toolitem action ="ModeAdd"/>
    <toolitem action ="ModeEdit"/>
    <toolitem action ="DeletePeak"/>
    <toolitem action ="ModeStds"/>
    <separator/>
    <toolitem action ="CopyTable"/>
    <toolitem action ="Recalculate"/>
    <toolitem action ="Revert"/>
  </toolbar>
  </ui>'
  return(nav.xml)
}



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


#######################################
# Code for isodat file parser (IDP)   #
# Isodat file reading                 #
# Copyright 2013 Sebastian Kopf       #
# seb.kopf@gmail.com                  #
#######################################

# load tab
IDP.loadIsodatFileTab<-function(idp, fileTabObj) {
  IDP.loadPeakTable(idp, fileTabObj$peakTable) # load peak table
  widgets.load(idp$gui$fileInfo, fileTabObj$fileInfo) # load regular file information
}

# open isodat file into idp tab manager
IDP.openIsodatFile<-function(idp, directory, filename) {
  
  # check for existing files
  openFiles <- pn.getAllPlotTabNames(idp$gui$pn)
  if (!is.null(openFiles) && !is.empty(index <- which(openFiles == filename))){
    pn.setPlotTab(idp$gui$pn, index) # set tab
    IDP.plot(idp) # replot
    return()
  }
  
  # load new file
  IDP.showInfo(idp, paste("Loading ", filename, "...", sep=""), timer=NULL, okButton=FALSE)
    
  # try reading isodat file
  tryCatch(
    fileObj<-IDP.readIsodatFile(directory, filename), 
    error=function(e) {
      print(e)
      IDP.showInfo(idp, paste("Error reading binary file ", filename, ".", sep=""), type="error", timer=NULL, okButton=TRUE)
      stop(e)
    })
  
  # try reading peak table
  tryCatch({
    for (i in which(tag(idp$gui$win, "settings")$peakTableColumns$Type=="numeric")) 
      fileObj$peakTable[[i]]<-as.numeric(fileObj$peakTable[[i]])  
  },
    error=function(e){
      # FIXME: this does not necessarily mean we can't proceed!
      IDP.showInfo(idp, paste("Error decoding peak table in ", filename, ".\nThis might be an isodat version problem.", sep=""), type="error", timer=NULL, okButton=TRUE)
      stop(e)
    })
  
   
  # generate new tab
  do.call(idp$gui$pn$actions$aNewPlot$handler, list())
  pn.changePlotTabName(idp$gui$pn, filename)
  Sys.sleep(0.5) # FIXME: to avoid margin problem
  
  # store tab info
  tabInfo <- list(
    fileInfo = fileObj[c("H3factor", "GCprogram", "MSprogram", "Filename", "ASprogram")],
    originalPeakTable = fileObj$peakTable,
    peakTable = fileObj$peakTable,
    peakTableEdited = FALSE, # whether the peak table has been edited at all
    peakTableEvaluated = TRUE, # whether the peak table has been evaluated since the last edit that would require reevaluation (e.g. changing standards)
    data = fileObj$data)
  pn.storeInfo(idp$gui$pn, tabInfo, reset=TRUE)
  
  # plot
  IDP.plot(idp)
  
  # load tab
  IDP.loadIsodatFileTab(idp, pn.getAllInfo(idp$gui$pn))
  
  # finished
  IDP.showInfo(idp, paste(filename, "loaded successfully."), timer=2, okButton=FALSE)
}

# read a standard isodat file with H2 data in it
# printRemoved - if you want command line output of keys that are removed in the cleanup process
IDP.readIsodatFile<-function(filepath, filename, printRemoved = FALSE) {
  # return vaues
  obj<-list()
  obj$Filepath<-file.path(filepath, filename)
  
  # raw data
  rawdata<-bin.readFile(obj$Filepath)
  obj$keys<-bin.findAllText(rawdata)
  obj$keys<-bin.cleanText(obj$keys, printRemoved = printRemoved)
  
  ###### H2/H3 traces ######
  # struture of the header
  headerStr<-bin.struct("raw", 14)
  headerStr<-bin.struct("character", 13, size = 2, id = "text", struct=headerStr)
  headerStr<-bin.struct("raw", 20, struct=headerStr)
  headerStr<-bin.struct("integer", 1, size = 4, id = "size", struct=headerStr)
  headerStr<-bin.struct("integer", 1, size = 2, id = "nions_maybe", struct=headerStr)
  headerStr<-bin.struct("raw", 29, struct=headerStr)
  headerStr<-bin.struct("integer", 1, size = 2, id = "nions_maybe", struct=headerStr)
  header<-bin.parseStruct(rawdata, headerStr, offset = subset(obj$keys, value=="CRawDataScanStorage")$byteEnd, saveUnknown=FALSE)
  
  # structure of the data
  dataStr<-bin.struct("numeric", 1, size = 4, id = "time")
  dataStr<-bin.struct("numeric", 1, size = 8, id = "mass2", struct=dataStr)
  dataStr<-bin.struct("numeric", 1, size = 8, id = "mass3", struct=dataStr)
  data<-bin.multiParseStruct(rawdata, dataStr, header$size, 
                             offset=subset(obj$keys, value=="CRawDataScanStorage")$byteEnd + bin.structLength(headerStr))

  # structre of the footer (post data) # FIXME: not currently used because the information is not something useful for us
  # NOTE: could theoretically use this to figure out the names of the ion channels measured ubove (to be even more dynamic)
  #footerStr<-bin.struct("raw", 70)
  #footerStr<-bin.struct("character", 6, size = 2, id = "text", struct=footerStr)
  #footerStr<-bin.struct("raw", 4, struct=footerStr)
  #footerStr<-bin.struct("character", 6, size = 2, id = "text", struct=footerStr)
  #footer<-bin.parseStruct(rawdata, footerStr, offset = subset(obj$keys, value=="CRawDataScanStorage")$byteEnd + bin.structLength(headerStr) + bin.structLength(dataStr)*header$size, saveUnknown=FALSE)
  
  #### RATIO DATA ####
  # struture of the ratio header
  ratioHeadStr<-bin.struct("raw", 14)
  ratioHeadStr<-bin.struct("character", 13, size = 2, id = "text", struct=ratioHeadStr)
  ratioHeadStr<-bin.struct("raw", 20, struct=ratioHeadStr)
  ratioHeadStr<-bin.struct("integer", 1, size = 2, id = "size", struct=ratioHeadStr)
  ratioHeadStr<-bin.struct("raw", 2, struct=ratioHeadStr)
  ratioHeadStr<-bin.struct("integer", 1, size = 2, id = "nions_maybe", struct=ratioHeadStr)
  ratioHeadStr<-bin.struct("raw", 18, struct=ratioHeadStr)
  ratioHeadStr<-bin.struct("integer", 1, size = 2, id = "nions_maybe", struct=ratioHeadStr)
  ratioHeader<-bin.parseStruct(rawdata, ratioHeadStr, offset = subset(obj$keys, value=="CRatioDataScanStorage")$byteEnd, saveUnknown=FALSE)
  
  # structure of the ratio data
  ratioDataStr<-bin.struct("numeric", 1, size = 4, id = "time")
  ratioDataStr<-bin.struct("numeric", 1, size = 8, id = "ratio_3o2", struct=ratioDataStr)
  ratios<-bin.multiParseStruct(rawdata, ratioDataStr, header$size, 
                               offset=subset(obj$keys, value=="CRatioDataScanStorage")$byteEnd + bin.structLength(ratioHeadStr))
  
  obj$data<-cbind(data, ratios["ratio_3o2"]) ### data that is returned
  
  ### INDIVIDUAL DATA ###
  obj$H3factor<-bin.parseStruct(rawdata, bin.struct("numeric", 1, size = 8, id="H3"), offset = subset(obj$keys, value=="H3 Factor")$byteEnd[1] + 8)$H3
  obj$GCprogram<-obj$keys[grep(".gcm$", obj$keys$value)[1],"value"]
  obj$MSprogram<-obj$keys[grep(".met$", obj$keys$value)[1],"value"]
  obj$Filename<-obj$keys[grep(".cf$", obj$keys$value)[1],"value"]
  obj$ASprogram<-obj$keys[grep("Internal", obj$keys$value)[1],"value"]
  
  ### PEAK TABLE ###
  isodat.decodePeakTable<-function(data, keys) {
    rawtable<-data[subset(keys, value=="CPkDataListBox")$byteEnd:subset(keys, value=="CGCPeakList")$byteStart]
    arials<-grepRaw("([Arial][^\u0020-\u007e]){5}", rawtable, all=TRUE)
    #FIXME: newer versions of isodat (2.5 and 3.1 don't have this business, just 18 bytes between each label!)
    if (length(arials) > 5) {
      entries<-NULL
      spos <- 9 + (regexpr("14000000fffeff08", paste(readBin(rawtable[1:(arials[1]-48)], "raw", n=(arials[1]-48)), collapse=""), fixed=TRUE)-1)/2
      for (i in arials) {
        epos<-(i-48)
        entries<-c(entries, paste(readBin(rawtable[spos:epos], "character", n=(epos-spos)/2, size=2), collapse=""))
        spos<-i+100
      }
      table<-matrix(entries[-length(entries)], byrow=TRUE, ncol=27) # FIXME not sure this is always true that it's 27 columns but appears to be the case
      df<-data.frame(table[2:nrow(table),], stringsAsFactors=FALSE)
      names(df)<-table[1,]
      return(df)
    } else
      return(NULL) #data.frame(error=c("this cf file appears to be saved in a newer isodat version (>2.0)", "peak table parsing not implemented yet")))
  }
  
  obj$peakTable<-isodat.decodePeakTable(rawdata, obj$keys) ### peak table 
  
  return (obj)
}



#######################################
# Code for isodat file parser (IDP)   #
# Peak Table Funcs                    #
# Copyright 2013 Sebastian Kopf       #
# seb.kopf@gmail.com                  #
#######################################

IDP.getEmptyPeakTable<-function(idp) {
  options(stringsAsFactors = FALSE)
  indices <- which(tag(idp$gui$win, "settings")$peakTableColumns$Show)
  df<-do.call("data.frame", lapply(tag(idp$gui$win, "settings")$peakTableColumns$Type[indices], FUN = function(x) do.call(x, list())))
  names(df)<-paste(tag(idp$gui$win, "settings")$peakTableColumns$Column[indices], tag(idp$gui$win, "settings")$peakTableColumns$Units[indices], sep="\n")
  return(df)
}

IDP.getDataPeakTable<-function(idp, peakTable) {
  indices <- which(tag(idp$gui$win, "settings")$peakTableColumns$Show)
  return(peakTable[,indices])
}

IDP.loadPeakTable<-function(idp, peakTable) {
  if (!is.null(peakTable)) { # peak table available
    tag(idp$gui$win, "dataTable")[] <- IDP.getDataPeakTable(idp, peakTable)
  } else { # no peak table --> just show empty one
    tag(idp$gui$win, "dataTable")[] <- IDP.getEmptyPeakTable(idp)
  }
}

# copy peak table to clipboard
IDP.copyPeakTable<-function(idp) {
  # FIXME: throw warning if there have been changes to the standards that have not be recalculated yet
  cp.copyDF(tag(idp$gui$win, "dataTable")[])
}

# recalculate isotopic value of peak table
IDP.recalculatePeakTable<-function(idp) {
  gmessage("sorry, not implemented yet")
}

# revert to original peak table
IDP.revertPeakTable<-function(idp) {
  if (!is.null(oriPeakTable <- pn.getAllInfo(idp$gui$pn)$originalPeakTable)) {
    if (gconfirm("Do you really want to discard all changes you have made to peak/standard assignments in this file?")) {
      pn.storeInfo(idp$gui$pn, list(peakTable=oriPeakTable), reset=TRUE)
      IDP.loadPeakTable(idp, oriPeakTable)
    }
  }
}


##################################
# code for generic GUI functions #
# Copyright 2013 Sebastian Kopf  #
# seb.kopf@gmail.com             #
##################################

##########
# COMBOS #
##########

# load combo
# sort=FALSE/TRUE does alphabetical sorting
combo.load<-function(combo, entries, sort=FALSE, selection=NULL, blockHandlers=NULL) {
  combo.blockHandlers(combo, blockHandlers)
  if (sort) # sort the combo
    entries<-entries[order(entries)]
  combo[]<-entries
  if (!is.null(selection))
    combo.setSelection(combo, selection)
  combo.unblockHandlers(combo, blockHandlers)
}

# add to combo
combo.add<-function(combo, entry, sort=FALSE, select=TRUE, blockHandlers=NULL) {
  if (select)
    selection<-entry # select new entry
  else
    selection<-1 # select first entry
  combo.load(combo, c(combo[], entry), sort=sort, selection=selection, blockHandlers=blockHandlers)
}

# delete selected entry
combo.deleteSelection<-function(combo, reSelect=TRUE, blockHandlers=NULL) {
  index<-combo.getSelection(combo, index=TRUE)
  if (reSelect) 
    selection<-min(index,length(combo[])-1) # select the second to last object if the delted one was the last one
  else
    selection<-1 # select first entry otherwise
  combo.load(combo, combo[-index], selection=selection, blockHandlers=blockHandlers)
}

# selection
combo.getSelection<-function(combo, index=FALSE) return (svalue(combo, index=index))
combo.setSelection<-function(combo, value) {
  if (is.numeric(value))
    svalue(combo, index=TRUE)<-value
  else
    svalue(combo, index=FALSE)<-value
}

# block handlers
combo.blockHandlers<-function(combo, handlerIDs){
  if (!is.null(handlerIDs))
    for (handlerID in handlerIDs)
      blockHandler(combo, handlerID)
}

# unblock handlers
combo.unblockHandlers<-function(combo, handlerIDs){
  if (!is.null(handlerIDs))
    for (handlerID in handlerIDs)
      unblockHandler(combo, handlerID)
}

##########
# TABLES #
##########

####### ENTRIES (get, set, add, edit, delete) ########

# get value of a table entry at the provided index
# if no field is given, returns whole row, if field is given (e.g. c("adsfs", "sdfd")) then only field
table.getValue<-function(table, index, fields=NULL) {
  if (!is.null(fields))
    return(table[][index, fields])
  else
    return(table[][index, ])
}

# get the selected index of a table
table.getSelectedIndex<-function(table) {
  if (!is.empty(index<-svalue(table, index=TRUE)))
    return (index)
  else
    return (NULL)
}

# get selected value from a table (depends on what is defined as the selection column, usually the first column)
# or if providing field param, get specific one
table.getSelectedValue<-function(table, field=NULL) {
  if (is.null(field) && !is.empty(value<-svalue(table, index=FALSE)))
    return (value)
  else if (!is.null(field) && !is.null(dataset<-table.getSelection(table)))
    return (dataset[field])
  return (NULL)
}

# get selected record from a gtable
table.getSelection<-function(table) {
  if (!is.null(index<-table.getSelectedIndex(table))) { # check if anything is selected
    if (is.null(nrow(table[]))) 
      return(table[]) # if there's only one object in the table
    else
      return(table[][index, ]) # if there's multiple objects, return just selected
  }
  return (NULL)
}

# select something in the table
# blocks the changed handler if provided during the selection
table.setSelectedValue<-function(table, value, index=FALSE, blockHandlers=NULL) {
  table.blockHandlers(table, handlers=blockHandlers) # block handlers
  svalue(table, index=index)<-value # set value (use index to decide whether by value or by index)
  table.unblockHandlers(table, handlers=blockHandlers) # unblock handlers
}

# delete selected entry in a data table
# reselect - reselects the record at the same position
# if blockChangedHandlerID is provided, it will block this handler before doing the reselection business (and then unblock it)
table.deleteSelection<-function(table, reSelect=FALSE, blockHandlers=NULL) {
  if (!is.null(index<-table.getSelectedIndex(table))) { # check if anything is selected
    if (!is.null(total<-nrow(table[]))) { # does not support deleting the last one
      table.blockHandlers(table, handlers=blockHandlers) # block handlers
      table[]<-table[][-index,]  # remove line
      if (reSelect) {
        if (index==total) index<-max(1,total-1) # select the second to last object if the delted one was the last one
        table.setSelectedValue(table, index, index=TRUE) 
      }
      table.unblockHandlers(table, handlers=blockHandlers)
    } else
      print("WARNING: can't delete the last item in a table")
  }
}

# update table entry
table.update<-function(table, data, index, ignoreMissing = FALSE) {
  for (field in names(data)) {
    if (field%in%names(table[])) # update table field
      table[][index,field]<-data[[field]]
    else if (!ignoreMissing)
      cat(paste("\nWARNING (to disable, pass ignoreMissing = TRUE):\n\tTrying to update table field", 
                  field, #"with value", data[[field]], 
                  "but field does not exist in table (", paste(names(table[]), collapse=", "), ")"))  
  }
}

# update currently selected table entry
table.updateSelection<-function(table, data, ignoreMissing = FALSE) {
  if (!is.null(index<-table.getSelectedIndex(table))) # check if anything is selected
      table.update(table, data, index, ignoreMissing = ignoreMissing)
}

# add an entry in the data table
# index --> add at specific index, if NULL, adds at the end of the table (or if index > length of table)
# returns index of newly added entry
table.add<-function(table, data, index=NULL, select=TRUE, blockHandlers=NULL, ignoreMissing = FALSE) {
  table.blockHandlers(table, handlers=blockHandlers) # block handlers
  if (!is.null(index) && index<=nrow(table[])) 
    table[][(index+1):(nrow(table[])+1),]<-table[][index:nrow(table[]),] # move records up one
  else
    index<-nrow(table[])+1 # adding at the end of the table
  table.update(table, data, index, ignoreMissing = ignoreMissing)
  
  if (select) 
    table.setSelectedValue(table, index, index=TRUE) 
  table.unblockHandlers(table, handlers=blockHandlers) # unblock handlers
  return (index)
}

# add an entry in the data table after the current selection
# if nothing is selected, adds a new entry at the end of the table
# returns index of newly added entry
table.addAfterSelection<-function(table, data, select=TRUE, blockHandlers=NULL, ignoreMissing = FALSE) {
  if ( !is.null(index<-table.getSelectedIndex(table)))
    index<-index + 1
  return (table.add(table, data, index=index, select=select, blockHandlers=blockHandlers, ignoreMissing = ignoreMissing))
}

####### HANDLERS ##########

# table handler blocks (This is only implemented this way because handler blocking does not seem to work for gtables!)
# important, need to implement this in the handlers though!!
table.blockHandlers<-function(obj, handlers=c("changed", "clicked")) for (handler in handlers) tag(obj, paste(handler,"Handler", sep=""))<-TRUE
table.unblockHandlers<-function(obj, handlers=c("changed", "clicked")) for (handler in handlers) tag(obj, paste(handler,"Handler", sep=""))<-FALSE
table.isHandlerBlocked<-function(obj, handler=NULL) {
  if (is.null(handler) || is.null(tag(obj, paste(handler,"Handler", sep=""))))
      return (FALSE)
  return (tag(obj, paste(handler,"Handler", sep="")))
}
  
###### TABLE SORTING ########

# datasets move up and down in tables
# move = c("up", "down", "top", "bottom"), pick one
# gvar = the name of a global variable to update
# tableID = if a global varaible is provided, use this field in the table to find the right entries in the global variable to update
# gvarID = if a global variable is provided, use this field to find the right entries to update
# gvarOrder = if a global variable is provided, use this field to keep track of the locations
table.moveHandler<-function(table, move=NULL, gvar=NULL, tableID="ID", gvarID="ID", gvarOrder="Order", blockHandlers=NULL) {
  if (!is.null(move)) { # only move if a move command is provided
    if (!is.empty(index<-svalue(table, index=TRUE))) { # make sure something is selected
      # block event handlers
      table.blockHandlers(table, handlers=blockHandlers) 
      
      # get new index
      newIndex<-switch(move, "up"=(index-1), "down"=(index+1), "top"=1, "bottom"=nrow(table[])) # find the record to exchange with
      if ( index!=newIndex && newIndex >= 1 && newIndex <= nrow(table[])) { # can move (no point moving outside the table or staying at the same place)
        
        # update global variable if provided
        if (!is.null(gvar)) {
          globalvar<-get(gvar, envir=.GlobalEnv) # get global variable
          
          ID<-table[index, tableID] # figure out which record in the global variable to update
          sort<-globalvar[[gvarOrder]][which(globalvar[[gvarID]]==ID)] # current sort 
          newID<-table[][newIndex, tableID] # figure out ID of new position
          newSort<-globalvar[[gvarOrder]][which(globalvar[[gvarID]]==newID)] # sort of new position
          
          if (move=="top")
            globalvar<-var.updateSorting(globalvar, newSort, sort, orderField=gvarOrder, changeBy=1) # update sorts
          else if (move=="bottom")
            globalvar<-var.updateSorting(globalvar, sort, newSort, changeBy=-1) # update sorts
          else
            globalvar[[gvarOrder]][which(globalvar[[gvarID]]==newID)]<-sort # update exchange item with current sort
          globalvar[[gvarOrder]][which(globalvar[[gvarID]]==ID)]<-newSort
          
          assign(gvar, globalvar, envir=.GlobalEnv) # assign global variable
        }
        
        ### UPDATE TABLE ###
        selectedRow<-table[][index,]
        if (move=="top")
          table[][2:index,]<-table[][1:(index-1),] # move everything down 1 that's above the item
        else if (move=="bottom")
          table[][index:(nrow(table[])-1),]<-table[][(index+1):nrow(table[]),] # move everything up 1 that's below the item
        else # up and down moves
          table[][index,]<-table[newIndex,] # exchange it with the exchange index
        
        # update new index position and select it
        table[][newIndex,]<-selectedRow
        svalue(table, index=TRUE)<-newIndex 
      }
      # unblock event handlers
      table.unblockHandlers(table, handlers=blockHandlers)
    }
  }
}

# provide the buttons for moving data in a table
table.moveButtons<-function(container, table, gvar=NULL, tableID="ID", gvarID="ID", gvarOrder="Order", blockHandlers=c("changed", "clicked") ) {
  moveButtonsGrp<-ggroup(horizontal=FALSE, cont=container)
  addSpring(moveButtonsGrp)
  gbutton(action=gaction("T", icon="gtk-goto-top", handler=function(h,...) table.moveHandler(table, move="top", gvar=gvar, tableID=tableID, gvarID=gvarID, gvarOrder=gvarOrder, blockHandlers=blockHandlers)), cont=moveButtonsGrp) 
  gbutton(action=gaction("U", icon="gtk-go-up", handler=function(h,...) table.moveHandler(table, move="up", gvar=gvar, tableID=tableID, gvarID=gvarID, gvarOrder=gvarOrder, blockHandlers=blockHandlers)), cont=moveButtonsGrp) 
  gbutton(action=gaction("D", icon="gtk-go-down", handler=function(h,...) table.moveHandler(table, move="down", gvar=gvar, tableID=tableID, gvarID=gvarID, gvarOrder=gvarOrder, blockHandlers=blockHandlers)),  cont=moveButtonsGrp)
  gbutton(action=gaction("B", icon="gtk-goto-bottom", handler=function(h,...) table.moveHandler(table, move="bottom", gvar=gvar, tableID=tableID, gvarID=gvarID, gvarOrder=gvarOrder, blockHandlers=blockHandlers)),  cont=moveButtonsGrp)
  addSpring(moveButtonsGrp)
}

####### special tables ########
# table that has toggle abalitiy
# returns a rGtkDataFrame whose data frame content can be accessed by []
table.toggleTable <- function(cont, df, toggleColumn) {
  index <- which(names(df) == toggleColumn) # index of toggle column
  model <- rGtkDataFrame(df) # data frame model
  view <- gtkTreeView() # display structure
  cr <- gtkCellRendererToggle() # cell renderer for the check box
  cr['activatable'] <- TRUE
  
  # add cell renderers to table
  view$insertColumnWithAttributes (0, toggleColumn, cr, active = (index-1) ) 
  mapply(view$insertColumnWithAttributes, -1, colnames(df[-index]), list(gtkCellRendererText()), text = (seq_along(df)[-index]) -1)
  
  # signal for processing cell renderer
  gSignalConnect (cr, "toggled", function (cr, path, user.data ) {
    row <- (as.numeric(path) + 1)
    model <- user.data$getModel()
    model[row, index] <- !model[row, index]
  }, data=view)
  
  # add model
  view$setModel(model)
  
  # add to a scroll window in container
  scrolled_window <- gtkScrolledWindow()
  getToolkitWidget(cont)$packStart(scrolled_window , expand = TRUE, fill = TRUE)
  scrolled_window$add(view)
  scrolled_window$setPolicy ("automatic", "automatic")
  
  # return model table --> can access data by model[]
  return(model)
}


###########
# GENERIC #
# WIDGETS #
###########

# load widgets from a data object
widgets.load<-function(widgets, data) {
  sapply(names(widgets), function(i) {
    if (i%in%names(data)) { #field exists in data
      if (class(widgets[[i]])[[1]]=="gTable") { # gtable style widgets
        if (class(data[[i]])=="list") # single record
          widgets[[i]][] <- data.frame(data[[i]], stringsAsFactors=FALSE)	
        else # multiple records
          widgets[[i]][] <- data[[i]]	
      } else # all other widgets
        svalue(widgets[[i]]) <- data[[i]]
    } else
      print(paste("WARNING: trying to load widget", i, "but no corresponding field found in dataset."))
  })
}

# get widgets into list
widgets.getValues<-function(widgets) {
  return (sapply(names(widgets), function(var) {
      tryCatch(list(widget.getValue(widgets[[var]])),
               warning=function(w) { return(NA) } ) # coerce with NA values      
    }))
}

# get widget value (returns gTable always as data frame)
widget.getValue<-function(widget) {
  if (class(widget)[[1]]=="gTable") #in case it's a gTable(i.e. data frame), have to access info slightly differently
    if (class(widget[])=="list") # single record is returned as a list rather than a dataframe
      return (data.frame(widget[], stringsAsFactors=FALSE))
    else # multiple records in table are returned properly as data frame
      return (widget[]) 
  else
    return(svalue(widget))
}

# get widgets into dataframe (WARNING: will collapse any widgets that are tables down to NULL, if you want to preserve those, use widgets.getValues instead to get a list)
widgets.getValuesAsDF<-function(widgets) {
  return (data.frame(sapply(names(widgets), function(var) {list(svalue(widgets[[var]]))}), stringsAsFactors=TRUE))
}

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




###################################
# code for generic data functions #
# Copyright 2013 Sebastian Kopf   #
# seb.kopf@gmail.com              #
###################################

#################
# data transfer #
#################

# function to output a dataframe to the clipboard (can then be easily copied into excel for example)
cp.copyDF<-function(df) {
  if (exists("writeClipboard")) # windows
    clipboard <- "clipboard"
  else # unix/MacOS
    clipboard <- pipe("pbcopy", "w")
    
  write.table(df, file=clipboard, sep="\t", row.names=FALSE)
    
  if (!exists("writeClipboard")) # unix
    close(clipboard)
}

# function to paste clipboard to data frame
cp.pasteDF<-function(header=TRUE, sep="\t", skip=0, comment.char="#", row.names=NULL, quote=""){
  return(read.clipboard(sep=sep, stringsAsFactors=FALSE, header=header, 
                        skip=skip, comment.char=comment.char, row.names=NULL, quote=quote))
}

#############################
# reading binary data files #
#############################
# NOTE: using the 010 Editor software, figuring out structures will be easier!

# read whole binary file and return the raw data
bin.readFile<-function(path) {
  con<-file(path, "rb")
  rawdata<-readBin(con, raw(), n=file.info(path)$size)
  close(con)
  return(rawdata)
}

# assemble read structure for binary file
# either pass in vectors and it will assemble a dta frame from it or pass in structure to amend
bin.struct<-function(what, length, size = 1, id = NA, struct = NULL) {
  df<-data.frame(id=id, what=what, length=length, size=size, stringsAsFactors=FALSE)
  if (!is.null(struct))
    df<-rbind(struct, df)
  return (df)
}

# provides the total length (in bytes) of a structure
bin.structLength<-function(struct) {
  return(sum(struct$length*struct$size))
}

# read from byte stream (data has to be a byte stream, structure has to be a structure data frame)
# returns a list coded by the ids in the structure
bin.parseStruct<-function(data, struct, offset = 0, saveUnknown = TRUE) {
  results <- list()
  pos <- offset + 1
  size <- length(data)
  for (i in 1:nrow(struct)) {
    id<-struct$id[i]
    if (!is.na(id) || saveUnknown) { # only process if real data or saving unknowns
      
      # different reads
      if (struct$what[i] == "raw")
        read<-paste(readBin(data[pos:size], "raw", n=struct$length[i], size=struct$size[i]), collapse=" ")
      else if (struct$what[i] == "character" && struct$size[i] == 1)
        read<-rawToChar(readBin(data[pos:size], "raw", n=struct$length[i], size=1))
      else if (struct$what[i] == "character" && struct$size[i] == 2)
        read<-paste(readBin(data[pos:size], "character", n=struct$length[i], size=2), collapse="")
      else
        read<-readBin(data[pos:size], struct$what[i], n=struct$length[i], size=struct$size[i])
      
      # saving results
      if (is.na(id))
        id <- "unknown"
      if (!is.null(results[[id]]))
        results[[id]]<-c(results[[id]], list(read))
      else
        results[[id]]<-read
    }
    pos <- pos + struct$size[i] * struct$length[i]
  }
  return(results)
}

# optimized method for repeat reading of the same structure (say to read an entire data array)
# WARNING: only supported for structures where each element is of length=1 (otherwise, mayhem!)
bin.multiParseStruct<-function(data, struct, rep, offset = 0) {
  datalength <- bin.structLength(struct)
  subdata<-data[(offset+1):(offset+rep*datalength)]
  structpos<-0
  df<-data.frame(read = 1:rep)
  for (i in 1:nrow(struct)) {
    byteselect<-rep(FALSE, datalength)
    byteselect[(structpos+1):(structpos<-structpos+struct$size[i])]<-TRUE
    if (!is.na(struct$id[i]))
      df[struct$id[i]]<-readBin(subdata[which(rep(byteselect, times=rep))], struct$what[i], size=struct$size[i], n=rep)
  }
  return (df)
}

# find all ascii strings in a data stream
# FIXME: it appears that after each string, there are 3x null character (i.e. 00 00 00) --> use this to make finding strings better! (couldn't quite figure out how to recognize 00 characters)
bin.findAllAscii<-function(data, minlength=10) {
  regexp<-paste("[\u0020-\u007e]{", minlength, ",}", sep="")
  text<-data.frame(
    byteStart = grepRaw(regexp, data, all=TRUE), #get ANSII strings
    value = ldply(grepRaw(regexp, data, all=TRUE, value=TRUE), 
                  function(x) rawToChar(x))$V1, encoding='ASCII', stringsAsFactors=FALSE)
  text$byteEnd<-text$byteStart + nchar(text$value) - 1
  text$byteLength<-text$byteEnd - text$byteStart + 1
  text$strLength<-text$byteLength
  return (text)
}

# find all unicode strings in a binary data stream
bin.findAllUnicode<-function(data, minlength=5) {
  regexp<-paste("([\u0020-\u007e][^\u0020-\u007e]){", minlength, ",}", sep="")
  text<-data.frame(
    byteStart = grepRaw(regexp, data, all=TRUE), #get Unicode strings
    value = ldply(grepRaw(regexp, data, all=TRUE, value=TRUE), 
                  function(x) rawToChar(x[c(TRUE, FALSE)]))$V1,
    #paste(readBin(x, "character", n=length(x)/2, size=2), collapse=""))$V1, 
    encoding='Unicode', stringsAsFactors=FALSE)
  text$byteEnd<-text$byteStart + nchar(text$value) * 2 - 1
  text$byteLength<-text$byteEnd - text$byteStart + 1
  text$strLength<-text$byteLength/2
  return (text)
}

# find all text in a binary data stream
bin.findAllText<-function(data, asciiL=10, unicodeL=5) {
  text<-rbind(bin.findAllAscii(data, minlength=asciiL), bin.findAllUnicode(data, minlength=unicodeL))
  text<-text[order(text$byteStart),] # sort all text
  text$byteGap<-diff(sort(c(text$byteStart, text$byteEnd, length(data))))[c(FALSE,TRUE)] # add byte gap
  return(text)
}

# clean up text by removing randomly found strings that are clearly not proper targets
bin.cleanText<-function(text, removeText="Arial", removePattern = "[&{}!^@?#]", unlessByteLength = 26, unlessText = "Is Ref.?", printRemoved = TRUE) {
  rem<-union(
    which(text$value==removeText),
    intersect(grep(removePattern, text$value), which(text$byteLength < unlessByteLength & !(text$value%in%unlessText))))
  if (printRemoved) {
    cat("\nRemoved:\n")
    print(text[rem,"value"])
  }
  text<-text[-rem,]
  # re calculate gaps between occuring strings
  text$byteGap<-diff(sort(c(text$byteStart, text$byteEnd, max(text$byteEnd))))[c(FALSE,TRUE)] # add byte gap
  return (text)
}

####################################
# data frame convenience functions #
####################################

# delete certain columns from data frame (by name or indices)
df.delCols<-function(df, cols) {
  if (mode(cols)=="numeric") #referring to indices
    return (df[-cols])
  else
    return (df[!names(df) %in% cols]) #referring to names 
}

# rename certain columns
# if "from" is omitted, "to" is expected to contain names for all columns
df.nameCols<-function(df, to, from) {
  if (missing(from)) 
    names(df) <- to
  else {
    for (i in 1:length(from)){
      if (!is.empty(col<-which(names(df)==from[i])))
        names(df)[col]<-to[i]
    }
  }
  return (df)
}

# change the data type of several columns
# types include integer and numeric
df.changeDataType<-function(df, cols, type="numeric") {
  for (i in 1:length(cols))
    mode(df[,cols[i]])<-type
  return (df)
}

# convert all numeric columns in a data frame to true numeric
df.convertNumerics<-function(df) {
  for (i in which(sapply(df, is.numeric))) df[[i]]<-as.numeric(df[[i]])
  return(df)
}

# change date/time to time diff (makes a column with the name of units)
# baseDT
#	- if omitted, take first entry as reference
#	- if int, take this element of the dataset as BT
#	- actualy date time --> make by strToDateTime("<date>","<time>")
# valid units --> "hours", "days" (probably some others too)
df.dateTimeToDiff<-function(df, baseDT, col=c("date","time"), units="hours", format="%m/%d/%y %H:%M") {
  datetime<-strToDateTime(do.call(paste,c(df[col])), format=format)
  if(missing(baseDT)) baseDT<-datetime[1]
  if(mode(baseDT)=="numeric") baseDT<-datetime[baseDT]
  df[units]<-timeDiff(datetime,baseDT,units=units)
  return(df)
}

# add experimental info ro reshaped data set
# col = name of new column, e.g. "strain" (or old column if want to overwrite, resetcol if you want to reset the whole thing)
# info = list coded with the new information
#   e.g. list("PA14"=c(1,4,5), "MAI-1"=c(2,3)), where 1-5 are the ids of the different experimental conditions (after doing df.reshapeAvgs or reshapeRawData)
# regexp (default: FALSE), how to look for these values, by default it looks by fixed string, set regexp to TRUE to evaluate a regular expression, NOTE: additional parameters are passed to the regexp function
# resetcol ( default: TRUE) --> resets the column entirely, otherwise only valuse specified by the matching conditions are set
# asnumeric --> decide whether the newly created values are numerics
df.addInfoCol<-function(df, col, info, id="exp", regexp=FALSE, resetCol=FALSE, asNumeric=FALSE, ...) {
  if (resetCol==TRUE || !(col%in%names(df)))
    df[col]<-NA # make new column / overwrite existing data in it
  for (i in 1:length(info)) {
    value<-names(info)[i]
    idvals<-info[[i]]
    for (idval in idvals) {
      if (regexp)
        matches<-grep(idval, df[[id]],...)
      else
        matches<-which(df[[id]]==idval)
      if (!is.empty(matches)) df[matches,col]<-value
    }
  }
  if (asNumeric)
    df[[col]]<-as.numeric(df[[col]])
  return(df)
}


################################
# generic data variable access #
################################

# get index by id
var.getIndexByID<-function(var, id, idField="ID") {
  if (!is.null(id) && !is.empty(sel<-which(var[[idField]]==id)))
    return(sel) #selected index in dataset
  return (NULL)
}

# get entry by id
var.getEntryByID<-function(var, id, idField="ID", fields=NULL) {
  if (!is.null(index<-var.getIndexByID(var, id, idField=idField))) {
    if(class(var) == "data.frame") { # data.frame
      if (is.null(fields)) 
        return (var[index,]) # return record
      else
        return (var[index, fields]) # return specific fields
    } else if (class(var) == "list") { #list
      if (is.null(fields)) 
        return (lapply(var,"[[",index))
      else if (length(fields) == 1) # return just this one field
        return (var[[fields]][[index]])
      else # multiple records
        return (lapply(var[fields],"[[",index))
    } else
      stop("data structure type not supported")
  }
  return (NULL)
}

# new object with fields whatever is passed in
# returns a list
var.new<-function(...) {
  return (list(...))
}

# new object with fields whatever is passed in
# returns as data frame
var.newAsDF<-function(...){
  return (data.frame(var.new(...), stringsAsFactors=FALSE))
}

# add entry in variable
# returns var
# supports reordering if desired (just supply currentOrderN to insert after this position)
# FIXME: ordering not tested/supported(?) for lists
var.add<-function(var, newId, data, idField="ID", orderField="Order", orderN=NULL) {
  if ( !(newId%in%var[[idField]])) {
    # reordering:
    if (!is.null(orderN)) { # insert at this orderN
      var<-var.updateSorting(var, orderN, orderField=orderField) # update all orderNs
      data[[orderField]]<-orderN
    } 
    # insert new entry
    if (class(var) == "data.frame") {
      newIndex<-nrow(var)+1
      var[newIndex,idField]<-newId
    } else if (class(var) == "list") {
      newIndex<-length(var[[idField]])+1
      var[[idField]][[newIndex]]<-newId
    }
    var<-var.update(var, newId, data, idField=idField)
    return (var)
  } else 
    stop(paste("ERROR: failed to add data set with new ID", newId, "because this ID already exists in the dataset."))
}

# update entry in variable (by default identified with idField but can go via index)
# returns updated variable
# WARNING: this might only work well with data frame fields that are NOT FACTORS
var.update<-function(var, id, data, idField="ID") {
  if (!is.null(index<-var.getIndexByID(var, id, idField=idField))) {
    for (field in names(data))
      if (field%in%names(var)) { # update varialbe
        # get new value (either from list or data frame)
        if (class(data)=="data.frame")
          newValue<-data[1,field]
        else if (class(data)=="list")
          newValue<-data[[field]]
        
        # set value depending on what you're updating (list or data frame)
        if (class(var)=="data.frame")
          var[index,field]<-newValue
        else if (class(var)=="list")
          var[[field]][[index]]<-newValue
        
      } else
        print(paste("WARNING: trying to update table field", field, "with value", data[[field]], "but field does not exist in variable.")) 
  }
  return (var)
}

# delete entry
# returns updated variable
var.delete<-function(var, id, idField="ID") {
  if (!is.null(index<-var.getIndexByID(var, id, idField=idField))) {
    if (class(var)=="data.frame") # delete record from data.frame
      var<-var[-var.getIndexByID(var, id, idField=idField),] 
    else if (class(var)=="list") { # delete record from list
      for (field in names(var)) {
        if (class(var[[field]])=="list") #in case it's a sublist, need to set elements to NULL
          var[[field]][[index]]<-NULL
        else # in case it's a direct array (e.g. numeric, character, logical)
          var[[field]]<-var[[field]][-index]
      }
    }
  }
  return(var) 
}

################
# data sorting #
################

# update order in a variable
# updateN.from - the ordering number from which on (inclusive, >=) all sorting numbers should be updated 
# updateN.to - if not NULL (default), the ordering number until which (inclusive, <=) all ordering numbers should be updated
#            - if NULL, goes till the end (nrow(table[]))
# returns the updated variable
# can changeBy 1 or -1 depending on need
var.updateSorting<-function(var, updateN.from, updateN.to=NULL, orderField="Order", changeBy=1) {
  if (is.null(updateN.to))
    updateN.to<-max(var[[orderField]], na.rm=TRUE)
  if ( !is.null(updateN.from) && !is.empty(indices<-which(var[[orderField]]>=updateN.from & var[[orderField]]<=updateN.to)) ) 
    var[[orderField]][indices]<-var[[orderField]][indices]+changeBy
  return (var)
}


#####################################
# Utility functions  			        	#
# Copyright 2013 by Sebastian Kopf  #
#####################################

######################
# variable functions #
######################

# get all variables/functions/everything defined in the passed in environment as a list with the variable name = variable object
# e.g. to get a list of all variables in a function, just call ls.asList(environment()) from within the function
# params
#  env = the environment to get do ls() on
#  exclude - a vectr of variable/function names to exclude from the list
ls.asList<-function(env, exclude=c()) sapply(setdiff(ls(env=env), exclude), FUN=function(i) list(get(i, env=env)))

#############################
# small scale utility funcs #
#############################

# checks if lenght of the variable is 0 and returns TRUE if it is
# also returns TRUE if variable is NA or NULL
is.empty<-function(variable)
  return (is.null(variable) || is.na(variable) || length(variable) == 0)

# convert strings to date time
strToDateTime<-function(dateStr, timeStr, format="%m/%d/%y %H:%M"){
  if(!missing(timeStr)) dateStr<-paste(dateStr,timeStr)
  return(strptime(dateStr, format))
}

#timeDiff function
timeDiff<-function(dateTime, baseDT, units="days")   
  return (as.numeric(difftime(dateTime,rep(baseDT,length(dateTime))),units))



#####################################
# Graphing functions        				#
# Copyright 2013 by Sebastian Kopf  #
# These are not all mine, sources   #
# indicated where taken from else-  #
# where                             #
#####################################

##############################
# overwrite funtions         #
# fixme: removed as soon as  #
# fixed in package           #
##############################
gglocator<-function (n = 1, object = last_plot(), message = FALSE, xexpand = c(0.05, 0), yexpand = c(0.05, 0)) {
  if (n > 1) {
    df <- NULL
    for (k in 1:n) {
      df <- rbind(df, gglocator(object = object, message = message, 
                                xexpand = xexpand, yexpand = yexpand))
    }
    return(df)
  }
  x <- grid.ls(print = message)[[1]]
  #x <- x[grep("panel-", grid.ls(print = message)[[1]])] #SK CHANGE
  x <- grid.ls()[[1]][grep("panel", grid.ls(print = message)[[1]])][1] #SK CHANGE
  seekViewport(x)
  loc <- as.numeric(grid.locator("npc"))
  xrng <- with(object, range(data[, deparse(mapping$x)]))
  yrng <- with(object, range(data[, deparse(mapping$y)]))
  xrng <- scales::expand_range(range = xrng, mul = xexpand[1], 
                               add = xexpand[2])
  yrng <- scales::expand_range(range = yrng, mul = yexpand[1], 
                               add = yexpand[2])
  point <- data.frame(xrng[1] + loc[1] * diff(xrng), yrng[1] + 
                        loc[2] * diff(yrng))
  names(point) <- with(object, c(deparse(mapping$x), deparse(mapping$y)))
  point
}


####### useful constants #########
#color-blind friendly palettes:
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# Preferred shapes for filling (they can take a fill colour)
prefShapes<-21:25
# e.g.: #ggplot(data.frame(x=1:5, y=10:14, colour=1:5), aes(x=x, y=y, fill=factor(colour), shape=factor(colour))) + geom_point(colour="black", size=5) + scale_shape_manual(values=21:25)

# simple histogram whisker plot combination function 
# data=supplied as a simple vector
plot.whiskerhist<-function(data, xlab, filename="whiskerhist.pdf") {
	maxF=max(hist(data)$counts)
	pdf(filename)
	h<-hist(data,main='',xlab=xlab,ylim=c(0,1.5*maxF))
	boxplot(data,horizontal=TRUE,at=1.125*maxF,add=TRUE,axes=FALSE)
	stripchart(data,add=TRUE,at=1.375*maxF)
	axis(3)
	dev.off()
}

# error bars
plot.addErrorBars<-function(q, err, y, color="black", width=1, linetype=1) {
	if (is.na(color) && is.na(linetype))
		q<-q + geom_errorbar(aes_string(ymax = paste(y,"+",err), ymin=paste(y,"-",err)),width=width)
	else if (is.na(linetype))
		q<-q + geom_errorbar(aes_string(ymax = paste(y,"+",err), ymin=paste(y,"-",err)),width=width,color=color)
	else if (is.na(color))
		q<-q + geom_errorbar(aes_string(ymax = paste(y,"+",err), ymin=paste(y,"-",err)),width=width,linetype=linetype)
	else
		q<-q + geom_errorbar(aes_string(ymax = paste(y,"+",err), ymin=paste(y,"-",err)),width=width,color=color,linetype=linetype)
	return(q)
}

# scales
plot.addAxes<-function(q, xlim, xbreaks, ylim, ybreaks) {
	if(length(xlim)==2 && length(ylim)==2) { 
		q<-q+coord_cartesian(xlim=xlim, ylim=ylim)
		q<-q+scale_y_continuous(breaks=ybreaks)+scale_x_continuous(breaks=xbreaks)
	} else if(length(ylim)==2) {
		q<-q+coord_cartesian(ylim=ylim)
		q<-q+scale_y_continuous(breaks=ybreaks)
	} else if (length(xlim)==2) {
		q<-q+coord_cartesian(xlim=xlim)
		q<-q+scale_x_continuous(breaks=xbreaks)
	}
	return (q)	
}

# simple background
plot.addSimpleBgrd<-function(q)
	return (q + opts(panel.background = theme_rect(colour = 'black', size = 1.5, linetype='solid'), panel.grid.major = theme_line(colour = 'gray', size = 0.5, linetype = 'solid')))

# standard labels
plot.addLabels<-function(q,main,xlab,ylab,colLegend=NULL,shapeLegend=NULL, lineLegend=NULL, fillLegend=NULL) 
	return (q + opts(title = main) + labs(y = ylab, x = xlab, colour=colLegend, shape=shapeLegend, linetype=lineLegend, fill=fillLegend))

########################
# complex graphs funcs #
########################

# q = a ggplot object which has 
#	- its colour aesthetic set (even if it's =factor(ID) and all colours passed in the colours argument are the same)
# 	- its size aesthetic set to the same distinguisher as the color aesthestic (e.g. some ID, use factor(ID) if discrete)
#	- no geom_line OR geom_line(size=x) called BEFORE geom_point() ;(size has to be fixed in geom_line attribute)
# colors = color_palette used, e.g. c("black", "blue", "red", "dark green")
# order = the order (relative to the colors provided) in which the graphs should appear, e.g. c(4,1,3,2)
# autosave = TRUE: save each plot automatically, FALSE: ask whether to save it
# filename = base filename (number in the animation sequence is added at the end), omit ".pdf"
# ... = additional arguments are all passed on to ggsave (e.g. width, height, dpi)
ggplot.animate<-function(q, order, colors, psize=2.5, autosave=FALSE, filename="animate", ...){
	if (max(order)>length(colors) || min(order) < 1) {
		print("ERROR: order includes indices that are not available. If you provide 4 colours, only provide order numbers 1 through 4, e.g. c(1,4,2,3).")
		return()
	}
	animateSizes<-rep(0,times=length(colors))
	animateColors<-rep("white", times=length(colors))
	for(i in 0:length(order)) {
		if (i>0){
			animateSizes[order[i]]<-psize
			animateColors[order[i]]<-colors[order[i]]
		}
		print(q + scale_size_manual(values=animateSizes, legend=FALSE) + scale_colour_manual(values=animateColors))
		if (autosave==TRUE) {
			save <- "y"
		} else {
			save <- ask(paste("Would you like to save this plot (#", i, ")? Type y for yes, enter for no.", sep=""))
		}
		if (save=="y") {
			name<-paste(filename,"_",i,".pdf",sep="")
			print(paste("Saving plot #", i, " as ", name, sep=""))
			ggsave(filename=name,...)
		}
	}
}

##### BORROWED MULTIPLOT FUCTION #####
# from R cookbook
# Multiple plot function
# NOTE: the layout structure is extremely useful!!
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



####################
# simplicity funcs #
# for + syntax use #
# TODO: figure out how to use multiple ggplot commands (i.e. the + syntax)
# TODO: upgrade to new ggplo2 syntax (a lot of commands are now deprecated)
####################


# make black and white theme (call before calling any other theme vars, sucha s nogrid, adjustX, etc.)
gg.bw<-function() theme_bw()

# removing grid (call after gg.bw, otherwise gg.bw recreates the lines)
gg.nogrid<-function() return(opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank()))

# add title
gg.title<-function(title) opts(title=title)

# adjust x axis position
gg.adjustX<-function(vjust=-0.5, hjust=0, size=12) opts(axis.title.x = theme_text(vjust=vjust, hjust=0.5+hjust, size=size))

# adjust y axis position (note to self: maybe allow adjusting face="bold"?)
gg.adjustY<-function(vjust=0.2, hjust=0, size=12, angle=90) opts(axis.title.y = theme_text(vjust=vjust, hjust=0.5+hjust, angle=angle, size=size))

# change margins
# recommmend using this together with gg.noLegend, otherwise need to adjust the right parameter substantially
gg.noMargins<-function(ylabel=TRUE, xlabel=TRUE, top=0.2, right=-0.5, bottom=-1, left=-1) {
	if (ylabel==TRUE) left<-left+1
	if (xlabel==TRUE) bottom<-bottom+1
	opts(plot.margin = unit(c(top,right,bottom,left), "lines"))
}

# remove legend
gg.noLegend<-function() opts(legend.position="none")


