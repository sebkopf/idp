#' Isodat Data Processor (IDP)
#' 
#' User interface to facilitate interacting with isodat data.
#' 
#' @name idp-package
#' @aliases idp
#' @docType package
#' @title idp package
#' @author Sebastian Kopf
#' @seealso \code{\link{idp.start}}
#' @examples
#' \dontrun{\code{idp.start()}}
#' 
#' @include gui.R
#' @include widgets.R
#' @include notebook.R
#' @include plots.R
#' @include read.R
#' @include table.R
#' @include export.R
#' @include utils.R
#' @include data.R
NULL

#' Start in development mode
#' @export
idp.dev <- function() {
  return(idp.start(load = "idpdev", askReload = FALSE))
}

#' Start the Isodat Data Processor
#' @param load previous instance of the program settings stored in this global variable
#'    NULL if never want to load previous settings
#' @param askReload whether to ask the user if they want to reload the previous instance 
#'  or just reload it without asking
#' @param parent if launching as part of a bigger application 
#' @param modal whether to run as a modal dialog (necessary if from a script!!)
#' @return invisible instance of the idp (can be used for calling things directly)
#' @export
idp.start <- function(load = "idp_settings", askReload = TRUE, parent=NULL, modal=FALSE) {
  
  # load RGtk2
  options("guiToolkit"="RGtk2")
  
  # make sure the right id function (from the widgets package) is always used
  id <- gWidgets::id
  
  # inform user
  message("\nLaunching IDP... please wait...")
  
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
  
  # set parent and modal
  idp$gui$parent<-parent
  idp$gui$modal<-modal
  
  # launch gui
  idp <- IDP.gui(idp)
  
  return (invisible(idp))
}

#' Start the Isodat Data Processor with an Rscript
#' 
#' Same as idp.start except it makes sure to keep the terminal alive with
#' a loop until the program actually exits
#' @export
idp.start_from_script <- function(..., askReload = FALSE) {
  if (file.exists("./idp.RDATA")) {
    load("idp.RDATA", envir=.GlobalEnv)
    message("\n\nReading user settings from idp.RDATA")
  }
  obj <- idp.start(..., modal = TRUE, askReload = askReload)
  var_name <- obj$settings$gvar
  if (exists( var_name)) {
    message("\nSaving user settings to idp.RDATA\n\n")
    do.call(save, args = list(var_name, file = "idp.RDATA"))
  }
}

################
# Initializing #
################

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
  idp$settings$leftPane <- 0.5 # position of left pane top vs bottom
  idp$settings$rightPane <- 0.6 # position of right pane top vs bottom
  idp$settings$centerPane <- 0.25  # position of center pane left vs right
  
  # plotting options
  idp$settings$plotOptions<-list(
    labels = list(x="Time", y="Signal"), 
    xUnits = list(value = 1, ids = c("XaxisSec", "XaxisMin"), labels = c("s", "min"), funcs = c(function(x) x, function(x) x/60), revFuncs = c(function(x) x, function(x) x*60)),
    yUnits = list(value = 1, ids = c("YaxismV", "YaxisV"), labels = c("mV", "V"), funcs = c(function(x) x, function(x) x/1000)), # y units not currently implemented
    trace2 = list(on = TRUE, color="black", offset=200), #offset in mV
    trace3 = list(on = TRUE, color="dark green", offset=0),
    markRefs = TRUE, # whether standards should be marked
    baseMarker = list(on = TRUE, color="red"), #  not implemtend yet
    apexMarker = list(on = TRUE, color="blue"),
    edgeMarker = list(on = FALSE, color="blue"),
    zoomBuffer = 0.05,  # when it will be considered to be a y zoom or just x zoom (currently 5%)
    zoomIn = 0.5, # how much to zoom in (50%)
    zoomOut = 2, # how much to zoom out (100%)
    zoomMove = 0.2 # how much to move interval (20%) 
  )
  
  # FIXME: implement ordering here
  idp$settings$peakTableColumns<-data.frame(
    Name=c("Filename" ,"PeakNr", "RefPeak", "Status", "ID" ,"Component" ,"Master" ,"RefName" ,"Start" ,"Rt" ,"End" ,"Width" ,"Amp2" ,"Amp3" ,"BGD2" ,"BGD3" ,"AreaAll" ,"Area2" ,"Area3" ,"rAreaAll" ,"rArea2" ,"rArea3" ,"R3H2v2H2" ,"rR3H2v2H2" ,"rd3H2v2H2" ,"d3H2v2H2" ,"DeltaDelta3H2v2H2" ,"R2H1H" ,"d2H1H" ,"AT2H1H"),
    Column=c("Filename" ,"Peak Nr.", "Ref. Peak", "Status", "ID" ,"Component" ,"Master Peak" ,"Ref. Name" ,"Start" ,"Rt" ,"End" ,"Width" ,"Ampl. 2" ,"Ampl. 3" ,"BGD 2" ,"BGD 3" ,"Area All" ,"Area 2" ,"Area 3" ,"rArea All" ,"rArea 2" ,"rArea 3" ,"R 3H2/2H2" ,"rR 3H2/2H2" ,"rd 3H2/2H2" ,"d 3H2/2H2" ,"DeltaDelta 3H2/2H2" ,"R 2H/1H" ,"d 2H/1H" ,"AT% 2H/1H"),
    Units=c("" ,"" ,"" , "", "", "","" ,"" ,"[s]" ,"[s]" ,"[s]" ,"[s]" ,"[mV]" ,"[mV]" ,"[mV]" ,"[mV]" ,"[Vs]" ,"[Vs]" ,"[Vs]" ,"[mVs]" ,"[mVs]" ,"[mVs]" ,"" ,"" ,"[per mil] vs. ref" ,"[per mil] vs. VSMOW" ,"" ,"" ,"[per mil] vs. VSMOW" ,"[%]"),
    Type=c("character", "integer", "logical", "character", "integer", "character", "character", "character", rep("numeric", 22)),
    Show=TRUE, Required=FALSE, IsodatCol=TRUE, stringsAsFactors=FALSE)
  idp$settings$peakTableColumns[2, "Required"]<-TRUE # Peak Nr. is required b/c it's the ID
  idp$settings$peakTableColumns[3:5, "IsodatCol"]<-FALSE # Columns not present in isodat
  idp$settings$peakTableColumns[3:5, "Show"]<-FALSE # do not show these by default
  return (idp)
}

###########################
# Change program settings #
###########################

# change settings on the idp object
IDP.changeSettings<-function(idp, settings) {
  #FIXME - implement me
  return (idp)
}

# get settings (all or selected subset)
# settings = c("setting1", "setting2")
IDP.getSettings<-function(idp, settings=NULL) {
  if (is.null(settings))
    return (tag(idp$gui$win, "settings"))
  else if (length(settings)==1)
    return (tag(idp$gui$win, "settings")[[settings]])
  else
    return (tag(idp$gui$win, "settings")[settings])
}

# set settings
# settings - list of settings to change
# --> list(gvar="1.2", fileDirectory="/Users/")
# HINT: understands . as a level separator
# --> e.g., list(plotOptions.zoomIn = 0.3, plotOptions.baseMarker.on = FALSE) 
IDP.setSettings<-function(idp, settings) {
  for (var in names(settings)) {
    levels <- unlist(strsplit(var, "\\."))
    evalStr <- paste(paste("[[levels[", 1:length(levels), "]]]", sep=""), collapse="")
    evalstr <- paste("tag(idp$gui$win, 'settings')", evalStr, "<-settings[[var]]", sep="")
    eval(parse(text=evalstr))
  }
}



#######################
# Saving to workspace #
#######################

# saves idp instance to the gvar stored in it
IDP.save <- function(idp) {
  save <- IDP.init()
  IDP.setSettings(idp, list(
    leftPane = svalue(idp$gui$gl), 
    rightPane = svalue(idp$gui$gr), 
    centerPane = svalue(idp$gui$gall)))
  save$settings <- IDP.getSettings(idp) # get newest settings back from window instance
  
  # FIXME: also save data
  # Note: not saving any of the gui components (no need to keep the pointers)
  assign(idp$settings$gvar, save, envir=.GlobalEnv)
}


