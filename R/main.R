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
NULL

#' Start in development mode
#' @export
idp.dev <- function() {
  return(idp.start(load = "idpdev", askReload = FALSE))
}

#' Start the Isodat Data Processor
#' @param load previous instance of the program OR name of the global variable to look for
#' @param askReload whether to ask the user if they want to reload the previous instance 
#'  or just reload it without asking
#' @return invisible instance of the idp (can be used for calling things directly)
#' @export
idp.start <- function(load = NULL, askReload = TRUE) {
  
  # load RGtk2
  options("guiToolkit"="RGtk2")
  
  # make sure the right id function (from the widgets package) is always used
  id <- gWidgets::id
  
  # inform user
  message("\nLaunching IDP... please wait...\n\n")
  
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
    
  # launch gui
  idp <- IDP.gui(idp)
  
  return (invisible(idp))
}

#' Start the Isodat Data Processor with an Rscript
#' 
#' Same as idp.start except it makes sure to keep the terminal alive with
#' a loop until the program actually exits
#' @export
idp.start_from_script <- function(...) {
  
  stop("not implemented yet!")
  
  idp.start(...)
  
  # run loop until the program is finished
  message("\n\nIDP running modally. Have fun!")
  while (IDP.running) {
    # FIXME IMPLEMENT THIS!!! HERE !!!
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
  idp$settings$gvar <- "idp_settings" # global variable name
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

###########################
# Change program settings #
###########################

# change settings on the idp object
IDP.changeSettings<-function(idp, settings) {
  #FIXME - implement me
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


