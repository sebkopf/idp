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