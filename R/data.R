##################
# Data functions #
##################

# find peak index of peak that belongs to the x coordinate (return NULL if no peak associated with this)
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

# load tab
IDP.loadIsodatFileTab<-function(idp, fileTabObj) {
  IDP.loadPeakTable(idp, fileTabObj$peakTable) # load peak table
  widgets.load(idp$gui$fileInfo, fileTabObj$fileInfo) # load regular file information
  if (svalue(idp$gui$fileInfo.nb) == 2) # refs tab selected
    IDP.plotRefs(idp, peakTable = fileTabObj$peakTable) # load references
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
    peakNr<-str_match(fileObj$peakTable[,2],"^([0-9]+)([\\*\\+]?)$")
    fileObj$peakTable$ID<-NA
    fileObj$peakTable[,2]<-as.integer(peakNr[,2])
    fileObj$peakTable$RefPeak<-(peakNr[,3]=="*") # whether it is a reference peak
    fileObj$peakTable$Status<-sapply(peakNr[,3], FUN=function(x) { if (x=="+") "Added" else "Auto" }) # whether peak was modified
    fileObj$peakTable<-fileObj$peakTable[,c(1,2,29,30,28,3:27)] # reorder
    names(fileObj$peakTable)<-IDP.getSettings(idp, "peakTableColumns")$Name # assigne names
    for (i in which(IDP.getSettings(idp, "peakTableColumns")$Type=="numeric")) 
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


###############
# excel files #
###############

# reads a data frame from an excel sheet with the headers provided in the startRow
# trueColNames - gives the data frame the real names from the columns
excel.readDF <- function(file, sheet = 1, startRow = 1, stringsAsFactors=FALSE, trueColNames = TRUE) {
  df <- read.xlsx2(file, sheet, startRow=startRow, stringsAsFactors=stringsAsFactors, header=TRUE) 
  if (trueColNames) {
    dfcols <- read.xlsx(file, sheet, rowIndex=startRow, header=FALSE, stringsAsFactors=stringsAsFactors) 
    names(df) <- gsub("\\s*$", "", dfcols, perl=T) # trailing whitespaces removed
  }
  return(df)
}

# write data frames to an excel sheet
# file = file name/path
# df = either
#   - a data frame (data.frame(x=.., y=...))
#   - a list of data frames with the list IDs as sheet names (list("Sheet 1" = data.frame(x=.., y=...)))
excel.writeDF <- function(file, df) {
  wb <- createWorkbook(type="xlsx")
  csStd <- CellStyle(wb) + Font(wb)
  csBold <- CellStyle(wb) + Font(wb, isBold=TRUE) 
  if (identical(class(df), "data.frame")) { # single data frame
    sheet <- createSheet(wb, sheetName="Sheet1")
    addDataFrame(df, sheet, startRow=1, startColumn=1, colnamesStyle=csBold, row.names=FALSE, colStyle=list(`2`=csStd, `3`=csStd))
  } else if (identical(class(df), "list")) { # multiple data frames
    for (dfi in 1:length(df)) {
      sheetName <- names(df)[dfi]
      if (identical(sheetName, "")) # no sheet name given
        sheetName <- paste0("Sheet", dfi)
      sheet  <- createSheet(wb, sheetName=sheetName)
      addDataFrame(df[[dfi]], sheet, startRow=1, startColumn=1, colnamesStyle=csBold, row.names=FALSE, colStyle=list(`2`=csStd, `3`=csStd))
    }
  }
  saveWorkbook(wb, file) 
}

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
