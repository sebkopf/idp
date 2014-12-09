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

