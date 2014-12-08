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