# export current tab to excel
# --> exports all data
idp.exportFileToExcel <- function(idp) {
  f=gfile("Select where to save this excel export.", type="save", cont=idp$gui$win, 
          initialfilename = paste(format(Sys.time(),format="%Y%m%d"),"_", pn.getSelectedPlotTabName(idp$gui$pn),".xlsx", sep=""),
          filter = list("Excel Files" = list(patterns=c("*.xlsx", "*.xls")), "All files" = list(patterns = c("*"))))
  
  if (!is.na(f)){
    if (length(grep("\\.xlsx$", f))==0) f<-paste(f,".xlsx",sep="") # ensure .xslx ending
    
    # status
    IDP.showInfo(idp, paste("Exporting to ", f, "...", sep=""), timer=NULL, okButton=FALSE)
    
    # info
    info <- pn.getAllInfo(idp$gui$pn)
    
    # workbook
    wb <- createWorkbook(type="xlsx")
    
    # styles
    csStd <- CellStyle(wb) + Font(wb)
    csItalic <- CellStyle(wb) + Font(wb, isItalic=TRUE) 
    csBold <- CellStyle(wb) + Font(wb, isBold=TRUE) 
    
    # data
    sheetPT  <- createSheet(wb, sheetName="Peak Table")
    addDataFrame(data.frame(File=info$fileInfo$Filename), sheetPT, startRow=1, startColumn=1, 
                 colnamesStyle=csBold, row.names=FALSE, colStyle=list(`2`=csStd, `3`=csStd))  
    
    if (!is.null(info$peakTable)) # add peak table
      addDataFrame(IDP.getDataPeakTable(idp, info$peakTable), sheetPT, startRow=4, startColumn=1, 
                   colnamesStyle=csBold, row.names=FALSE, colStyle=list(`2`=csStd, `3`=csStd))
    
    sheetDT  <- createSheet(wb, sheetName="Data Trace")
    addDataFrame(info$data, sheetDT, startRow=1, startColumn=1, 
                 colnamesStyle=csBold, row.names=FALSE, colStyle=list(`2`=csStd, `3`=csStd))
    
    saveWorkbook(wb, f) 
    
    # finished
    IDP.showInfo(idp, paste(f, "saved successfully."), timer=2, okButton=FALSE)
  }
}

# export to excel
idp.exportAllPeakTablesToExcel<-function(idp, saveAll = FALSE){
  f=gfile("Select where to save this excel export.", type="save", cont=idp$gui$win, 
          initialfilename = paste(format(Sys.time(),format="%Y%m%d"),"_peak_tables.","xlsx", sep=""),
          filter = list("Excel Files" = list(patterns=c("*.xlsx", "*.xls")), "All files" = list(patterns = c("*"))))
  
  if (!is.na(f)){
    if (length(grep("\\.xlsx$", f))==0) f<-paste(f,".xlsx",sep="") # ensure .xslx ending
    wb <- createWorkbook(type="xlsx")
    sheetPT  <- createSheet(wb, sheetName="Isodat Data Processor") # FIXME, consider giving option to do separate tabs for each
    
    # styles
    csStd <- CellStyle(wb) + Font(wb)
    csItalic <- CellStyle(wb) + Font(wb, isItalic=TRUE) 
    csBold <- CellStyle(wb) + Font(wb, isBold=TRUE) 
    
    # all data
    rownum <- 1
    df <- NULL
    for (i in 1:length(idp$gui$pn$plot.nb)) {
      info <- pn.getPlotTabParam(idp$gui$pn, i, params="plotinfo")
      
      # FIXME, maybe merge this code with code for single export file?
      #addDataFrame(data.frame(File=info$fileInfo$Filename), sheetPT, startRow=rownum, startColumn=1, 
      #             colnamesStyle=csBold, row.names=FALSE, colStyle=list(`2`=csStd, `3`=csStd))  
      #rownum <- rownum + 3
      
      if (!is.null(info$peakTable)) { # add peak table
        idf <- IDP.getDataPeakTable(idp, info$peakTable)
        if (is.null(df)) df<-idf
        else df <- rbind(df, idf)
        
        #rownum <- rownum + 2 + nrow(info$peakTable)
      }
      #rownum <- rownum + 1
    }
    addDataFrame(df, sheetPT, startRow=rownum, startColumn=1, 
                 colnamesStyle=csBold, row.names=FALSE, colStyle=list(`2`=csStd, `3`=csStd))
    
    saveWorkbook(wb, f) 
    
    # finished
    IDP.showInfo(idp, paste("All peak tables saved successfully to ", f, ".", sep=""), timer=2, okButton=FALSE)
  }
}

