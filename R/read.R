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
