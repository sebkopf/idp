# FUNCTION: utility functions for converstion between dD and FD (in multi isotope system, pass all ratios! e.g. c(18O/16O, 17O/16O))
iso.RtoF<-function(ratio) ratio/(1+ratio) # ratio to fraction
iso.FtoR<-function(frac) frac/(1-frac) # fraction to ratio
iso.RtoD<-function(ratios, refs) ratios/refs - 1 #fraction to delta value (NOT 1000x!!)
iso.RtoDx<-function(ratios, refs) iso.RtoD(ratios, refs)*1000 #fraction to delta value (1000x)
iso.DtoR<-function(deltas, refs) (deltas+1) * refs #delta to fraction (delta NOT 1000x!!)
iso.DxtoR<-function(deltas, refs) iso.DtoR(deltas/1000, refs) #delta to fraction (deltas are 1000x)
iso.FtoD<-function(fracs, refs) iso.RtoD(iso.FtoR(fracs), refs) #fractions to delta values (deltas NOT 1000x)
iso.FtoDx<-function(fracs, refs) iso.FtoD(fracs, refs)*1000 #fractions to delta values  (deltas are 1000x)
iso.DtoF<-function(deltas, refs) iso.RtoF(iso.DtoR(deltas, refs)) #delta values to fractions (deltas NOT 1000x)
iso.DxtoF<-function(deltas, refs) iso.DtoF(deltas/1000, refs) #delta values to fractions (deltas are 1000x)
iso.DtoA<-function(deltas1, deltas2) (deltas1 + 1)/(deltas2 + 1) #delta notation to alpha fractionation factor (not 1000X)
iso.DxtoA<-function(deltas1, deltas2) iso.DtoA(deltas1/1000, deltas2/1000) #delta (in permil, i.e. 1000x) to alpha
iso.AtoEx<-function(alphas) (alphas-1)*1000 #alpha to epsilon (as permill, i.e. 1000x)
iso.DxtoEx<-function(deltas1, deltas2) iso.AtoEx(iso.DxtoA(deltas1, deltas2)) # from delta notation (in permil, i.e. 1000x) to epsilon in permil


####################################
# data frame convenience functions #
# these might not be actively used #
# anymore
####################################

# get indices of columns by name
df.getColIs <- function(df, cols) {
  indices <- sapply(cols, function(col) which(names(df) == col)[1])
  if (!is.empty(which(naIs <- is.na(indices))))
    warning(paste("Some column names were not found:", cols[naIs] ))
  return(indices[!naIs])
}

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
# type
# - integer
# - numeric
# - excelDate
# - excelTimestamp
df.changeDataType<-function(df, cols, type="numeric") {
  for (i in df.getColIs(df, cols)) {
    if (identical(type, "excelDate"))
      df[,i] <- as.Date(as.integer(df[,i]) - 25569, origin="1970-01-01") # convert from excel date
    else if (identical(type, "excelTimestamp"))
      df[,i] <- as.POSIXct((as.numeric(df[,i])-25569)*86400, tz="GMT", origin="1970-01-01")
    else  
      mode(df[,i])<-type
  }
  return (df)
}

# convert all numeric columns in a data frame to true numeric
df.convertNumerics<-function(df) {
  for (i in which(sapply(df, is.numeric))) df[[i]]<-as.numeric(df[[i]])
  return(df)
}

# change date/time to time diff (makes a column with the name of units)
# baseDT
#  - if omitted, take first entry as reference
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

# add experimental info to reshaped data set
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



