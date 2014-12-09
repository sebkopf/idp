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
