###########
# WINDOWS #
###########

# standard window
# -- makes gwindow or gdialogbasic depending on input
win.init <- function (title = "", width = 100, height = 100, modal = FALSE, parent=NULL) {
  # get window going
  if (modal) {
    gw <- gbasicdialog(title=title, do.buttons=FALSE, parent=parent) 
    size(gw) <- c(width, height)
  } else
    gw <- gwindow(title, visible=FALSE, width=width, height=height)
  return (gw)
}


##########
# COMBOS #
##########

# load combo
# sort=FALSE/TRUE does alphabetical sorting
combo.load<-function(combo, entries, sort=FALSE, selection=NULL, blockHandlers=NULL) {
  combo.blockHandlers(combo, blockHandlers)
  if (sort) # sort the combo
    entries<-entries[order(entries)]
  combo[]<-entries
  if (!is.null(selection))
    combo.setSelection(combo, selection)
  combo.unblockHandlers(combo, blockHandlers)
}

# add to combo
combo.add<-function(combo, entry, sort=FALSE, select=TRUE, blockHandlers=NULL) {
  if (select)
    selection<-entry # select new entry
  else
    selection<-1 # select first entry
  combo.load(combo, c(combo[], entry), sort=sort, selection=selection, blockHandlers=blockHandlers)
}

# delete selected entry
combo.deleteSelection<-function(combo, reSelect=TRUE, blockHandlers=NULL) {
  index<-combo.getSelection(combo, index=TRUE)
  if (reSelect) 
    selection<-min(index,length(combo[])-1) # select the second to last object if the delted one was the last one
  else
    selection<-1 # select first entry otherwise
  combo.load(combo, combo[-index], selection=selection, blockHandlers=blockHandlers)
}

# selection
combo.getSelection<-function(combo, index=FALSE) return (svalue(combo, index=index))
combo.setSelection<-function(combo, value) {
  if (is.numeric(value))
    svalue(combo, index=TRUE)<-value
  else
    svalue(combo, index=FALSE)<-value
}

# block handlers
combo.blockHandlers<-function(combo, handlerIDs){
  if (!is.null(handlerIDs))
    for (handlerID in handlerIDs)
      blockHandler(combo, handlerID)
}

# unblock handlers
combo.unblockHandlers<-function(combo, handlerIDs){
  if (!is.null(handlerIDs))
    for (handlerID in handlerIDs)
      unblockHandler(combo, handlerID)
}

##########
# TABLES #
##########

####### ENTRIES (get, set, add, edit, delete) ########

# get value of a table entry at the provided index
# if no field is given, returns whole row, if field is given (e.g. c("adsfs", "sdfd")) then only field
table.getValue<-function(table, index, fields=NULL) {
  if (!is.null(fields))
    return(table[][index, fields])
  else
    return(table[][index, ])
}

# get the selected index of a table
table.getSelectedIndex<-function(table) {
  if (!is.empty(index<-svalue(table, index=TRUE)))
    return (index)
  else
    return (NULL)
}

# get selected value from a table (depends on what is defined as the selection column, usually the first column)
# or if providing field param, get specific one
table.getSelectedValue<-function(table, field=NULL) {
  if (is.null(field) && !is.empty(value<-svalue(table, index=FALSE)))
    return (value)
  else if (!is.null(field) && !is.null(dataset<-table.getSelection(table)))
    return (dataset[field])
  return (NULL)
}

# get selected record from a gtable
table.getSelection<-function(table) {
  if (!is.null(index<-table.getSelectedIndex(table))) { # check if anything is selected
    if (is.null(nrow(table[]))) 
      return(table[]) # if there's only one object in the table
    else
      return(table[][index, ]) # if there's multiple objects, return just selected
  }
  return (NULL)
}

# select something in the table
# blocks the changed handler if provided during the selection
table.setSelectedValue<-function(table, value, index=FALSE, blockHandlers=NULL) {
  table.blockHandlers(table, handlers=blockHandlers) # block handlers
  svalue(table, index=index)<-value # set value (use index to decide whether by value or by index)
  table.unblockHandlers(table, handlers=blockHandlers) # unblock handlers
}

# delete selected entry in a data table
# reselect - reselects the record at the same position
# if blockChangedHandlerID is provided, it will block this handler before doing the reselection business (and then unblock it)
table.deleteSelection<-function(table, reSelect=FALSE, blockHandlers=NULL) {
  if (!is.null(index<-table.getSelectedIndex(table))) { # check if anything is selected
    if (!is.null(total<-nrow(table[]))) { # does not support deleting the last one
      table.blockHandlers(table, handlers=blockHandlers) # block handlers
      table[]<-table[][-index,]  # remove line
      if (reSelect) {
        if (index==total) index<-max(1,total-1) # select the second to last object if the delted one was the last one
        table.setSelectedValue(table, index, index=TRUE) 
      }
      table.unblockHandlers(table, handlers=blockHandlers)
    } else
      print("WARNING: can't delete the last item in a table")
  }
}

# update table entry
table.update<-function(table, data, index, ignoreMissing = FALSE) {
  for (field in names(data)) {
    if (field%in%names(table[])) # update table field
      table[][index,field]<-data[[field]]
    else if (!ignoreMissing)
      cat(paste("\nWARNING (to disable, pass ignoreMissing = TRUE):\n\tTrying to update table field", 
                field, #"with value", data[[field]], 
                "but field does not exist in table (", paste(names(table[]), collapse=", "), ")"))  
  }
}

# update currently selected table entry
table.updateSelection<-function(table, data, ignoreMissing = FALSE) {
  if (!is.null(index<-table.getSelectedIndex(table))) # check if anything is selected
    table.update(table, data, index, ignoreMissing = ignoreMissing)
}

# add an entry in the data table
# index --> add at specific index, if NULL, adds at the end of the table (or if index > length of table)
# returns index of newly added entry
table.add<-function(table, data, index=NULL, select=TRUE, blockHandlers=NULL, ignoreMissing = FALSE) {
  table.blockHandlers(table, handlers=blockHandlers) # block handlers
  if (!is.null(index) && index<=nrow(table[])) 
    table[][(index+1):(nrow(table[])+1),]<-table[][index:nrow(table[]),] # move records up one
  else
    index<-nrow(table[])+1 # adding at the end of the table
  table.update(table, data, index, ignoreMissing = ignoreMissing)
  
  if (select) 
    table.setSelectedValue(table, index, index=TRUE) 
  table.unblockHandlers(table, handlers=blockHandlers) # unblock handlers
  return (index)
}

# add an entry in the data table after the current selection
# if nothing is selected, adds a new entry at the end of the table
# returns index of newly added entry
table.addAfterSelection<-function(table, data, select=TRUE, blockHandlers=NULL, ignoreMissing = FALSE) {
  if ( !is.null(index<-table.getSelectedIndex(table)))
    index<-index + 1
  return (table.add(table, data, index=index, select=select, blockHandlers=blockHandlers, ignoreMissing = ignoreMissing))
}

####### HANDLERS ##########

# table handler blocks (This is only implemented this way because handler blocking does not seem to work for gtables!)
# important, need to implement this in the handlers though!!
table.blockHandlers<-function(obj, handlers=c("changed", "clicked")) for (handler in handlers) tag(obj, paste(handler,"Handler", sep=""))<-TRUE
table.unblockHandlers<-function(obj, handlers=c("changed", "clicked")) for (handler in handlers) tag(obj, paste(handler,"Handler", sep=""))<-FALSE
table.isHandlerBlocked<-function(obj, handler=NULL) {
  if (is.null(handler) || is.null(tag(obj, paste(handler,"Handler", sep=""))))
    return (FALSE)
  return (tag(obj, paste(handler,"Handler", sep="")))
}

###### TABLE SORTING ########

# datasets move up and down in tables
# move = c("up", "down", "top", "bottom"), pick one
# gvar = the name of a global variable to update
# tableID = if a global varaible is provided, use this field in the table to find the right entries in the global variable to update
# gvarID = if a global variable is provided, use this field to find the right entries to update
# gvarOrder = if a global variable is provided, use this field to keep track of the locations
table.moveHandler<-function(table, move=NULL, gvar=NULL, tableID="ID", gvarID="ID", gvarOrder="Order", blockHandlers=NULL) {
  if (!is.null(move)) { # only move if a move command is provided
    if (!is.empty(index<-svalue(table, index=TRUE))) { # make sure something is selected
      # block event handlers
      table.blockHandlers(table, handlers=blockHandlers) 
      
      # get new index
      newIndex<-switch(move, "up"=(index-1), "down"=(index+1), "top"=1, "bottom"=nrow(table[])) # find the record to exchange with
      if ( index!=newIndex && newIndex >= 1 && newIndex <= nrow(table[])) { # can move (no point moving outside the table or staying at the same place)
        
        # update global variable if provided
        if (!is.null(gvar)) {
          globalvar<-get(gvar, envir=.GlobalEnv) # get global variable
          
          ID<-table[index, tableID] # figure out which record in the global variable to update
          sort<-globalvar[[gvarOrder]][which(globalvar[[gvarID]]==ID)] # current sort 
          newID<-table[][newIndex, tableID] # figure out ID of new position
          newSort<-globalvar[[gvarOrder]][which(globalvar[[gvarID]]==newID)] # sort of new position
          
          if (move=="top")
            globalvar<-var.updateSorting(globalvar, newSort, sort, orderField=gvarOrder, changeBy=1) # update sorts
          else if (move=="bottom")
            globalvar<-var.updateSorting(globalvar, sort, newSort, changeBy=-1) # update sorts
          else
            globalvar[[gvarOrder]][which(globalvar[[gvarID]]==newID)]<-sort # update exchange item with current sort
          globalvar[[gvarOrder]][which(globalvar[[gvarID]]==ID)]<-newSort
          
          assign(gvar, globalvar, envir=.GlobalEnv) # assign global variable
        }
        
        ### UPDATE TABLE ###
        selectedRow<-table[][index,]
        if (move=="top")
          table[][2:index,]<-table[][1:(index-1),] # move everything down 1 that's above the item
        else if (move=="bottom")
          table[][index:(nrow(table[])-1),]<-table[][(index+1):nrow(table[]),] # move everything up 1 that's below the item
        else # up and down moves
          table[][index,]<-table[newIndex,] # exchange it with the exchange index
        
        # update new index position and select it
        table[][newIndex,]<-selectedRow
        svalue(table, index=TRUE)<-newIndex 
      }
      # unblock event handlers
      table.unblockHandlers(table, handlers=blockHandlers)
    }
  }
}

# provide the buttons for moving data in a table
table.moveButtons<-function(container, table, gvar=NULL, tableID="ID", gvarID="ID", gvarOrder="Order", blockHandlers=c("changed", "clicked") ) {
  moveButtonsGrp<-ggroup(horizontal=FALSE, cont=container)
  addSpring(moveButtonsGrp)
  gbutton(action=gaction("T", icon="gtk-goto-top", handler=function(h,...) table.moveHandler(table, move="top", gvar=gvar, tableID=tableID, gvarID=gvarID, gvarOrder=gvarOrder, blockHandlers=blockHandlers)), cont=moveButtonsGrp) 
  gbutton(action=gaction("U", icon="gtk-go-up", handler=function(h,...) table.moveHandler(table, move="up", gvar=gvar, tableID=tableID, gvarID=gvarID, gvarOrder=gvarOrder, blockHandlers=blockHandlers)), cont=moveButtonsGrp) 
  gbutton(action=gaction("D", icon="gtk-go-down", handler=function(h,...) table.moveHandler(table, move="down", gvar=gvar, tableID=tableID, gvarID=gvarID, gvarOrder=gvarOrder, blockHandlers=blockHandlers)),  cont=moveButtonsGrp)
  gbutton(action=gaction("B", icon="gtk-goto-bottom", handler=function(h,...) table.moveHandler(table, move="bottom", gvar=gvar, tableID=tableID, gvarID=gvarID, gvarOrder=gvarOrder, blockHandlers=blockHandlers)),  cont=moveButtonsGrp)
  addSpring(moveButtonsGrp)
}

####### special tables ########
# table that has toggle abalitiy
# returns a rGtkDataFrame whose data frame content can be accessed by []
table.toggleTable <- function(cont, df, toggleColumn, toggleFunc = NULL, invisibleColumns = NULL) {
  index <- which(names(df) == toggleColumn) # index of toggle column
  model <- rGtkDataFrame(df) # data frame model
  view <- gtkTreeView() # display structure
  
  # visible columns
  if (!is.null(invisibleColumns))
    excludeIndices <- c(index, which(names(df) %in% invisibleColumns))
  else 
    excludeIndices <- index
  
  # cell renderers
  cr <- gtkCellRendererToggle() # cell renderer for the check box
  cr['activatable'] <- TRUE
  
  # add cell renderers to table
  view$insertColumnWithAttributes (0, toggleColumn, cr, active = (index-1) ) 
  mapply(view$insertColumnWithAttributes, -1, colnames(df[-excludeIndices]), list(gtkCellRendererText()), text = (seq_along(df)[-excludeIndices]) -1)
  
  # signal for processing cell renderer
  gSignalConnect (cr, "toggled", function (cr, path, user.data ) {
    row <- (as.numeric(path) + 1)
    model <- user.data$getModel()
    #if (is.null(toggleFunc) || do.call(toggleFunc, list(data = model, row = row))) #FIXME --> write toggle func
    model[row, index] <- !model[row, index]
  }, data=view)
  
  # add model
  view$setModel(model)
  
  # add to a scroll window in container
  scrolled_window <- gtkScrolledWindow()
  getToolkitWidget(cont)$packStart(scrolled_window , expand = TRUE, fill = TRUE)
  scrolled_window$add(view)
  scrolled_window$setPolicy ("automatic", "automatic")
  
  # return model table --> can access data by model[]
  return(model)
}


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

