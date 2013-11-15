#!/usr/bin/Rscript

setwd("~/Documents/Studier & Förkovran/Högskolestudier/Stockholm School of Economics/MSc in Economics/Kurser/5350 Thesis in Economics/workdir/")

## Datainhämtning

GetZ1Cat <- function(zipname) {
  # Downloads zip-file, and read data from the textfile to a list of dataframes
  #
  # Args:
  #   zipname: The name of the zipfile on the FED webpage
  #
  # Returns:
  # A list of dataframes
  baseurl <- "http://www.federalreserve.gov/releases/z1/current/Disk/"

  url <- paste(baseurl, zipname, sep = "")

  zipfile <- tempfile()

  download.file(url, zipfile)

  # Vi tar ut en lista över filer för att sedan kunna extrahera dem
  filelist <- as.character(unzip(zipfile, list = TRUE)$Name)

  # Vi behöver behandla zipfiler med problem.
  filelist <- FixFileListZ1(filelist, zipname)

  # Vi sparar datan som tabeller i en lista som sedan kan hanteras.
  datalist <- lapply(filelist, function(i) {read.table(unz(zipfile, i), header=TRUE)})

  # När vi namnger listorna så uppstår inte "structur" problemet vid cbind senare.
  # Med en textoperation så tar vi dessutom bort onödig filändelse.
  names(datalist) <- gsub("*.prn$", "", filelist)
  return(datalist)
}

GrepQZ1 <- function(filelist) {
  grep("quarterly", filelist, value=TRUE)
}

FixFileListZ1 <- function(filelist,zipname) {
  if (zipname=="stabs.zip") {
    return(GrepQZ1(filelist))
  } else {
    return(filelist)
  }
}

TsZ1Cat <- function(datalist) {
  # Converts a list of dataframes to a list of time series objects
  #
  # Args:
  #   datalist: List of dataframes
  #
  # Returns:
  # A list of time series objects
  tslist <- lapply(datalist, function(i) {
    ts(i, start = c(as.numeric(substring(min(i$DATES),1,4)),
      as.numeric(substring(min(i$DATES),5,6))), frequency = 4)
  })
  #tslist <- lapply(datalist, function(i) ts(i, start = c(1952, 1), frequency = 4)) # Shold give identical value
  #identical(tslist,tslist2)
  #warnings()
  return(tslist)
}

CbindZ1Cat <- function(tslist) {
  # Merges(cbind) the time series objects in a list
  #
  # Args:
  #   tslist: List of time series objects
  #
  # Returns:
  # A merged time series object
  # tslist[unlist(lapply(tslist, length))>0]  # Skulle lösa problem, som löstes på annat sätt.
  tsdata <- do.call(cbind, tslist)
  return(tsdata)
}

Z1Cat <- function(zipname) {
  # Uses functions to return a category as a time series object
  #
  # Args:
  #   zipname: Name of a zipfile
  #
  # Returns:
  # A merged time series object
  CbindZ1Cat(TsZ1Cat(GetZ1Cat(zipname)))
}

GetZ1 <- function() {
  # Gets multiple TS-objets and returns them in a list.
  #
  # Args:
  #   zipnames: vector with zipnames
  #
  # Returns:
  # A list with time series objects

  # Vector with all zip-files avaliable at FED
  #zipnames <- c("atabs.zip", "utabs.zip", "ltabs.zip", "btabs.zip", "gtabs.zip", "stabs.zip", "itabs.zip")
  # Problematic files removed. Seems not to be important, and only anual data.
  #zipnames <- zipnames[!zipnames %in% c("stabs.zip", "itabs.zip")]

  # Vi skickar sedan namnvektorn in i get.z1.cat via lapply, och erhåller då
  # en lista med TS-objekt med de olika categorierna.
  z1list <- lapply(ZipNamesZ1(), function(i) Z1Cat(i))
  # Adding table names to the lists entries.
  names(z1list) <- gsub("*.zip$", "", ZipNamesZ1())
  return(z1list)
}

SaveZ1 <- function() {
  # Get a new Z1 list and saves it i the WD.
  saveRDS(GetZ1(),FileNameZ1())
}

ZipNamesZ1 <- function() {
  # Returns the defines name of local Z1-file

  # Vector with all zip-files avaliable at FED
  zipnames <- c("atabs.zip", "utabs.zip", "ltabs.zip", "btabs.zip", "gtabs.zip", "stabs.zip", "itabs.zip")
  # Problematic files removed. Seems not to be important, and only anual data.
  zipnames <- zipnames[!zipnames %in% c("itabs.zip")]

  # Vi skickar sedan namnvektorn in i get.z1.cat via lapply, och erhåller då
  # en lista med TS-objekt med de olika categorierna.
  return(zipnames)
}

FileNameZ1 <- function() {
  # Returns the defines name of local Z1-file
  return("z1")
}

ExistsZ1 <- function() {
  # Checks if local Z1-file exists.
  #
  # Returns:
  #   TRUE if it exists
  return(file.exists(FileNameZ1()))
}

MtimeZ1 <- function(){
  # Time when local Z1 file was last modified.
  #
  # Returns:
  #   mtime value for Z1-file.
  mtime <- file.info(FileNameZ1())$mtime
  return(mtime)
}

DifftimeZ1 <- function(){
  # Durations since local Z1 file was last modified.
  #
  # Returns:
  #   A timediffobject in hour units.
  #difftime(Sys.time(),get.time.z1()), units="hours")
  return(difftime(Sys.time(),MtimeZ1(), units="hours"))
}

OldZ1 <- function(){
  # Is the local Z1 file old.
  #
  # Returns:
  #   TRUE more than 24 hours since modification.
  return(24 < DifftimeZ1())
}

OKZ1 <- function(){
  # Is the local Z1 file OK.
  #
  # Returns:
  #   TRUE if it exists and is not old.
  return(ExistsZ1() & !OldZ1())
}

LoadZ1 <- function(){
  # If lokal Z1 isnt Ok, it gets a (new) copy.
  # Then it loads the returns Z1.
  #
  # Returns:
  #   Z1.
  if (!OKZ1()) {
    SaveZ1()
  }
  return(readRDS(FileNameZ1()))
}

Z1 <- function() {
  if (!exists(FileNameZ1())) {
    return(LoadZ1())
  } else {
  #return(11)
  return(get(FileNameZ1()))
  }
}

## Datahantering

SearchVectorZ1 <- function(searchvector) {
  # Converts a vector of search phrases to a string separated by OR-operators
  #
  # Args:
  #   searchvector: The vector to be converter
  #
  # Returns:
  #   A charachter string with OR-operators
  return(paste(searchvector, collapse="|"))
}

VarNamesZ1 <- function(searchvector, data, value = TRUE) {
  # Search columnnames that matches any pattern in the search vector
  #
  # Args:
  #   searchvector: Patters to be matched.
  #   data: The data which names that are to be searched.
  #   value: Should grep return value (default) or indices.
  #
  # Returns:
  #   A vector with matching indices or elements.
  return(grep(SearchVectorZ1(searchvector), colnames(data), value = value))
}

VarNameListZ1 <- function(searchvector, tslist=Z1(), value = TRUE) {
  # Search columnames in list elements, and returns a list of matches.
  #
  # Args:
  #   searchvector: Patters to be matched.
  #   tslist: A list with data which names that are to be searched.
  #   value: Should grep return value (default) or indices.
  #
  # Returns:
  #   A list containing vector with matching indices or elements.
  return(lapply(tslist, function(i) VarNamesZ1(searchvector,i, value)))
}

VarZ1 <- function(searchvector, data, value = TRUE) {
  # Access variables that match search pattern.
  #
  # Args:
  #   searchvector: Patters to be matched.
  #   data: The data which names that are to be searched.
  #   value: Should grep return value (default) or indices. For other functions.
  #
  # Returns:
  #   The columns that matched the search pattern.
  data <- data[, VarNamesZ1(searchvector, data, value)]
  if (length(data) > 0){
    return (data)
  #}
  #if (length(data) == 1){
  #  return (paste(VarNamesZ1(searchvector, data, TRUE)) <- data)
  } else {
  return(NULL)
  }
}

VarListZ1 <- function(searchvector, tslist=Z1(), value = TRUE) {
  # Returns a list of variables that matched the search term.
  #
  # Args:
  #   searchvector: Patters to be matched.
  #   tslist: A list with data which names that are to be searched.
  #   value: Should grep return value (default) or indices. For other functions.
  #
  # Returns:
  #   A list containing columns that matched the search pattern.
  return(lapply(tslist, function(i) VarZ1(searchvector,i, value)))
}

VarSelectionZ1 <- function(searchvector, tslist=Z1(), value = TRUE) {
  # Returns variables that matched the search term.
  #
  # Args:
  #   searchvector: Patters to be matched.
  #   tslist: A list with data which names that are to be searched.
  #   value: Should grep return value (default) or indices. For other functions.
  #
  # Returns:
  #   A MTS object with variables that matched the search pattern.
  selection <- CbindZ1Cat(VarListZ1(searchvector, tslist, value))
  colnames(selection) <- unlist(VarNameListZ1(searchvector))
  return(selection)
}

DuplicatedColsZ1 <- function(data) {
  # Finds duplicated columns.
  #
  # Args:
  #   data: a data object as a dataframe or mts.
  #
  # Returns:
  #   A vector where duplicated columns are marked TRUE.
  return(duplicated(data, MARGIN = 2))
}

RemoveDuplicatedColsZ1 <- function(data) {
  # Removes duplicated columns.
  #
  # Args:
  #   data: a data object as a dataframe or mts.
  #
  # Returns:
  #   A data object as a dataframe or mts.
  data[, !DuplicatedColsZ1(data)]
}
