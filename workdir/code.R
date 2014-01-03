#!/usr/bin/Rscript

setwd("~/Documents/Studier & Förkovran/Högskolestudier/Stockholm School of Economics/MSc in Economics/Kurser/5350 Thesis in Economics/workdir/")


require(xtable)


# Table and ADF-test functions

pstars <- function (p) {
#  Function that from a p-value returns a corresponding star-charachter.
  ifelse(p < .001, "***",
    ifelse(p < .01, "**",
      ifelse(p < .05, "*",
        ifelse(p < .1, "'", ""
        )
      )
    )
  )
}


RankTable <- function(cajoobj = NULL, caption = "Rank", label = "", varlabels = NULL){

  RankTableHead <- function() {
    lambda = "\\lambda   "
    if (!is.null(cajoobj)) {
      if (cajoobj@type == "trace statistic") {lambda = "\\lambda_{trace}"}
      if (cajoobj@type == "maximal eigenvalue statistic (lambda max)") {lambda = "\\lambda_{max}"}
    }

    cat("
      \\begin{table}
        \\centering
          \\caption{", caption,"}
          \\label{", label,"}
        \\begin{tabular}{@{}crlll@{}}
          \\toprule
            \\multicolumn{2}{c}{} &
            \\multicolumn{3}{c}{Critical Values}
          \\\\
            \\cmidrule(l){3-5}
          %\\\\
            \\multicolumn{1}{l}{$\\mathcal{H}_0$} &
            \\multicolumn{1}{l}{$",lambda,"$} &
            \\multicolumn{1}{l}{$10\\%$} &
            \\multicolumn{1}{l}{$5\\%$} &
            \\multicolumn{1}{l}{$1\\%$}
          \\\\
            \\midrule
    ")
  }

  RankTableFoot <- function() {
    cat("
          \\bottomrule
        \\end{tabular}
      \\end{table}
    ")
  }

  RankTableRow <- function(rankrow) {
    if (rankrow[1] == 0) rankrel <- paste0("$r = ", rankrow[1],"$")
    if (rankrow[1] != 0 & cajoobj@type == "trace statistic") rankrel <- paste0("$r \\le ", rankrow[1],"$")
    if (rankrow[1] != 0 & cajoobj@type == "maximal eigenvalue statistic (lambda max)") rankrel <- paste0("$r = ", rankrow[1],"$")
    #if (rankrow[1] == 0) {rankrel <- paste0("r = ", rankrow[1])}
    #if (rankrow[1] != 0 & cajoobj@type == "trace statistic") {rankrel <- paste0("r \\le ", rankrow[1])}
    #if (rankrow[1] != 0 & cajoobj@type == ) {rankrel <- paste0("r = ", rankrow[1])}

    cat(rankrel,"&",rankrow[2],"&",rankrow[3],"&",rankrow[4],"&",rankrow[5],"\\\\
      ")
  }

  RankTableRows <- function(ranktable) {
    apply(ranktable, 1, function(i) {RankTableRow(i)})
  }

  RankTableHead()

  if (!is.null(cajoobj)) {
    ranktable <- cbind((nrow(cajoobj@cval)-1):0,cajoobj@teststat, cajoobj@cval)  # cbind(rownames(cajoobj@cval), cajoobj@teststat, cajoobj@cval)
    ranktable <- ranktable[nrow(ranktable):1, ]
    RankTableRows(ranktable)
  }

  RankTableFoot()

}

#RankTable()
#RankTable(vecm1)

ADFTable <- function(adfobjls, caption = "ADF-test results", label = "", varlabels = NULL){
# Returns a latex-table from a list of ur.df-class objects.

  ADFTableHead <- function() {
    # Returns the head of the latex table.
    cat("
      \\begin{table}
        %\\small
        \\centering
        %\\begin{threeparttable}
          \\caption{", caption,"}
          \\label{", label,"}
          \\centering
          %\\newcolumntype{d}[1]{D{.}{.}{-1} }
          %\\tabcolsep=0.11cm
          \\begin{tabular}{l c c c c c c c}

            \\toprule
            \\multicolumn{1}{c}{Variable}&
            \\multicolumn{1}{c}{Model}&
            \\multicolumn{1}{c}{Lags}&
            \\multicolumn{1}{c}{Test value}&
            sgn &
            \\multicolumn{3}{c}{Critical values}

            \\\\
            \\cline{6-8}
            & & & & & \\multicolumn{1}{c}{1\\%} & \\multicolumn{1}{c}{5\\%} & \\multicolumn{1}{c}{10\\%} \\\\
            \\midrule
            ")
  }

  ADFTableFoot <- function() {
    # Returns the foot of the latex table.
            cat("
            \\bottomrule
            \\multicolumn{8}{ p{\\textwidth} }{\\footnotesize Variables: as holding and issuing part. Model: Trend includes a intercept ($\\alpha$) and a drift term ($\\beta$); Drift includes the intercept term; and None includes neither. Lags: Lag length chosen using BIC. sgn: Symbols for significance at the 10\\% ('), 5\\% (*), and 1\\% (**) level.}\\\\
          \\end{tabular}
          %\\renewcommand{\\TPTnoteSettings}{\\footnotesize}
          %\\begin{tablenotes}
            %notes
          %\\end{tablenotes}
        %\\end{threeparttable}
      \\end{table}
    ")
  }

  ADFTableRow <- function(adfobj,varname) {
    # Returns a row in the latex from a ur.df-class object.

    simpleCap <- function(x) {
      # Capitalize the first letter.
      s <- strsplit(x, " ")[[1]]
      paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
    }

    ADFtauStat <- function(adfobj) {
      # Extracts the tau statistic from the ur.df-class object.
      adfobj@teststat[,grep("tau",colnames(adfobj@teststat))]
    }

    ADFtauCval <- function(adfobj) {
      # Extracts the critical values for tau from a ur.df-class object.
      adfobj@cval[grep("tau",rownames(adfobj@cval)), ]
    }

    ADFStars <- function (adfobj) {
      # Compares the test statistic to the critical values and returns a star.
      c("","'","*","**")[length(which(ADFtauStat(adfobj) < ADFtauCval(adfobj)))+1]
    }

    cat(
      cat(
        varname,
        simpleCap(adfobj@model),
        adfobj@lags,
        round(ADFtauStat(adfobj), digits = 3),
        ADFStars(adfobj),
        paste(
          ADFtauCval(adfobj),
          sep = "&"
        )
        ,       sep = "&"
      )
      , "\\\\\n"
    )
  }

  ADFTableRows <- function(adfobjls) {
    # Returns latex rows from a list of ur.df-class objects.
    varnames <- names(adfobjls)
    lapply(1:length(adfobjls), function(i) {ADFTableRow(adfobjls[[i]], varnames[i])})
    #lapply(varnamesorder, function(i) {ADFTableRow(adfobjls[[i]], i)})
  }

  ADFTableHead()

  ADFTableRows(adfobjls)

  ADFTableFoot()
}

ADFObjLs <- function(data, types = NULL, lags = NULL, selectlags = NULL, varnames = NULL) {
# Returns a list of ur.df-class objects from a mts-object.
  require(urca)

  if(ncol(data)!=length(types)) {
    if(length(types)==1) {types=rep(types,ncol(data))}
    else {types = rep("none",ncol(data))}
    warning("ncol(data)!=length(types)")
  }

  if(ncol(data)!=length(lags)) {
    if(length(lags)==1) {lags=rep(lags,ncol(data))}
    else {lags = rep(1, ncol(data))}
    warning("ncol(data)!=length(lags)")
  }

  if(ncol(data)!=length(selectlags)) {
    if(length(selectlags)==1) {selectlags=rep(selectlags,ncol(data))}
    else {selectlags = rep("Fixed", ncol(data))}
    warning("ncol(data)!=length(selectlags)")
  }

  if(ncol(data)!=length(varnames)) {varnames = colnames(data)}

  adfobjlist <- lapply(seq(ncol(data)), function(i) {
    ur.df(y = data[, i] , type = types[i], lags = lags[i], selectlags = selectlags[i])
  })

  names(adfobjlist) <- varnames

  return(adfobjlist)
}

ADFObjLsSum <- function(adfobjls) {
# Returns summary output from a list of ur.df-class objects.
  lapply(adfobjls, summary)
}



ZipNamesZ1 <- function() {
  # Returns the defined name of local Z1-file

  # Vector with all zip-files avaliable at FED
  zipnames <- c("atabs.zip", "utabs.zip", "ltabs.zip", "btabs.zip", "gtabs.zip", "stabs.zip", "itabs.zip")
  # Problematic files removed. Seems not to be important, and only anual data.
  zipnames <- zipnames[!zipnames %in% c("itabs.zip")]

  # Vi skickar sedan namnvektorn in i get.z1.cat via lapply, och erhåller då
  # en lista med TS-objekt med de olika categorierna.
  return(zipnames)
}

FileNameZ1 <- function() {
  # Returns the defined name of local Z1-file
  return("z1")
}

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

Z1 <- function() {
  #

  LoadZ1 <- function(){
    # If lokal Z1 isnt Ok, it gets a (new) copy.
    # Then it loads the returns Z1.
    #
    # Returns:
    #   Z1.

    OKZ1 <- function(){
      # Is the local Z1 file OK.
      #
      # Returns:
      #   TRUE if it exists and is either not old or not locked.

      ExistsZ1 <- function() {
        # Checks if local Z1-file exists.
        #
        # Returns:
        #   TRUE if it exists
        return(file.exists(FileNameZ1()))
      }

      LockedZ1 <- function(){
        # Is the local Z1 file locked. If set to locked (TRUE), the local Z1-file
        # won't update even if it's old. Lock when starting estimation is finished.
        #
        # Returns:
        #   The desired value.
        return(TRUE)
      }

      OldZ1 <- function(){
        # Is the local Z1 file old.
        #
        # Returns:
        #   TRUE more than 24 hours since modification.

        DifftimeZ1 <- function(){
          # Durations since local Z1 file was last modified.
          #
          # Returns:
          #   A timediffobject in hour units.
          #difftime(Sys.time(),get.time.z1()), units="hours")

          MtimeZ1 <- function(){
            # Time when local Z1 file was last modified.
            #
            # Returns:
            #   mtime value for Z1-file.
            return(file.info(FileNameZ1())$mtime)
          }

          return(difftime(Sys.time(),MtimeZ1(), units="hours"))
        }

        return(24 < DifftimeZ1())
      }

      return(ExistsZ1() & (!OldZ1() | LockedZ1()))
    }

    SaveZ1 <- function() {
     # Get a new Z1 list and saves it i the WD.
     saveRDS(GetZ1(),FileNameZ1())
    }

    if (!OKZ1()) {
      SaveZ1()
    }

    return(readRDS(FileNameZ1()))
  }

  if (!exists(FileNameZ1())) {
    return(LoadZ1())
  } else {
  #return(11)
  return(get(FileNameZ1()))
  }

}

## Datauthämtning

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
  data <- data[, VarNamesZ1(searchvector, data, value), drop=FALSE]
  if (length(data) > 0){
    #colnames(data) <- VarNamesZ1(searchvector, data, value) # FUNGERAR?
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
  #colnames <- as.vector(unlist(VarNameListZ1(searchvector, tslist, value)))
  #colnames(selection) <- (colnames)
  #colnames(selection) <- unlist(VarNameListZ1(searchvector))
  return(selection)
}

## Datastädning

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
  data[, !DuplicatedColsZ1(RoundOffZ1(data))]  # RoundOffZ1 för felaktiga serier.
}

UniqueVar <- function(searchvector) {
  RemoveDuplicatedColsZ1(VarSelectionZ1(searchvector))
}

RoundOffZ1 <- function(x) {
  round(x+0.01, digits=1)
}

SubColNamesZ1 <- function(colnames) {
  return(gsub("^[^.]*[.]", "", colnames))
}

NameExtract <- function(colnames) {
  gsub("^.*(F[^.]*[.].*$)", "\\1", colnames)
}

RenameColnamesZ1 <- function(data) {
  colnames(data) <- SubColNamesZ1(colnames(data))
  return(data)
}

DescTrunc <- function(dataframe) {
  dataframe[["Description"]] <- gsub("(^[^;]*); (.*$)", "\\1",dataframe[["Description"]])
  return(dataframe)
}

# Asset series table

FL894090005.Q <- as.data.frame(
  matrix(
    nrow=19,
    ncol=3,
    dimnames=list(1:19,c("FOF-code", "Description", "Acr.")),
    data=c(
      "FL104090005.Q",
      "FL114090005.Q",
      "FL154090005.Q",
      "FL214090005.Q",
      "FL413065005.Q",
      "FL264090005.Q",
      "FL314090005.Q",
      "FL404090005.Q",
      "FL474090005.Q",
      "FL514090005.Q",
      "FL544090005.Q",
      "FL574090005.Q",
      "FL684090005.Q",
      "FL714090005.Q",
      "FL224090005.Q",
      "FL764090005.Q",
      "FL744090005.Q",
      "FL754090005.Q",
      "FL344090005.Q",
      "Nonfinancial corporate business; total financial assets",
      "Nonfinancial noncorporate business; total financial assets",
      "Households and nonprofit organizations; total financial assets",
      "State and local governments, excluding employee retirement funds; total financial assets",
      "Agency-and GSE-backed mortgage pools; total mortgages; asset",
      "Rest of the world; total financial assets",
      "Federal government; total financial assets",
      "Government-sponsored enterprises; total financial assets",
      "Credit unions; total financial assets",
      "Property-casualty insurance companies; total financial assets",
      "Life insurance companies; total financial assets",
      "Private pension funds; total financial assets",
      "Private financial institutions not elsewhere classified; total financial assets",
      "Monetary authority; total financial assets",
      "State and local government employee retirement funds; total financial assets",
      "U.S.-chartered depository institutions, including IBFs; total financial assets",
      "Banks in U.S.-affiliated areas; total financial assets",
      "Foreign banking offices in the U.S., including IBFs; total financial assets",
      "Federal government retirement funds; total financial assets",
      "ANF",
      "ANF",
      "AHH",
      "AGG",
      "AGG",
      "ARW",
      "AGG",
      "AGG",
      "AFI",
      "AFI",
      "AFI",
      "AFI",
      "AFI",
      "ACB",
      "AGG",
      "AFI",
      "AFI",
      "AFI",
      "AGG"
    )
  )
)

allassetstable <- xtable(
  x = DescTrunc(FL894090005.Q),
  caption = "Series that the total assets for all sectors is derived from.",
  label = "tab:allassets"
)

# Liabilities series table

FL894190005.Q <- as.data.frame(
  matrix(
    nrow=14,
    ncol=3,
    dimnames=list(1:14,c("FOF-code", "Description", "Acr.")),
    data=c(
      "FL104190005.Q",
      "FL114190005.Q",
      "FL154190005.Q",
      "FL413065005.Q",
      "FL264190005.Q",
      "FL314190005.Q",
      "FL404190005.Q",
      "FL214190005.Q",
      "FL704190005.Q",
      "FL514190005.Q",
      "FL544190005.Q",
      "FL684190005.Q",
      "FL714190005.Q",
      "FL594190005.Q",
      "Nonfinancial corporate business; total liabilities",
      "Nonfinancial noncorporate business; total liabilities",
      "Households and nonprofit organizations; total liabilities",
      "Agency-and GSE-backed mortgage pools; total mortgages; asset",
      "Rest of the world; total liabilities",
      "Federal government; total liabilities",
      "Government-sponsored enterprises; total liabilities",
      "State and local governments, excluding employee retirement funds; total liabilities",
      "Private depository institutions; total liabilities",
      "Property-casualty insurance companies; total liabilities",
      "Life insurance companies; total liabilities",
      "Private financial institutions not elsewhere classified; total liabilities",
      "Monetary authority; total liabilities",
      "Pension funds; total liabilities",
      "LNF",
      "LNF",
      "LHH",
      "LGG",
      "LRW",
      "LGG",
      "LGG",
      "LGG",
      "LFI",
      "LFI",
      "LFI",
      "LFI",
      "LCB",
      "LFI"
    )
  )
)

allliabtable <- xtable(
  x = DescTrunc(FL894190005.Q),
  caption = "Series that the total liabilities for all sectors is derived from.",
  label = "tab:allliab"
)

# Liabilities + Equity series table

FL894194005.Q <- as.data.frame(
  matrix(
    nrow=3,
    ncol=3,
    dimnames=list(1:3,c("FOF-code", "Description", "Acr.")),
    data=c(
      "FL894190005.Q",
      "FL893064105.Q",
      "FL152090205.Q",
      "All sectors; total liabilities",
      "All sectors; corporate equities; asset",
      "Households and nonprofit organizations; proprietors' equity in noncorporate business",
      "-",
      "-",
      "-"
    )
  )
)

allliabeqtable <- xtable(
  x = FL894194005.Q,
  caption = "Series that the total liabilities and equity for all sectors is derived from.",
  label = "tab:allliabeq"
)

nonfinassetsdf <- as.data.frame(
  matrix(
    nrow=3,
    ncol=3,
    dimnames=list(1:3,c("FOF-code", "Description", "Acr.")),
    data=c(
      "FL102010005.Q",
      "FL112010005.Q",
      "FL152010005.Q",
      "Nonfinancial corporate business; nonfinancial assets",
      "Nonfinancial noncorporate business; nonfinancial assets",
      "Households and nonprofit organizations; nonfinancial assets",
      "NNF",
      "NNF",
      "NHH"
    )
  )
)

nonfinassetsxtable <- xtable(
  x = DescTrunc(nonfinassetsdf),
  caption = "Nonfinancial assets series",
  label = "tab:nonfinassetsdf"
)

incomedf <- as.data.frame(
  matrix(
    nrow=3,
    ncol=3,
    dimnames=list(1:3,c("FOF-code", "Description", "Acr.")),
    data=c(
      "FA086902005.Q",
      "FA086902105.Q",
      "FA086010005.Q",
      "Gross Domestic Product",
      "Gross National Product",
      "National Income",
      "GDP",
      "GNP",
      "GNI"
    )
  )
)

incomextable <- xtable(
  x = incomedf,
  caption = "Income series",
  label = "tab:income"
)

# Forloops to find what series that are empty. Has to be excluded.

## Make the assets dataset

assetsidseries <- FL894090005.Q[, 1]

#for (i in 1:length(assetsidseries)) {
#  print(i)
#  print(dim(VarSelectionZ1(assetsidseries[i])))
#}


drtyasset <- cbind(
VarSelectionZ1(assetsidseries[1])[, 1]
,#247   2
VarSelectionZ1(assetsidseries[2])[, 1]
,#247   2
VarSelectionZ1(assetsidseries[3])[, 1]
,#247   3
VarSelectionZ1(assetsidseries[4])
,#247   1
VarSelectionZ1(assetsidseries[5])[, 1]
,#247   8
VarSelectionZ1(assetsidseries[6])
,#247   1
VarSelectionZ1(assetsidseries[7])
,#247   1
VarSelectionZ1(assetsidseries[8])
,#247   1
VarSelectionZ1(assetsidseries[9])
,#247   1
VarSelectionZ1(assetsidseries[10])
,#247   1
VarSelectionZ1(assetsidseries[11])
,#247   1
VarSelectionZ1(assetsidseries[12])
,#247   1
#VarSelectionZ1(assetsidseries[13])
#,#NULL
VarSelectionZ1(assetsidseries[14])
,#247   1
VarSelectionZ1(assetsidseries[15])
,#247   1
VarSelectionZ1(assetsidseries[16])
,#247   1
VarSelectionZ1(assetsidseries[17])
,#247   1
VarSelectionZ1(assetsidseries[18])
,#247   1
VarSelectionZ1(assetsidseries[19])[, 1]
#247   3
)

assetsidseries <- assetsidseries["FL684090005.Q"!=  assetsidseries]

colnames(drtyasset) <- assetsidseries


# Dirty sector Liabilities
asectors <- sort(unique(FL894090005.Q[,3]))

dirtysectorassets <- cbind(
drtyasset[, colnames(drtyasset) %in% FL894090005.Q[(FL894090005.Q[,3] == asectors[1]), 1]]
,
rowSums(drtyasset[, colnames(drtyasset) %in% FL894090005.Q[(FL894090005.Q[,3] == asectors[2]), 1]])
,
rowSums(drtyasset[, colnames(drtyasset) %in% FL894090005.Q[(FL894090005.Q[,3] == asectors[3]), 1]])
,
drtyasset[, colnames(drtyasset) %in% FL894090005.Q[(FL894090005.Q[,3] == asectors[4]), 1]]
,
rowSums(drtyasset[, colnames(drtyasset) %in% FL894090005.Q[(FL894090005.Q[,3] == asectors[5]), 1]])
,
drtyasset[, colnames(drtyasset) %in% FL894090005.Q[(FL894090005.Q[,3] == asectors[6]), 1]]
)

colnames(dirtysectorassets) <- asectors

dirtysectorassets <- log(dirtysectorassets)

xtabledirtyassets <- xtable(summary(dirtysectorassets))


# Using the the output from the  functions above, we construct a datasets with some dirty code.

# VarSelectionZ1(liabilityidseries[i])

## Make the liabilitues dataset

# Series in the all Liabilites. 12 is empty.

liabilityidseries <- FL894190005.Q[, 1]
#for (i in 1:length(liabilityidseries)) {
#  print(i)
#  print(dim(VarSelectionZ1(liabilityidseries[i])))
#}

drtyliab <- cbind(
VarSelectionZ1(liabilityidseries[1])[, 1]
,#47   2
VarSelectionZ1(liabilityidseries[2])[, 1]
,#47   2
VarSelectionZ1(liabilityidseries[3])[, 1]
,#47   3
VarSelectionZ1(liabilityidseries[4])[, 1]
,#47   8
VarSelectionZ1(liabilityidseries[5])
,#47   1
VarSelectionZ1(liabilityidseries[6])
,#47   1
VarSelectionZ1(liabilityidseries[7])
,#47   1
VarSelectionZ1(liabilityidseries[8])
,#47   1
VarSelectionZ1(liabilityidseries[9])
,#47   1
VarSelectionZ1(liabilityidseries[10])
,#47   1
VarSelectionZ1(liabilityidseries[11])
,#47   1
#VarSelectionZ1(liabilityidseries[12])
#NULL
VarSelectionZ1(liabilityidseries[13])
,#47   1
VarSelectionZ1(liabilityidseries[14])
#47   1
)

liabilityidseries <- liabilityidseries["FL684190005.Q"!=  liabilityidseries]

colnames(drtyliab) <- liabilityidseries


# Dirty sector Liabilities
lsectors <- sort(unique(FL894190005.Q[,3]))

dirtysectorliab <- cbind(
drtyliab[, colnames(drtyliab) %in% FL894190005.Q[(FL894190005.Q[,3] == lsectors[1]), 1]]
,
rowSums(drtyliab[, colnames(drtyliab) %in% FL894190005.Q[(FL894190005.Q[,3] == lsectors[2]), 1]])
,
rowSums(drtyliab[, colnames(drtyliab) %in% FL894190005.Q[(FL894190005.Q[,3] == lsectors[3]), 1]])
,
drtyliab[, colnames(drtyliab) %in% FL894190005.Q[(FL894190005.Q[,3] == lsectors[4]), 1]]
,
rowSums(drtyliab[, colnames(drtyliab) %in% FL894190005.Q[(FL894190005.Q[,3] == lsectors[5]), 1]])
,
drtyliab[, colnames(drtyliab) %in% FL894190005.Q[(FL894190005.Q[,3] == lsectors[6]), 1]]
)

colnames(dirtysectorliab) <- lsectors

dirtysectorliab <- log(dirtysectorliab)


liabilityequityidseries <- c(liabilityidseries,FL894194005.Q[2:3, 1])
#for (i in 1:length(liabilityequityidseries)) {
#  print(i)
#  print(dim(VarSelectionZ1(liabilityequityidseries[i])))
#}


# nonfinancial



nonfinancialassets <- cbind(
  VarSelectionZ1(nonfinassetsdf[1,1])
  ,
  VarSelectionZ1(nonfinassetsdf[2,1])
  ,
  VarSelectionZ1(nonfinassetsdf[3,1])[, 1]
)

colnames(nonfinancialassets) <- nonfinassetsdf[ ,1]

nsectors <- sort(unique(nonfinassetsdf[,3]))


dirtysectornonfin <- cbind(
  nonfinancialassets[, colnames(nonfinancialassets) %in% nonfinassetsdf[(nonfinassetsdf[,3] == nsectors[1]), 1]]
  ,
  rowSums(nonfinancialassets[, colnames(nonfinancialassets) %in% nonfinassetsdf[(nonfinassetsdf[,3] == nsectors[2]), 1]])
)

colnames(dirtysectornonfin) <- nsectors

dirtysectornonfin <- log(dirtysectornonfin)



# income set

income <- cbind(
  VarSelectionZ1(incomedf[1,1])[, 1]
  ,
  VarSelectionZ1(incomedf[2,1])
  ,
  VarSelectionZ1(incomedf[3,1])[, 1]
)

colnames(income) <- incomedf[ ,3]

income <- log(income)



#assetadfd <- ADFObjLs(dirtysectorassets, types="drift", lags = 20, selectlags = "BIC")
assetadft <- ADFObjLs(dirtysectorassets, types="trend", lags = 20, selectlags = "BIC")

#liabadfd <- ADFObjLs(dirtysectorliab, types="drift", lags = 20, selectlags = "BIC")
liabadft <- ADFObjLs(dirtysectorliab, types="trend", lags = 20, selectlags = "BIC")

#nofinassadfd <- ADFObjLs(dirtysectornonfin, types="drift", lags = 20, selectlags = "BIC")
nofinassadft <- ADFObjLs(dirtysectornonfin, types="trend", lags = 20, selectlags = "BIC")

#incadfd <- ADFObjLs(income, types="drift", lags = 20, selectlags = "BIC")
incadft <- ADFObjLs(income, types="trend", lags = 20, selectlags = "BIC")

assetvarset <- colnames(dirtysectorassets)
liabvarset <- colnames(dirtysectorliab)
nonfinvarset <- colnames(dirtysectornonfin)
incvarset <- colnames(income)
allvarset <- c(assetvarset,liabvarset,nonfinvarset,incvarset)

dataset <- cbind(dirtysectorassets,dirtysectorliab,dirtysectornonfin,income)
colnames(dataset) <- c(colnames(dirtysectorassets),colnames(dirtysectorliab),colnames(dirtysectornonfin),colnames(income))


#adfobjls2_2 <- ADFObjLs(GDP, types="drift", lags = 10, selectlags = "BIC")
#adfobjls3_2 <- ADFObjLs(GDP, types="trend", lags = 10, selectlags = "BIC")


# VECM

library("urca")



vecm1 <- ca.jo(dataset[, c(liabvarset,nonfinvarset,"GNI")], ecdet = "const", type="trace", K=2, spec="longrun", season=4)
vecm1e <- ca.jo(dataset[, c(liabvarset,nonfinvarset,"GNI")], ecdet = "const", type="eigen", K=2, spec="longrun", season=4)
cajorls(vecm1, r = 1)


vecm2 <- ca.jo(dirtysectorliab, ecdet = "const", type="trace", K=8, spec="longrun", season=4)
#vecm2 <- ca.jo(cbind(dirtysectorliab,income[,3]), ecdet = "const", type="eigen", K=8, spec="longrun", season=4)
