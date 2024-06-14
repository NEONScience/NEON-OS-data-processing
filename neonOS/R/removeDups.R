##############################################################################################
#' @title Remove duplicates from a data table based on a provided primary key; flag duplicates that can't be removed.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' NEON observational data may contain duplicates; this function removes exact duplicates, attempts to resolve non-exact duplicates, and flags duplicates that can't be resolved.
#'
#' @param data A data frame containing data from a NEON observational data table [data frame]
#' @param variables The NEON variables file containing metadata about the data table in question [data frame]
#' @param table The name of the table. Must match one of the table names in 'variables' [character]
#' @param ncores The maximum number of cores to use for parallel processing. Defaults to 1. [numeric]

#' @return A modified data frame with resolveable duplicates removed and a flag field added and populated.

#' @details 
#' Duplicates are identified based on exact matches in the values of the primary key. For records with identical keys, these steps are followed, in order: (1) If records are identical except for NA or empty string values, the non-empty values are kept. (2) If records are identical except for uid, remarks, and/or personnel (xxxxBy) fields, unique values are concatenated within each field, and the merged version is kept. (3) For records that are identical following steps 1 and 2, one record is kept and flagged with duplicateRecordQF=1. (4) Records that can't be resolved by steps 1-3 are flagged with duplicateRecordQF=2. Note that in a set of three or more duplicates, some records may be resolveable and some may not; if two or more records are left after steps 1-3, all remaining records are flagged with duplicateRecordQF=2. In some limited cases, duplicates can't be unambiguously identified, and these records are flagged with duplicateRecordQF=-1.

#' @examples	
#' # Resolve and flag duplicates in a test dataset of foliar lignin
#' lig_dup <- removeDups(data=cfc_lignin_test_dups, 
#'                       variables=cfc_lignin_variables,
#'                       table="cfc_lignin")

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2018-11-27)
#     developed based on functionality written by
#   Natalie Robinson (2015-06-02)
#   Sarah Elmendorf (2015-08-19)
##############################################################################################

removeDups <- function(data, variables, 
                       table=NA_character_,
                       ncores=1) {
  
  if(is.na(table)) {
    table <- deparse(substitute(data))
  }
  
  # ensure data frames
  variables <- as.data.frame(variables, stringsAsFactors=F)
  data <- as.data.frame(data, stringsAsFactors=F)
  
  # check for enough data to run
  if(nrow(data)==1) {
    data$duplicateRecordQF <- 0
    warning("Only one row of data present. duplicateRecordQF set to 0.")
    return(data)
  } else {
    if(nrow(data)==0) {
      data$duplicateRecordQF <- numeric()
      warning("Data table is empty.")
      return(data)
    }
  }
  
  # check table matching
  if(length(which(variables$table==table))==0) {
    stop(paste("Table name", table, "does not match any table in variables file."))
  }
  
  # exceptions for specific data tables
  if(table=="brd_countdata") {
    stop("Duplicates cannot be unambiguously identified in brd_countdata. Multiple birds can be observed separately during the same observation minute, at the same distance.")
  }
  if(table=="vst_apparentindividual") {
    if(min(data$date) <= as.POSIXct("2021-12-31", tz="GMT")) {
      message("In vst_apparentindividual, tempStemID indicates different stems of a multi-stem individual. This temporary ID was rolled out during the 2019-2021 field seasons. In data collected prior to its implementation, multi-stem individuals cannot be distiguished from duplicates, and are flagged with -1, meaning could not be evaluated.")
    }
  }
  if(table=="mam_pertrapnight") {
    if(any(grepl(pattern="X", x=data$trapCoordinate)) | 
       any(grepl(pattern="4", x=data$trapStatus))) {
      message("In rare situations, duplicates cannot be unambiguously identified in mam_pertrapnight. These cases are (1) when multiple individuals are found in a single trap, and cannot be tagged, and (2) when multiple trap locations are disturbed, indicated by trap coordinates labeled with Xs, and the same capture data are found in each. These two scenarios are flagged with -1, meaning could not be evaluated.")
    }
  }
  
  # remove fields not published
  if(length(which(variables$downloadPkg=="none"))>0) {
    variables <- variables[which(variables$downloadPkg!="none"),]
  }
  
  # check field names
  varnames <- variables$fieldName[which(variables$table==table)]
  if(!all(names(data) %in% varnames) |
     !all(varnames %in% names(data))) {
    dif <- setdiff(varnames, names(data))
    if(length(dif)!=0) {
      if(all(dif %in% variables$fieldName[which(variables$downloadPkg=="expanded" & 
                                                variables$table==table)])) {
        stop(paste("Field names in data do not match variables file.\n",
                   paste0(setdiff(names(data), varnames), collapse=" "), 
                   ifelse(length(setdiff(names(data), varnames))>0, 
                          " are in data and not in variables file;\n",
                          ""), 
                   paste0(setdiff(varnames, names(data)), collapse=" "), 
                   ifelse(length(setdiff(varnames, names(data)))>0,
                          " are in variables file and not in data. The missing data fields are in the expanded package, suggesting the data file may be from the basic package; if an expanded package exists for a given table, removeDups() can only be used with the expanded package.",
                          ""), sep=""))
      }
    }
    stop(paste("Field names in data do not match variables file.\n",
               paste0(setdiff(names(data), varnames), collapse=" "), 
               ifelse(length(setdiff(names(data), varnames))>0, 
                      " are in data and not in variables file;\n",
                      ""), 
               paste0(setdiff(varnames, names(data)), collapse=" "), 
               ifelse(length(setdiff(varnames, names(data)))>0,
                      " are in variables file and not in data.",
                      ""), sep=""))
    }
  
  # get primary key
  key <- as.character(variables$fieldName[which(variables$table==table & variables$primaryKey=="Y")])
  if(length(key)==0) {
    stop("No primary key identified in variables file.")
  }
  
  # first check for duplicate uids
  if(length(unique(data$uid)) != length(data$uid)) {

    # check if entire records are duplicates
    if(length(which(duplicated(data)))==length(which(duplicated(data$uid)))) {
      data <- data[-which(duplicated(data$uid)),]
      message("Data contain identical records with identical uid. This indicates data have been combined from multiple downloads. Duplicate records have been removed without flagging.")
    } else {
      stop("Data contain records with identical uid but differing data values. This indicates something has gone wrong with the data post-download.\nStart over with a fresh download of the current data.")
    }
    
  }
  
  # Initiate duplicateRecordQF field at 0
  data$duplicateRecordQF <- 0
  
  # convert key fields to character (is this needed?)
  if(length(key)>1) {
    data[,which(names(data) %in% key)] <- lapply(data[,which(names(data) %in% key)],
                                                         function (x) as.character(x))
  } else {
    data[,which(names(data) %in% key)] <- as.character(data[,which(names(data) %in% key)])
  }
  
  # make key value field
  if(length(key)==1) {
    data$keyvalue <- tolower(data[,key])
  } else {
    data$keyvalue <- tolower(do.call(paste0, data[key]))
  }
  
  # make identifier that isn't uid (since uids of duplicates will be concatenated)
  data$rowid <- 1:nrow(data)
  
  # convert all data to lowercase for comparison purposes
  data.low <- apply(data, 2, tolower)
  data.low <- data.frame(data.low, stringsAsFactors=F)
  
  # convert any empty strings to NAs
  data.low[data.low==""] <- NA
  
  # check and see if there are duplicates of the primary key
  # if no, skip to flagging
  if(nrow(unique(cbind(data.low[,key])))==nrow(data.low)) {
    data <- data
    message("No duplicated key values found!")
  } else {
    
    # subset to only the records with duplicate values in the key fields
    data.sub <- data.low[union(which(duplicated(data.low[,key])),
                                        which(duplicated(data.low[,key], fromLast=T))),]
    data.sub$rowid <- as.numeric(data.sub$rowid)
    
    # get subset without duplicate values
    data.nodups <- data[which(!data$rowid %in% data.sub$rowid),]
    
    # iterate over unique key values
    dup.keys <- cbind(unique(data.sub[,key]))
    message(paste(nrow(dup.keys), "duplicated key values found, representing",
        nrow(data.sub), "non-unique records. Attempting to resolve.", sep=" "))
    
    # set up parallel cores
    if(nrow(dup.keys)>=100) {
      ncores <- min(ncores, parallel::detectCores()-2, na.rm=TRUE)
    } else {
      ncores <- 1
    }
    cl <- parallel::makeCluster(ncores)
    suppressWarnings(on.exit(parallel::stopCluster(cl)))
    
    # make data frame chunks of one key value each
    dup.list <- list(nrow(dup.keys))
    if(ncol(dup.keys)==1) {
      for(i in 1:nrow(dup.keys)) {
        dup.list[[i]] <- data.sub[which(data.sub[,key] == dup.keys[i]),]
      }
    } else {
      for(i in 1:nrow(dup.keys)) {
        dup.list[[i]] <- data.sub[which(do.call(paste0, data.sub[key]) == 
                                     do.call(paste0, dup.keys)[i]),]
      }
    }

    # make data frame chunks of one key value each from the original (mixed case) data
    data.list <- list(nrow(dup.keys))
    for(i in 1:nrow(dup.keys)) {
       data.list[[i]] <- data[which(data$keyvalue %in% dup.list[[i]]$keyvalue),]
    }
    
    # de-dup each chunk
    proc.data <- parallel::clusterMap(cl=cl, fun=dupProcess, data=data.list, 
                                      data.dup=dup.list, table=table)
    
    # stack chunks and re-order
    data.d <- data.table::rbindlist(proc.data, fill=TRUE)
    data <- data.table::rbindlist(list(data.d, data.nodups), fill=TRUE)
    data <- data[order(data$rowid),]
    data <- data.frame(data)
    
    # calculate de-dup numbers to report to user
    dupdiff <- sum(unlist(lapply(dup.list, FUN=nrow)) - unlist(lapply(proc.data, FUN=nrow)))
    
    if(nrow(unique(cbind(data.low[,key])))!=nrow(data.low)) {
      message(paste(dupdiff, " resolveable duplicates merged into matching records\n", length(which(data$duplicateRecordQF==1)), 
          " resolved records flagged with duplicateRecordQF=1", sep=""))
    }
    
    if(length(which(data$duplicateRecordQF==2))>0) {
      message(paste(length(which(data$duplicateRecordQF==2)), 
          " unresolveable duplicates flagged with duplicateRecordQF=2", sep=""))
    }
    
    if(length(which(data$duplicateRecordQF==-1))>0) {
      message(paste(length(which(data$duplicateRecordQF==-1)), 
          " records could not be evaluated and are flagged with duplicateRecordQF=-1", sep=""))
    }
    
  }
  
  # remove key value field and return data
  data <- data[,-which(colnames(data) %in% c("rowid","keyvalue"))]
  return(data)
  
}
