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

#' @return A modified data frame with resolveable duplicates removed and a flag field added and populated.

#' @details 
#' Duplicates are identified based on exact matches in the values of the primary key. For records with identical keys, these steps are followed, in order: (1) If records are identical except for NA or empty string values, the non-empty values are kept. (2) If records are identical except for uid, remarks, and/or personnel (xxxxBy) fields, unique values are concatenated within each field, and the merged version is kept. (3) For records that are identical following steps 1 and 2, one record is kept and flagged with duplicateRecordQF=1. (4) Records that can't be resolved by steps 1-3 are flagged with duplicateRecordQF=2. Note that in a set of three or more duplicates, some records may be resolveable and some may not; if two or more records are left after steps 1-3, all remaining records are flagged with duplicateRecordQF=2.

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

removeDups <- function(data, variables, table=NA_character_) {
  
  if(is.na(table)) {
    table <- deparse(substitute(data))
  }
  
  # ensure data frames
  variables <- as.data.frame(variables, stringsAsFactors=F)
  data <- as.data.frame(data, stringsAsFactors=F)
  
  # check table matching
  if(length(which(variables$table==table))==0) {
    stop(paste("Table name", table, "does not match any table in variables file."))
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
        stop("Input data appear to be the basic download package. The expanded data package is required for removeDups() to identify all duplicates correctly.")
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
    
    # iterate over unique key values
    dup.keys <- cbind(unique(data.sub[,key]))
    message(paste(nrow(dup.keys), "duplicated key values found, representing",
        nrow(data.sub), "non-unique records. Attempting to resolve.", sep=" "))
    pb <- utils::txtProgressBar(style=3)
    utils::setTxtProgressBar(pb, 1/nrow(dup.keys))
    ct <- 0
    for(i in 1:nrow(dup.keys)) {
      
      # check for NA key values
      if(ncol(dup.keys)==1) {
        na.check <- dup.keys[i]
        dup.keyvalue <- dup.keys[i]
      } else {
        na.check <- dup.keys[i,]
        dup.keyvalue <- paste0(dup.keys[i,])
      }
      if(all(is.na(na.check))) {
        data$duplicateRecordQF[which(data$keyvalue %in% dup.keyvalue)] <- -1
        next
      }
      
      # subset data to one key value
      if(ncol(dup.keys)==1) {
        data.dup <- data.sub[which(data.sub[,key] == dup.keys[i]),]
      } else {
        data.dup <- data.sub[which(do.call(paste0, data.sub[key]) == 
                                     do.call(paste0, dup.keys)[i]),]
      }
      data.dup$rowid <- as.numeric(data.dup$rowid)
      
      # assign a QF value of 1 in the original data
      data$duplicateRecordQF[which(data$keyvalue %in% data.dup$keyvalue)] <- 1
      
      # if a field is NA in one duplicate but populated in the other,
      # copy the value from the populated one into the NA
      # do it in both the original data and in the subset
      for(j in 1:nrow(data.dup)) {
        if(all(!is.na(data.dup[j,]))) {
          data <- data
        } else {
          for(k in 1:nrow(data.dup)) {
            if(all(data.dup[k,-which(colnames(data.dup) %in% c("uid","rowid"))] == 
                   data.dup[j,-which(colnames(data.dup) %in% c("uid","rowid"))], na.rm=T)) {
              data[which(data$rowid==data.dup$rowid[j]),][which(is.na(data.dup[j,]))] <- 
                data[which(data$rowid==data.dup$rowid[k]),][which(is.na(data.dup[j,]))]
              data.dup[j,][which(is.na(data.dup[j,]))] <- 
                data.dup[k,][which(is.na(data.dup[j,]))]
            } else {
              data[which(data$rowid==data.dup$rowid[j]),] <- 
                data[which(data$rowid==data.dup$rowid[j]),]
            }
          }
        }
      }
      
      # if all fields are duplicated besides remarks, personnel, and uid, 
      # concatenate non-data fields with pipe separators
      # do it in both the original and the subset
      not.data <- c("rowid", "uid", colnames(data)[grep("emarks", colnames(data))], 
                    colnames(data)[grep("edBy", colnames(data))])
      # if data columns differ, duplicate can't be resolved
      if(nrow(unique(data.dup[,setdiff(colnames(data.dup), not.data)]))==nrow(data.dup)) {
        data <- data
      } else {
        for(k in not.data[-which(not.data=="rowid")]) {
          if(length(unique(data.dup[,k]))==1) {
            next
          } else {
            dup.not <- data.dup$rowid[union(which(duplicated(
              data.dup[,setdiff(colnames(data.dup), not.data)])), 
              which(duplicated(data.dup[,setdiff(colnames(data.dup), not.data)], 
                                fromLast=T)))]
            data[which(data$rowid %in% dup.not),k] <- 
              paste(unique(data[which(data$rowid %in% dup.not),k]), collapse="|")
            data.dup[which(data.dup$rowid %in% dup.not),k] <- 
              paste(unique(data.dup[which(data.dup$rowid %in% dup.not),k]), collapse="|")
          }
        }
      }
      
      # delete all but one of the set of duplicate records
      dup.rows <- data.dup$rowid[which(duplicated(data.dup[,-which(colnames(data.dup)=="rowid")]))]
      if(length(dup.rows)>0) {
        data <- data[-which(data$rowid %in% dup.rows),]
      }
      ct <- ct + length(dup.rows)
      utils::setTxtProgressBar(pb, i/nrow(dup.keys))
    }
    
    # flag remaining, unresolved duplicates
    # if some in a group are resolved and some aren't, all end up with QF=2
    unres.dups <- union(which(duplicated(data$keyvalue)), 
                        which(duplicated(data$keyvalue, fromLast=T)))
    if(any(data$duplicateRecordQF==-1)) {
      unres.dups <- setdiff(unres.dups, which(data$duplicateRecordQF==-1))
    }
    data$duplicateRecordQF[unres.dups] <- 2
    
    utils::setTxtProgressBar(pb, 1)
    close(pb)
    
    if(nrow(unique(cbind(data.low[,key])))!=nrow(data.low)) {
      message(paste(ct, " resolveable duplicates merged into matching records\n", length(which(data$duplicateRecordQF==1)), 
          " resolved records flagged with duplicateRecordQF=1", sep=""))
    }
    
    if(length(which(data$duplicateRecordQF==2))>0) {
      message(paste(length(which(data$duplicateRecordQF==2)), 
          " unresolveable duplicates flagged with duplicateRecordQF=2", sep=""))
    }
    
    if(length(which(data$duplicateRecordQF==-1))>0) {
      message(paste(length(which(data$duplicateRecordQF==-1)), 
          " records could not be evaluated due to missing primary key values and are flagged with duplicateRecordQF=-1", sep=""))
    }
    
  }
  
  # remove key value field and return data
  data <- data[,-which(colnames(data) %in% c("rowid","keyvalue"))]
  return(data)
  
}
