##############################################################################################
#' @title Helper function to remove duplicates from a data table; assumes input data are a duplicated set.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Helper function to carry out duplicate removal on a data table of duplicates.
#'
#' @param data A data frame containing original duplicated data. [data frame]
#' @param data.dup A data frame containing lowercase conversion of the duplicated data. [character]
#' @param table The table name for the input data frame

#' @return A modified data frame with resolveable duplicates removed and a flag field added and populated.

#' @details 
#' Helper function to carry out the flagging and removal for removeDups().

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# changelog and author contributions / copyrights
#   Claire Lunch (2024-06-03)
##############################################################################################

dupProcess <- function(data, data.dup, table) {
 
  # check for NA key values
  if(all(is.na(data.dup$keyvalue))) { # not quite right - keyvalue could be a bunch of concatenated NAs
    data$duplicateRecordQF[which(data$keyvalue %in% data.dup$keyvalue)] <- -1
    return(data)
  }
  
  data$rowid <- as.numeric(data$rowid)
  
  # check for specific cases that can't be evaluated
  # veg structure: multi-stem individuals with empty tempStemID
  if(table=="vst_apparentindividual") {
    if(all(is.na(data.dup$tempStemID))) {
      data$duplicateRecordQF[which(data$keyvalue %in% data.dup$keyvalue)] <- -1
      return(data)
    }
  }
  # mammals: uncertain grid point locations
  if(table=="mam_pertrapnight") {
    if(any(grepl("X", data.dup$trapCoordinate))) {
      data$duplicateRecordQF[which(data$keyvalue %in% data.dup$keyvalue)] <- -1
      return(data)
    }
    # mammals: multiple untagged captures in one trap
    if(any(grepl("4", data.dup$trapStatus))) {
      if(length(which(is.na(data.dup$tagID)))>1) {
        data$duplicateRecordQF[which(data$keyvalue %in% data.dup$keyvalue)] <- -1
        return(data)
      }
    }
  }
  
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
                colnames(data)[grep("edBy", colnames(data))], 
                colnames(data)[grep("dataEntryRecord", colnames(data))])
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
  
  # flag remaining, unresolved duplicates
  # if some in a group are resolved and some aren't, all end up with QF=2
  unres.dups <- union(which(duplicated(data$keyvalue)), 
                      which(duplicated(data$keyvalue, fromLast=T)))
  if(any(data$duplicateRecordQF==-1)) {
    unres.dups <- setdiff(unres.dups, which(data$duplicateRecordQF==-1))
  }
  data$duplicateRecordQF[unres.dups] <- 2
  
  return(data)
  
}
  