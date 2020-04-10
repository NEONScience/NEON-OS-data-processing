##############################################################################################
#' @title Remove duplicates from a data table based on a provided primary key; flag duplicates that can't be removed.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}
#' Eric Sokol \email{esokol@battelleecology.org}

#' @description
#' NEON observational data may contain duplicates; this function removes exact duplicates, attempts to resolve non-exact duplicates, and flags duplicates that can't be resolved.
#'
#' @param data For use with removeDups. A data frame containing data from a NEON observational data table [data frame]
#' @param variables For use with removeDups. The NEON variables file containing metadata about the data table in question [data frame]
#' @param table For use with removeDups. The name of the table. Must match one of the table names in 'variables' [character]
#' @param tab For use with removeDups.L1(). Name of table for which to get data, in the form DP#.#####.001:table_name_in. Defaults to NA, # [character]
#' @param L1Data For use with removeDups.L1(). L1 data, a data.frame or tibble
#' @param pubWB For use with removeDups.L1(). Pub workbook, a data.frame or tibble. 
#' @param tableName For use with removeDups.L1(). A pub table name [character]
#' @param stack For use with removeDups.L1(). Stack to query if using restR. Can be one of 'int','cert', or 'prod'. Defaults to 'prod'
#' @param ... For use with removeDups.L1(). Other options passed to restR::get.os.l1.by.tab.all.opts(), use ?restR::get.os.l1.by.tab.all.opts for argument descriptions

#' @return A modified data frame with duplicates removed and a flag field added and populated.

#' @details 
#' Duplicates are identified based on exact matches in the values of the primary key. For records with identical keys, these steps are followed, in order: (1) If records are identical except for NA or empty string values, the non-NA values are kept. (2) If records are identical except for uid, remarks, and/or personnel (xxxxBy) fields, unique values are concatenated within each field, and the merged version is kept. (3) For records that are identical following steps 1 and 2, one record is kept and flagged with duplicateRecordQF=1. (4) Records that can't be resoved by steps 1-3 are flagged with duplicateRecordQF=2. Note that in a set of three or more duplicates, some records may be resolveable and some may not; if two or more records are left after steps 1-3, all remaining records are flagged with duplicateRecordQF=2. 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' @examples 
#' 
#' \dontrun{
#'
#' # using removeDups.L1, example 1 -- 
#' # pulling L1 data from restR within the function and then checking for dups
#' 
#' mam_perplotnight_pub__dups_flagged_ex1 <- removeDups.L1(
#'   tab = 'DP1.10072.001:mam_perplotnight_pub', 
#'   tableName = NA, #maps to table #optional
#'   minStartDate = '2020-01-01',
#'   maxStartDate = '2020-01-31')
#' 
#' 
#' # using removeDups.L1, example 2 -- 
#' # sending L1 data to fxn, but pulling pubWB from restR
#' 
#' # get L1 data
#' my_L1_data <- restR::get.os.l1.by.tab.all.opts(
#'   tab = 'DP1.10072.001:mam_perplotnight_pub',
#'   inclSamples = 'true',
#'   minStartDate = '2020-01-01',
#'   maxStartDate = '2020-01-31')
#' 
#' # send L1 data to dup check function, get putwb using restR
#' mam_perplotnight_pub__dups_flagged_ex2 <- removeDups.L1(
#'   tab = 'DP1.10072.001:mam_perplotnight_pub', 
#'   L1Data = my_L1_data) 
#' 
#' 
#' # using removeDups.L1, example 3 -- 
#' # sending L1 data and pub wb to fxn, using tab argument
#' 
#' # get L1 data
#' my_L1_data <- restR::get.os.l1.by.tab.all.opts(
#'   tab = 'DP1.10072.001:mam_perplotnight_pub',
#'   inclSamples = 'true',
#'   minStartDate = '2020-01-01',
#'   maxStartDate = '2020-01-31')
#' 
#' # get pubWB
#' my_pubWB <- restR::get.pub.workbook(
#'   DPID = 'DP1.10072.001', 
#'   table = 'mam_perplotnight_pub', 
#'   stack = 'prod')
#'  
#' # send L1 data to dup check function, get putwb using restR
#' mam_perplotnight_pub__dups_flagged_ex3 <- removeDups.L1(
#'   tab = 'DP1.10072.001:mam_perplotnight_pub', 
#'   pubWB = my_pubWB,
#'   L1Data = my_L1_data) 
#' 
#' 
#' # using removeDups.L1, example 4 -- 
#' # sending L1 data and pub wb to fxn, using tableName argument
#' 
#' # get L1 data
#' my_L1_data <- restR::get.os.l1.by.tab.all.opts(
#'   tab = 'DP1.10072.001:mam_perplotnight_pub',
#'   inclSamples = 'true',
#'   minStartDate = '2020-01-01',
#'   maxStartDate = '2020-01-31')
#' 
#' # get pubWB
#' my_pubWB <- restR::get.pub.workbook(
#'   DPID = 'DP1.10072.001', 
#'   table = 'mam_perplotnight_pub', 
#'   stack = 'prod')
#' 
#' # send L1 data to dup check function, get putwb using restR
#' mam_perplotnight_pub__dups_flagged_ex4 <- removeDups.L1(
#'   tableName = 'mam_perplotnight_pub',
#'   pubWB = my_pubWB,
#'   L1Data = my_L1_data) 
#' 
#' 
#' # using removeDups.L1, example 5 -- 
#' # should error out, you need to send either 'tab' or 'tableName'
#' 
#' # get L1 data
#' my_L1_data <- restR::get.os.l1.by.tab.all.opts(
#'   tab = 'DP1.10072.001:mam_perplotnight_pub',
#'   inclSamples = 'true',
#'   minStartDate = '2020-01-01',
#'   maxStartDate = '2020-01-31')
#' 
#' # get pubWB
#' my_pubWB <- restR::get.pub.workbook(
#'   DPID = 'DP1.10072.001', 
#'   table = 'mam_perplotnight_pub', 
#'   stack = 'prod')
#' 
#' # send L1 data to dup check function, get putwb using restR
#' mam_perplotnight_pub__dups_flagged_ex5 <- removeDups.L1(
#'   pubWB = my_pubWB,
#'   L1Data = my_L1_data) 
#' 
#' 
#' # using removeDups.L1, example 5 -- 
#' # should error out, you need to send arguments to get L1 data or L1Data
#' 
#' # get pubWB
#' my_pubWB <- restR::get.pub.workbook(
#'   DPID = 'DP1.10072.001', 
#'   table = 'mam_perplotnight_pub', 
#'   stack = 'prod')
#' 
#' # send L1 data to dup check function, get putwb using restR
#' mam_perplotnight_pub__dups_flagged_ex6 <- removeDups.L1(
#'   tableName = 'mam_perplotnight_pub',
#'   pubWB = my_pubWB) 
#' #' 
#' }#END DONTRUN

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2018-11-27)
#     developed based on functionality written by
#   Natalie Robinson (2015-06-02)
#   Sarah Elmendorf (2015-08-19)
#   Eric Sokol (2020-04-10) - added wrapper function to get L1 data
#     and pubWB from PDR API using restR package
##############################################################################################

removeDups <- function(data, variables, table) {
  
  # Initiate messages
  messages <- NA
  
  # ensure data frames
  variables <- as.data.frame(variables, stringsAsFactors=F)
  data <- as.data.frame(data, stringsAsFactors=F)
  
  # check table matching
  if(length(which(variables$table==table))==0) {
    stop("Table name does not match any table in variables file.")
  }
  
  # remove fields not published
  if(length(which(variables$downloadPkg=="none"))>0) {
    variables <- variables[which(variables$downloadPkg!="none"),]
  }
  
  # check field names
  if(!all(names(data) %in% variables$fieldName[which(variables$table==table)]) |
     !all(variables$fieldName[which(variables$table==table)] %in% names(data))) {
    stop("Field names in data do not match variables file.")
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
      cat("Data contain identical records with identical uid. This indicates data have been combined from multiple downloads. Duplicate records have been removed.\n\n")
    } else {
      stop("Data contain records with identical uid but differing data values. This indicates data have been combined from multiple downloads, and data were reprocessed between downloads.\nStart over with a fresh download of the current data.")
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
  data.low[data.low==''] <- NA
  
  # check and see if there are duplicates of the primary key
  # if no, skip to flagging
  if(nrow(unique(cbind(data.low[,key])))==nrow(data.low)) {
    data <- data
    cat("No duplicated key values found!\n")
  } else {
    
    # subset to only the records with duplicate values in the key fields
    data.sub <- data.low[union(which(duplicated(data.low[,key])),
                                        which(duplicated(data.low[,key], fromLast=T))),]
    
    # iterate over unique key values
    dup.keys <- cbind(unique(data.sub[,key]))
    na.check <- apply(dup.keys, 2, FUN=function(x){all(!is.na(x))})
    if(!all(na.check==T)) {
      cat("Primary key fields contain NA values and/or empty strings. Results may be unreliable; check input data carefully.\n")
    }
    cat(nrow(dup.keys), "duplicated key values found, representing",
        nrow(data.sub), "non-unique records. Attempting to resolve.\n")
    pb <- utils::txtProgressBar(style=3)
    utils::setTxtProgressBar(pb, 1/nrow(dup.keys))
    ct <- 0
    for(i in 1:nrow(dup.keys)) {
      
      #print(dup.keys[i,])
      
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
    
  }
  
  # flag remaining, unresolved duplicates
  # if some in a group are resolved and some aren't, all end up with QF=2
  unres.dups <- union(which(duplicated(data$keyvalue)), 
                      which(duplicated(data$keyvalue, fromLast=T)))
  data$duplicateRecordQF[unres.dups] <- 2
  
  if(nrow(unique(cbind(data.low[,key])))!=nrow(data.low)) {
    utils::setTxtProgressBar(pb, 1)
    close(pb)
    cat(ct, " resolveable duplicates merged into matching records\n", length(which(data$duplicateRecordQF==1)), 
        " resolved records flagged with duplicateRecordQF=1\n", 
        length(which(data$duplicateRecordQF==2)), 
        " unresolveable duplicates flagged with duplicateRecordQF=2\n", sep="")
  }
  # remove key value field and return data
  data <- data[,-which(colnames(data) %in% c("rowid","keyvalue"))]
  return(data)
  
}





#' @describeIn removeDups method to work with L1 data from PDR
#' @export
removeDups.L1 <- function(
  tab = NA, #Name of table for which to get data, in the form DP#.#####.001:table_name_in. [character]
  L1Data = data.frame(), #a data frame, #optional
  pubWB = data.frame(), #maps to variables, #optional
  tableName = NA, #maps to table #optional
  stack = 'prod', #can be one of 'int','cert','prod'
  ... #other options passed to get.os.l1.by.tab.all.opts, see ?restR::get.os.l1.by.tab.all.opts
  ){
  
  
  # extract DPID and tableName if tab is provided
  
  if(!is.na(tab)){
    tab_elements <- unlist(strsplit(tab, split = ':', fixed = TRUE))
    DPID <- tab_elements[1]
    tableName <- tab_elements[2]
  }else if(is.na(tableName)){
    stop("please provide a value for either 'tab' or 'tableName'")
  }
  
  # get if no pubWB provided, get one using restR
  if(nrow(pubWB) == 0){
    pubWB <- restR::get.pub.workbook(DPID = DPID, table = tableName, stack = stack)
  }
  
  
  # if no L1 data provided, get L1 data using restR
  
  if(nrow(L1Data)==0){
    
    if(is.na(tab)){
      stop("Please provide 'L1Data' or arguments to get L1 data from restR.\nsee help files for getting L1 data using '?restR::get.os.l1.by.tab.all.opts'")
    }else{
      L1Data <- restR::get.os.l1.by.tab.all.opts(
        tab = tab,
        ...)
    }
    
    # # for testing
    # L1Data <- restR::get.os.l1.by.tab.all.opts(
    #   tab = tab,
    #   # arguments to be passed with ...
    #   minStartDate = my_start_date,
    #   maxStartDate = my_end_date,
    #   inclSamples = 'true')
  }
  

  # get required pubWB fieldnames
  
  required_pub_field_names <- pubWB %>% 
    dplyr::filter(table == tableName, downloadPkg != 'none') %>%
    dplyr::select(fieldName) %>%
    unlist(use.names = FALSE)
  
  
  # get L1 column names
  
  L1names <- names(L1Data)
  
  
  # filter out fields that bork the function
  
  L1DataFormatted <- L1Data[,L1names %>% dplyr::intersect(required_pub_field_names)]
  
  
  # do dupe check
  
  out <- neonOSbase::removeDups(
    data = L1DataFormatted,
    variables = pubWB, 
    table = tableName)
  
  
  # return output from removeDups fxn
  
  return(out)
}
