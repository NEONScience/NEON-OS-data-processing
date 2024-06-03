##############################################################################################
#' @title Join two data tables from NEON Observational System

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' NEON observational data are published in multiple tables, usually corresponding to activities performed in different times or places. This function uses the fields identified in NEON Quick Start Guides to join tables containing related data.
#'
#' @param table1 A data frame containing data from a NEON observational data table [data frame]
#' @param table2 A second data frame containing data from a NEON observational data table [data frame]
#' @param name1 The name of the first table. Defaults to the object name of table1. [character]
#' @param name2 The name of the second table. Defaults to the object name of table2. [character]
#' @param location.fields Should standard location fields be included in the list of linking variables, to avoid duplicating those fields? For most data products, these fields are redundant, but there are a few exceptions. This parameter defaults to NA, in which case the Quick Start Guide is consulted. If QSG indicates location fields shouldn't be included, value is updated to FALSE, otherwise to TRUE. Enter TRUE or FALSE to override QSG defaults. [logical]
#' @param left.join Should the tables be joined in a left join? This parameter defaults to NA, in which case the Quick Start Guide is consulted. If the QSG does not specify, a full join is performed. Enter TRUE or FALSE to override the default behavior, including using FALSE to force a full join when a left join is specified in the QSG. Forcing a left join is not generally recommended; remember you will likely be discarding data from the second table. [logical]

#' @return A single data frame created by joining table1 and table2 on the fields identified in the quick start guide.

#' @details 
#' The "Table joining" section of NEON Quick Start Guides (QSGs) provides the field names of the linking variables between related NEON data tables. This function uses the QSG information to join tables. Tables are joined using a full join unless the QSG specifies otherwise. If you need to remove duplicates as well as joining, run removeDups() before running joinTableNEON(). Tables that don't appear together in QSG instructions can't be joined here. Some tables may not be straightforwardly joinable, such as tables of analytical standards run as unknowns. Theoretically, these data could be joined to analytical results by a combination of laboratory and date, but in general, a table join is not the best way to analyze this type of data. If a pair of tables is omitted from QSG instructions that you expected to find, contact NEON.

#' @examples	
#' # Join metadata from the point level to individual observations, for NEON bird data
#' all_bird <- joinTableNEON(table1=brd_perpoint, table2=brd_countdata)

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   created by Claire Lunch (2021-09-30)
##############################################################################################

joinTableNEON <- function(table1, table2, 
                          name1=NA_character_, 
                          name2=NA_character_,
                          location.fields=NA,
                          left.join=NA) {
  
  if(!inherits(name1, "character") | length(name1)>1 | 
     !inherits(name2, "character") | length(name2)>1 ) {
    stop(paste("Inputs name1 and name2 should each be a single character string, matching the official NEON names of the input tables. Check values and syntax for these inputs."))
  }
  
  if(is.na(name1)) {
    name1 <- deparse(substitute(table1))
  }
  
  if(is.na(name2)) {
    name2 <- deparse(substitute(table2))
  }
  
  # ensure data frames
  table1 <- as.data.frame(table1, stringsAsFactors=F)
  table2 <- as.data.frame(table2, stringsAsFactors=F)
  
  # get table joining tables (TJT) from NEON-quick-start-guides
  tjt <- tryCatch(utils::read.csv("https://raw.githubusercontent.com/NEONScience/NEON-quick-start-guides/main/allTableJoins.csv", stringsAsFactors=F),
             error=function(e) {return(invisible())}, warning=function(w) {return(invisible())})
  if(!inherits(tjt, "data.frame")) {
    message("Could not access quick start guides, using archived version of table joining instructions. Check for updates when you have internet access.")
    tjt <- table_joins
  }

  # check that both tables appear in TJT
  nt <- base::setdiff(c(name1, name2), c(tjt$Table1, tjt$Table2))
  if(length(nt)>0) {
    stop(paste("Table names", paste(nt, collapse=" and "), "not found in quick start guides."))
  }

  # check that the two tables appear together, and get the linking variables
  ind1 <- base::union(which(tjt$Table1==name1), which(tjt$Table2==name1))
  ind2 <- base::union(which(tjt$Table1==name2), which(tjt$Table2==name2))
  ind <- base::intersect(ind1,ind2)
  
  # if there is one matching entry, use it
  if(length(ind)==1) {
    lnk <- tjt[ind,]
  } else {
    # if there are multiple entries, check that the linking variables match
    if(length(ind)>1) {
      lnk <- base::unique(tjt[ind,])
      if(nrow(lnk)>1) {
        stop(paste("Multiple entries found for tables", name1, "and", name2, "; linking variables do not match. This is a metadata error, please notify NEON using the Contact Us page."))
      }
    } else {
      # if there are no entries, decide which error message to display
      if(any(grepl("Any other table", tjt$Table2[which(tjt$Table1 %in% c(name1, name2))]))) {
        stop(paste("Direct join of table", name1, "to table", name2, "is not recommended. Consult quick start guide for more information."))
      } else {
        stop(paste("Variable(s) to join tables", name1, "and", name2, "are not identified in any quick start guide."))
      }
    }
  }

  # match up table1 and table2
  if(name1==lnk$Table1) {
    table1 <- table1
  } else {
    temp <- table2
    table2 <- table1
    table1 <- temp
  }
  
  # check that linking variables are fields in the two tables
  lnk1 <- base::trimws(unlist(base::strsplit(lnk$JoinByTable1, split=",")))
  lnk2 <- base::trimws(unlist(base::strsplit(lnk$JoinByTable2, split=",")))

  nl1 <- base::setdiff(lnk1, names(table1))
  nl2 <- base::setdiff(lnk2, names(table2))
  
  # if linking variables are not present, figure out which error to display
  if(length(c(nl1, nl2))>0) {
    if(length(grep("automatable", c(nl1, nl2)))>0) {
      stop(paste("Tables", name1, "and", name2, "can't be joined as-is. Consult quick start guide for details about data relationships."))
    } else {
      if(length(grep("recommended", c(nl1, nl2)))>0) {
        stop(paste("Joining tables", name1, "and", name2, "is not recommended. Consult quick start guide for details."))
      } else {
        if(length(grep("intermediate", c(nl1, nl2)))>0) {
          stop(paste("Tables", name1, "and", name2, "can't be joined directly, but can each be joined to a common table(s). Consult quick start guide for details."))
        } else {
          stop(paste("Linking variables", paste(unique(c(nl1, nl2)), collapse=" and "), "not found in one or both data tables. Check quick start guides and check data table inputs."))
        }
      }
    }
  }
  
  # if sample IDs are in the join list, include the corresponding barcodes, after verifying
  # that they are present in both tables
  # this won't work for samples with different names in the two tables. for now, skip that case
  if(length(grep("ID", lnk1))>0 & length(grep("ID", lnk2))>0) {
    if(!identical(grep("ID", lnk1, value=TRUE), grep("ID", lnk2, value=TRUE))) {
      message("Linking sample fields do not have the same names; barcodes are not included in the joining variables.")
    } else {
      codes <- base::union(gsub("ID", "Code", lnk1), gsub("ID", "Code", lnk2))
      codesp <- base::intersect(base::intersect(codes, names(table1)), 
                                base::intersect(codes, names(table2)))
      lnk1 <- base::union(lnk1, codesp)
      lnk2 <- base::union(lnk2, codesp)
    }
  }
  
  # check for special cases - there are currently 3 machine-readable options
  # 1. set location.fields to FALSE, unless overridden by user setting
  if(is.na(location.fields)) {
    if(length(grep("location.fields=FALSE", lnk$Notes))>0) {
      location.fields <- FALSE
    } else {
      location.fields <- TRUE
    }
  } else {
    location.fields <- location.fields
  }
  
  # 2. specify a left join instead of full join
  if(length(grep("left join", lnk$Notes))>0) {
    yTF <- FALSE
  } else {
    yTF <- TRUE
  }
  
  # 3. overrule left/full join if function call specifies
  if(!is.na(left.join)) {
    if(left.join) {
      yTF <- FALSE
    } else {
      yTF <- TRUE
    }
  }
  
  # 4. only proceed if data are from after a certain date
  if(length(grep("[0-9]{4}[-][0-9]{2}[-][0-9]{2}", lnk$Notes))>0) {
    dat <- regmatches(lnk$Notes, regexpr("[0-9]{4}[-][0-9]{2}[-][0-9]{2}", lnk$Notes))
    dat <- as.POSIXct(dat, format="%Y-%m-%d", tz="GMT")
    date.fields <- colnames(table1)[grep("date", colnames(table1), ignore.case=TRUE)]
    date.classes <- sapply(table1[,date.fields], class)
    date.fields <- date.fields[grep("POSIXct", date.classes)]
    data.dat <- min(as.matrix(table1[,date.fields], na.rm=T))
    if(data.dat < dat) {
      stop(paste("For tables ", name1, " and ", name2, 
                 " this function can only be used to join data collected after ", 
                 dat, ". See quick start guide.", sep=""))
    }
  }
  
  # optionally include basic location fields in the linking variables, to avoid unnecessary column duplication
  # only include location fields that are present in both tables
  if(location.fields) {
    lf <- c("domainID", "siteID", "plotID", "locationID", "namedLocation")
    lfp <- base::intersect(base::intersect(lf, names(table1)), 
                           base::intersect(lf, names(table2)))
    lnk1 <- base::union(lnk1, lfp)
    lnk2 <- base::union(lnk2, lfp)
  }
  
  # check for capitalization issues
  intLow <- base::sort(base::toupper(base::intersect(base::unlist(table1[,lnk1]),
                                                     base::unlist(table2[,lnk2]))))
  intUp <- base::sort(base::intersect(base::toupper(base::unlist(table1[,lnk1])),
                                      base::toupper(base::unlist(table2[,lnk2]))))
  # if fields don't match, make everything uppercase
  if(!identical(intLow, intUp)) {
    table1[,lnk1] <- base::apply(table1[,lnk1], MARGIN=2, FUN=base::toupper)
    table2[,lnk2] <- base::apply(table2[,lnk2], MARGIN=2, FUN=base::toupper)
  }
  
  # Join tables!
  mergetable <- base::merge(table1, table2, by.x=lnk1, by.y=lnk2, 
                            all.x=TRUE, all.y=yTF)
  return(mergetable)
  
}
