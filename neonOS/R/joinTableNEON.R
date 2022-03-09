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
#' @param location.fields Should standard location fields be included in the list of linking variables, to avoid duplicating those fields? Defaults to TRUE. [logical]

#' @return A single data frame created by joining table1 and table2 on the fields identified in the quick start guide.

#' @details 
#' The "Table joining" section of NEON Quick Start Guides (QSGs) provides the field names of the linking variables between related NEON data tables. This function uses the QSG information to join tables. All tables are joined using a full join. If you need to remove duplicates as well as joining, run removeDups() before running joinTableNEON(). Tables that don't appear together in QSG instructions can't be joined here. Some tables may not be straightforwardly joinable, such as tables of analytical standards run as unknowns. Theoretically, these data could be joined to analytical results by a combination of laboratory and date, but in general, a table join is not the best way to analyze this type of data. If a pair of tables is omitted from QSG instructions that you expected to find, contact NEON.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   created by Claire Lunch (2021-09-30)
##############################################################################################

joinTableNEON <- function(table1, table2, 
                          name1=NA_character_, 
                          name2=NA_character_,
                          location.fields=TRUE) {
  
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
  #tjt <- utils::read.csv("https://github.com/NEONScience/NEON-quick-start-guides/blob/main/allTableJoins.csv")
  # temporary method
  tjt <- utils::read.csv("/Users/clunch/GitHub/NEON-quick-start-guides/allTableJoins.csv")
  
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
      if(any(tjt$Table2[which(tjt$Table1 %in% c(name1, name2))]=="Any other table")) {
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
      stop(paste("Tables", name1, "and", name2, "can't be joined automatically. Consult quick start guide for details about data relationships."))
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
      cat("Linking sample fields do not have the same names; barcodes are not included in the joining variables.\n")
    } else {
      codes <- base::union(gsub("ID", "Code", lnk1), gsub("ID", "Code", lnk2))
      codesp <- base::intersect(base::intersect(codes, names(table1)), 
                                base::intersect(codes, names(table2)))
      lnk1 <- base::union(lnk1, codesp)
      lnk2 <- base::union(lnk2, codesp)
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
  
  # Join tables!
  mergetable <- base::merge(table1, table2, by.x=lnk1, by.y=lnk2, all=TRUE)
  return(mergetable)
  
}
