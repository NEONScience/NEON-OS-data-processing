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
                          name2=NA_character_) {
  
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
  #tjt <- read.csv("https://github.com/NEONScience/NEON-quick-start-guides/blob/main/allTableJoins.csv")
  # temporary method
  tjt <- read.csv("/Users/clunch/GitHub/NEON-quick-start-guides/allTableJoins.csv")
  
  # check that both tables appear in TJT
  nt <- setdiff(c(name1, name2), c(tjt$Table1, tjt$Table2))
  if(length(nt)>0) {
    stop(paste("Table names", paste(nt, collapse=" and "), "not found in quick start guides."))
  }

  # check that the two tables appear together
  # and check if they both join to a third table via the same field?
  
}
