##############################################################################################
#' @title Update table_relationships data frame

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Connect to the database, pull in updated publication workbooks, and save locally. Run periodically to keep workbook info current.

#' @return A saved .csv file

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire K. Lunch (2026-11-25) adapted from update_table_types.R in neonUtilities
##############################################################################################

update_table_relationships <- function(){
  
  # get publication tables from PDR
  req <- httr::GET(Sys.getenv("PUB_TABLES"))
  rawj <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), 
                             simplifyDataFrame=TRUE, flatten=TRUE)
  
  rawt <- tidyr::unnest(rawj$pubTables[c("dataProductId","dataProductName",
                                         "tableName","ingestTableDpId",
                                         "ingestTableName","filterSampleClass",
                                         "pubFields")], 
                        cols=c("pubFields"))
  
  # write table to repo
  data.table::fwrite(rawt, paste(Sys.getenv('NO_REPO'), 'helper_files/table_relationships.csv', sep='/'))

}

update_table_relationships()
