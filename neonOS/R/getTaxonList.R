#' Get NEON taxon table
#'
#' @author  Eric R. Sokol \email{esokol@battelleecology.org}
#'
#' @description This is a function to retrieve a taxon table from the NEON data portal for a given taxon type and provide it in a tractable format.
#'
#' @param taxonType The taxonTypeCode to access. Must be one of ALGAE, BEETLE, BIRD, FISH, HERPETOLOGY, MACROINVERTEBRATE, MOSQUITO, MOSQUITO_PATHOGENS, SMALL_MAMMAL, PLANT, TICK [character]
#' @param recordReturnLimit The number of items to limit the result set to. If NA (the default), will return either the first 100 records, or all records in the table, depending on the value of `stream`. Use `stream='true'` to get all records. [integer]
#' @param stream True or false, obtain the results as a stream. Utilize for large requests. Note this is lowercase true and false as character strings, not logical. [character]
#' @param verbose True or false, include all possible taxonomic parameters. Defaults to false, only essential parameters. Note this is lowercase true and false as character strings, not logical. [character]
#' @param token User specific API token (generated within neon.datascience.org user account) [character]
#'
#' @return Data frame with selected NEON taxonomic data
#'
#' @examples	
#' # taxonTypeCode must be one of	
#' # ALGAE, BEETLE, BIRD, FISH,	
#' # HERPETOLOGY, MACROINVERTEBRATE, 
#' # MOSQUITO, MOSQUITO_PATHOGENS,	
#' # SMALL_MAMMAL, PLANT, TICK	
#' #################################	
#' # get the first 4 fish taxa	
#' taxa_table <- getTaxonList('FISH', recordReturnLimit = 4)
#'
#' @references License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#'
#'
#' @export
#' 
# changelog and author contributions / copyrights
#   created by Eric Sokol (2018)
#   migrated from neonUtilities (as getTaxonTable()) to neonOS and updated with verbose option (Claire Lunch, 2022-09-15)
##############################################################################################

getTaxonList <- function(
  taxonType = NA, #string, taxonTypeCode, one of ALGAE, BEETLE, BIRD, FISH, HERPETOLOY, MACROINVERTEBRATE, MOSQUITO, MOSQUITO_PATHOGENS, SMALL_MAMMAL, PLANT, TICK
  recordReturnLimit = NA, #integer, The number of items to limit the result set to. If NA, will return all records in table.
  stream = 'true', #string, Option to obtain the result as a stream. Utilize for large requests.
  verbose = 'false',
  token = NA
  ){

  url_prefix = 'http://data.neonscience.org/api/v0/taxonomy?taxonTypeCode=' #hard code endpoint into function
  url_to_get <- as.character(paste0(url_prefix, taxonType))

  url_to_get <- paste0(url_to_get,'&offset=0')
  if(!is.na(recordReturnLimit)) {url_to_get <- paste0(url_to_get,'&limit=',recordReturnLimit)}
  if(!is.na(stream)) {url_to_get <- paste0(url_to_get,'&stream=',stream)}
  if(!is.na(verbose)) {url_to_get <- paste0(url_to_get,'&verbose=',verbose)}

  req.df <- data.frame()
  req <- NULL

  req <- getAPI(apiURL = url_to_get, token = token)

  # request code error handling
  if(is.null(req)) {
    message(paste("No data were returned"))
    return(invisible())
  }
  
  if (req$status_code == 204) {
    message(paste("No data are available"))
    return(invisible())
  }else if (req$status_code == 413) {
    message(paste("Data GET failed with status code ", req$status_code,
               ": Payload Too Large. Query a smaller dataset.",
               sep = ""))
    return(invisible())
  }else {
    if (req$status_code != 200) {
      message(paste("Data GET failed with status code ", req$status_code,
                 ". Check the formatting of your inputs.", sep = ""))
      return(invisible())
    }
  }


  if(!is.null(req)){
    taxa_list <- jsonlite::fromJSON(httr::content(req, as='text', encoding="UTF-8"))
    taxa_table <- taxa_list$data

    # get rid of prefixes in column names on left side of ":"
    names(taxa_table) <- gsub('.+?\\:','',names(taxa_table))
  }

  ###########
  return(taxa_table)
}
