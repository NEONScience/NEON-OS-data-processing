#' Get NEON taxon tables via the Backbone taxonomy data product (DP1.10300.001)
#'
#' @author  Claire K. Lunch \email{clunch@battelleecology.org}
#'
#' @description This is a function to retrieve the taxon and taxon location tables from the NEON Backbone taxonomy data product.
#'
#' @param taxonType The taxonTypeCode to access. Must be one of ALGAE, BEETLE, BIRD, FISH, HERPETOLOGY, MACROINVERTEBRATE, MOSQUITO, MOSQUITO_PATHOGENS, SMALL_MAMMAL, PLANT, TICK [character]
#' @param site Either "all" or a set of 4-letter site codes. [character]
#' @param locations True or false, include the taxon location table for the selected taxonType and site(s). Defaults to T. [logical]
#' @param download True or false, start by downloading the backbone taxonomy data. Defaults to T, set to F if the product is already downloaded. [logical]
#' @param taxonData If download=F, input the data table from DP1.10300.001 that matches the taxon type you want to access. Omit if download=T. [data.frame]
#' @param release The data release to be downloaded; either 'current' or the name of a release, e.g. 'RELEASE-2021'. 'current' returns the most recent release. [character]
#' @param token User specific API token (generated within neon.datascience.org user account). Required if download=T. [character]
#'
#' @return List of data frames with selected NEON taxonomic data
#'
#' @examples	
#' # taxonType must be one of	
#' # ALGAE, BEETLE, BIRD, FISH,	
#' # HERPETOLOGY, MACROINVERTEBRATE, 
#' # MOSQUITO, MOSQUITO_PATHOGENS,	
#' # SMALL_MAMMAL, PLANT, TICK	
#' #################################	
#' 
#'
#' @references License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#'
#'
#' @export
#' 
# changelog and author contributions / copyrights
#   created by Claire Lunch (2026-11-13)
#   
##############################################################################################

getTaxonBackbone <- function(
  taxonType = NA,
  site = "all",
  locations = TRUE,
  download = TRUE,
  taxonData = NA,
  release = "current",
  token = NA_character_
  ){

  if(!taxonType %in% c("ALGAE", "BEETLE", "BIRD", "FISH",	
                       "HERPETOLOGY", "MACROINVERTEBRATE", 
                       "MOSQUITO", "MOSQUITO_PATHOGENS",	
                       "SMALL_MAMMAL", "PLANT", "TICK")) {
    stop(paste(taxonType, "is not a valid NEON taxon type."))
  }
  tabName <- paste("tax", base::tolower(taxonType), sep="_")
  
  if(isTRUE(download)) {
    
    taxD <- neonUtilities::loadByProduct(dpID="DP1.10300.001",
                                         site=site,
                                         startdate=NA, enddate=NA,
                                         package="basic",
                                         release=release,
                                         timeIndex="all",
                                         tabl=tabName,
                                         hor=NA, ver=NA,
                                         cloud.mode=FALSE,
                                         include.provisional=TRUE,
                                         check.size=FALSE,
                                         progress=FALSE,
                                         token=token
                                         )
    taxB <- taxD[[tabName]]
    
  } else {
    taxB <- taxonData
  }
  
  # get taxon list for the type
  taxonList <- utils::read.csv(unique(taxB$taxonomyUrl))
  
  # get taxon location table matching sites requested, if location data are requested
  if(isTRUE(locations)) {
    if(all(site=="all")) {
      taxL <- unique(taxB$taxonomyLocationUrl)
    } else {
      taxSite <- taxB[which(taxB$siteID %in% site),]
      taxL <- unique(taxSite$taxonomyLocationUrl)
    }
    
    if(length(taxL)==0) {
      stop(paste("No taxon location data found for taxon type", 
                 taxonType, "and sites", paste(site, collapse=" "), sep=" "))
    }
    
    if(length(taxL)==1) {
      taxonLocation <- utils::read.csv(taxL)
    } else {
      listLocF <- list()
      for(i in 1:length(taxL)) {
        listLocF[[i]] <- utils::read.csv(taxL[i])
      }
      taxonLocation <- data.table::rbindlist(listLocF, fill=TRUE)
    }
    return(list(taxonList=taxonList, taxonLocation=taxonLocation))
  } else {
    return(taxonList)
  }
}
