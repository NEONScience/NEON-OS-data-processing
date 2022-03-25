##############################################################################################
#' @title Find all ancestors of a given sample.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Find all samples in the upstream sample tree of a given sample. Used in getSampleTree().
#'
#' @param sampleUuid A NEON sample UUID. [character]
#' @param token User specific API token (generated within neon.datascience user accounts). Optional. [character]

#' @return A table of sample identifiers, their classes, and their parent samples.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2021-11-11): original creation

##############################################################################################

getSampleParents <- function(sampleUuid, token=NA_character_) {
  
  req <- getAPI(paste("http://data.neonscience.org/api/v0/samples/view?sampleUuid=", 
                            sampleUuid, sep=""), token=token)

  req.content <- httr::content(req, as="parsed")
  if(!is.null(req.content$error$status)) {
    message(paste("Sample", sampleUuid, "not found."))
    return()
  } else {
    
    samp <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), simplifyVector=FALSE)
    sampVal <- idSampleParents(sampleUuid, token=NA_character_)
    sampParents <- suppressWarnings(data.table::rbindlist(samp$data$sampleViews[[1]]$parentSampleIdentifiers, 
                                                          fill=TRUE))
    
    if(nrow(sampParents)==0) {
      sampAll <- idSampleParents(sampleUuid, token=NA_character_)
      return(sampAll)
    } else {
      sampAll <- data.table::rbindlist(list(sampVal, 
                                            data.table::rbindlist(lapply(sampVal$sampleUuid, 
                                                                         idSampleParents, token),
                                                                  fill=TRUE)), fill=TRUE)
      return(sampAll)
    }
    
  }
  
  
  
}
