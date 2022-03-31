##############################################################################################
#' @title Reformat sample data to indicate the parents of a focal sample.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Reformat table of sample identifiers to include parent sample identifiers. Used in getSampleTree().
#'
#' @param sampleUuid A NEON sample UUID. [character]
#' @param token User specific API token (generated within neon.datascience user accounts). Optional. [character]

#' @return A table of sample identifiers, their classes, and their parent samples.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2021-11-11): original creation

##############################################################################################

idSampleParents <- function(sampleUuid, token=NA_character_) {
  
  req <- getAPI(paste("http://data.neonscience.org/api/v0/samples/view?sampleUuid=", 
                      sampleUuid, sep=""), token=token)
  
  samp <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), simplifyVector=FALSE)
  sampFoc <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), simplifyVector=TRUE)
  sampFoc <- sampFoc$data$sampleViews[,!names(sampFoc$data$sampleViews) %in% 
                                          c("parentSampleIdentifiers",
                                            "childSampleIdentifiers",
                                            "sampleEvents")]
  sampParents <- suppressWarnings(data.table::rbindlist(samp$data$sampleViews[[1]]$parentSampleIdentifiers, 
                                                        fill=TRUE))
  sampChildren <- suppressWarnings(data.table::rbindlist(samp$data$sampleViews[[1]]$childSampleIdentifiers, 
                                                           fill=TRUE))
  sampEmp <- data.frame(matrix(data=NA, nrow=1, ncol=5), row.names=NULL)
  names(sampEmp) <- c("parentSampleUuid", "parentSampleTag", "parentSampleClass",
                       "parentSampleBarcode", "parentSampleArchiveGuid")
  
  sampAll <- cbind(sampFoc, sampEmp, row.names=NULL)
  sampAll$done <- rep(NA, nrow(sampAll))
  
  if(nrow(sampChildren)==0) {
    sampAll <- sampAll
  } else {
    sampTemp <- data.frame(cbind(sampFoc$sampleUuid, sampFoc$sampleTag,
                                 sampFoc$sampleClass, sampFoc$barcode, sampFoc$archiveGuid,
                                 row.names=NULL))
    names(sampTemp) <- c("parentSampleUuid", "parentSampleTag", "parentSampleClass",
                         "parentSampleBarcode", "parentSampleArchiveGuid")
    sampChildrenAll <- cbind(sampChildren, sampTemp, row.names=NULL)
    sampAll <- data.table::rbindlist(list(sampAll, sampChildrenAll), fill=TRUE)
  }
  
  if(nrow(sampParents)==0) {
    sampAll <- sampAll
  } else {
    sampTemp <- data.frame(cbind(sampParents$sampleUuid, sampParents$sampleTag,
                                 sampParents$sampleClass, sampParents$barcode, 
                                 sampParents$archiveGuid, row.names=NULL))
    names(sampTemp) <- c("parentSampleUuid", "parentSampleTag", "parentSampleClass",
                         "parentSampleBarcode", "parentSampleArchiveGuid")
    sampParentsAll <- cbind(sampFoc, sampTemp, row.names=NULL)
    sampParentsOnly <- sampTemp
    names(sampParentsOnly) <- c("sampleUuid", "sampleTag", "sampleClass",
                                "barcode", "archiveGuid")
    sampAll <- data.table::rbindlist(list(sampAll, sampParentsOnly, sampParentsAll), fill=TRUE)
  }
  
  return(sampAll)
  
}
