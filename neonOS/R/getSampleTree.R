##############################################################################################
#' @title Find all relatives (parents, children, and outward) of a given sample.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Find all samples in the sample tree of a given sample.
#'
#' @param sampleNode A NEON sample identifier. [character]
#' @param idType Is sampleNode a tag, barcode, or guid? Defaults to tag. [character]
#' @param sampleClass The NEON sampleClass of sampleNode. Required if sampleNode is a tag and there are multiple valid classes. [character]
#' @param token User specific API token (generated within neon.datascience user accounts). Optional. [character]

#' @return A table of sample identifiers, their classes, and their parent samples.

#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2021-11-11): original creation

##############################################################################################

getSampleTree <- function(sampleNode, idType="tag", 
                          sampleClass=NA_character_, token=NA_character_) {
  
  # if using tag, first check classes
  if(idType=="tag" & is.na(sampleClass)) {
    req <- getAPI(paste("http://data.neonscience.org/api/v0/samples/classes?sampleTag=", 
                        sampleNode, sep=""), token=token)
    
    req.content <- httr::content(req, as="parsed")
    if(!is.null(req.content$error$status)) {
      message(paste("Sample", sampleNode, "not found."))
      return()
    } else {
      cls <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"))$data$sampleClasses
      if(length(cls)==1) {
        sampleClass <- cls
      } else {
        message(paste("Multiple sampleClasses found for tag ", sampleNode, 
                      ". Re-try with one of the valid sampleClasses:", sep=""))
        message(paste(cls, collapse="\n"))
        return()
      }
    }
  }
  
  if(idType=="tag") {
    req <- getAPI(paste("http://data.neonscience.org/api/v0/samples/view?sampleTag=", 
                        sampleNode, "&sampleClass=", sampleClass, sep=""), token=token)
  } else {
    if(idType=="barcode") {
      req <- getAPI(paste("http://data.neonscience.org/api/v0/samples/view?barcode=", 
                          sampleNode, sep=""), token=token)
    } else {
      if(idType=="guid") {
        req <- getAPI(paste("http://data.neonscience.org/api/v0/samples/view?archiveGuid=", 
                            sampleNode, sep=""), token=token)
      } else {
        if(idType=="uuid") {
          req <- getAPI(paste("http://data.neonscience.org/api/v0/samples/view?sampleUuid=", 
                              sampleNode, sep=""), token=token)
        }
      }
    }
  }

  req.content <- httr::content(req, as="parsed")
  if(!is.null(req.content$error$status)) {
    message(paste("Sample", sampleNode, "not found."))
    return()
  } else {
    
    sampFoc <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), simplifyVector=TRUE)
    sampUUID <- sampFoc$data$sampleViews$sampleUuid
    
    # first get all ancestors, then get descendants of each ancestor
    parents <- getSampleParents(sampUUID, token=token)
    descendants <- data.table::rbindlist(lapply(parents$sampleUuid, getSampleChildren, token), fill=TRUE)
    sampAll <- data.table::rbindlist(list(parents, descendants), fill=TRUE)
    
    # remove duplicates
    sampAll <- sampAll[!duplicated(sampAll),]
    
    # remove parent sample rows without their own parents
    ind <- numeric()
    for(i in sort(union(which(duplicated(sampAll$sampleUuid)), 
                        which(duplicated(sampAll$sampleUuid, fromLast=TRUE))))) {
      if(!is.na(sampAll$parentSampleUuid[i])) {
        next
      }
      si <- sampAll$sampleUuid[i]
      if(is.na(sampAll$parentSampleUuid[i])) {
        if(!all(is.na(sampAll$parentSampleUuid[which(sampAll$sampleUuid==si)]))) {
          ind <- c(ind, i)
        }
      }
    }
    if(length(which(is.na(sampAll$sampleUuid)))>0) {
      ind <- c(ind, which(is.na(sampAll$sampleUuid)))
    }
    sampAll <- sampAll[-ind,]
    
    # attempt to order (would be great to improve. this at least puts root samples at the top)
    sampAll <- sampAll[order(sampAll$parentSampleClass, na.last=FALSE),]
    
    return(sampAll)
    
  }
  
}
