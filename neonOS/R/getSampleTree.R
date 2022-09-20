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
#' 
#' @details Related NEON samples can be connected to each other in a parent-child hierarchy. Parents can have one or many children, and children can have one or many parents. Sample hierarchies can be simple or complex - for example, particulate mass samples (dust filters) have no parents or children, whereas water chemistry samples can be subsampled for dissolved gas, isotope, and microbial measurements. This function finds all ancestors and descendants of the focal sample (the sampleNode), and all of their relatives, and so on recursively, to provide the entire hierarchy. See documentation for each data product for more specific information.
#' 
#' @examples	
#' # Find related samples for a soil nitrogen transformation sample
#' \dontrun{
#' soil_samp <- getSampleTree(sampleNode="B00000123538", idType="barcode")
#' }

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
    
    if(is.null(req)) {
      return(invisible())
    }
    
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
    
    # get parents and children of focal sample, then repeat process for the parents and children
    sampAll <- idSampleParents(sampUUID, token=token)
    
    # mark the focal sample's records complete
    sampAll$done[which(sampAll$sampleUuid==sampUUID)] <- rep("done",
                                                            length(which(sampAll$sampleUuid==sampUUID)))
    
    # this is mostly working, but records marked as undone keep getting re-introduced
    while(any(is.na(sampAll$done))) {
      sampNew <- sampAll[which(is.na(sampAll$done)),]
      sampNewRel <- data.table::rbindlist(lapply(sampNew$sampleUuid, idSampleParents, token), fill=TRUE)
      sampAll <- data.table::rbindlist(list(sampAll, sampNewRel), fill=TRUE)
      sampAll <- data.frame(sampAll)
      
      # remove duplicates
      sampAll <- sampAll[!duplicated(sampAll),]
      
      # remove sample rows that don't include parents, and aren't root samples
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
      if(length(ind)>0) {
        sampAll <- sampAll[-ind,]
      }
      
      # mark the focal sample's records complete
      sampAll$done[which(sampAll$sampleUuid %in% sampNew$sampleUuid)] <- rep("done",
                                            length(which(sampAll$sampleUuid %in% sampNew$sampleUuid)))
      
      # remove sample rows that are identical except only one is marked "done"
      ind.d <- numeric()
      for(i in sort(union(which(duplicated(sampAll$sampleUuid)), 
                          which(duplicated(sampAll$sampleUuid, fromLast=TRUE))))) {
        if(!is.na(sampAll$done[i])) {
          next
        }
        di <- sampAll$sampleUuid[i]
        if(is.na(sampAll$done[i])) {
          focSamp <- paste(sampAll[i,colnames(sampAll)[which(!colnames(sampAll) %in% "done")]], 
                           collapse=".")
          dupSamp <- apply(sampAll[which(sampAll$sampleUuid==di),
                                   colnames(sampAll)[which(!colnames(sampAll) %in% "done")]],
                           1, paste, collapse=".")
          if(length(which(focSamp==dupSamp))>1) {
            ind.d <- c(ind.d, i)
          }
        }
      }
      if(length(ind.d)>0) {
        sampAll <- sampAll[-ind.d,]
      }
      
      # remove duplicates again
      sampAll <- sampAll[!duplicated(sampAll),]
    }
    
    # attempt to order (would be great to improve. this at least puts root samples at the top)
    # also re-orders columns
    sampAll <- sampAll[order(sampAll$parentSampleClass, na.last=FALSE),]
    sampAll <- sampAll[,c("sampleUuid", "sampleTag", "barcode", "archiveGuid", 
                          "sampleClass", "parentSampleUuid", "parentSampleTag", 
                          "parentSampleBarcode", "parentSampleArchiveGuid", 
                          "parentSampleClass")]
    
    return(sampAll)
    
  }
  
}
