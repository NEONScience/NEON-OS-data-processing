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
      }
    }
  }

  req.content <- httr::content(req, as="parsed")
  if(!is.null(req.content$error$status)) {
    message(paste("Sample", sampleNode, "not found."))
    return()
  } else {
    
    samp <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), flatten=FALSE)
    
    # STOPPED HERE. child sample identifiers output format is not ideal.
    
    loc.children <- loc$data[base::grep('Child', base::names(loc$data))]$locationChildren
    loc.values <- getLocValues(loc, history)
    
    cat('Finding spatial data for', namedLocation, rep('', 50), '\r')
    utils::flush.console()
    
    if(length(loc.children)==0) {
      loc.all <- getLocValues(loc, history)
      return(loc.all)
    } else {
      loc.all <- plyr::rbind.fill(loc.values, 
                                  data.table::rbindlist(lapply(loc.children, getLocChildren, history), 
                                                        fill=T))
      return(loc.all)
    }
  }
  
}
