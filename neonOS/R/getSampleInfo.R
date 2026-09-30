##############################################################################################
#' @title Get the essential identifiers for a given NEON sample.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Get the basic information (uuid, tag, barcode, guid, and class) for a given NEON sample.
#'
#' @param sampleNode A NEON sample identifier. [character]
#' @param idType Is sampleNode a tag, barcode, or guid? Defaults to tag. [character]
#' @param sampleClass The NEON sampleClass of sampleNode. Required if sampleNode is a tag and there are multiple valid classes. [character]
#' @param token User specific API token (generated within data.neonscience.org user accounts). [character]

#' @return A data frame of sample identifiers.
#' 
#' @examples	
#' # Find identifiers for a soil nitrogen transformation sample
#' \dontrun{
#' soil_samp_ids <- getSampleInfo(sampleNode="B00000123538", idType="barcode")
#' }

#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2026-09-30): adapted from getSampleTree()

##############################################################################################

getSampleInfo <- function(sampleNode, idType="tag", 
                          sampleClass=NA_character_, token=NA_character_) {
  
  # if using tag, first check classes
  if(idType=="tag" & is.na(sampleClass)) {
    req <- getAPI(paste("https://data.neonscience.org/api/v0/samples/classes?sampleTag=", 
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
    req <- getAPI(paste("https://data.neonscience.org/api/v0/samples/view?sampleTag=", 
                        sampleNode, "&sampleClass=", sampleClass, sep=""), token=token)
  } else {
    if(idType=="barcode") {
      req <- getAPI(paste("https://data.neonscience.org/api/v0/samples/view?barcode=", 
                          sampleNode, sep=""), token=token)
    } else {
      if(idType=="guid") {
        req <- getAPI(paste("https://data.neonscience.org/api/v0/samples/view?archiveGuid=", 
                            sampleNode, sep=""), token=token)
      } else {
        if(idType=="uuid") {
          req <- getAPI(paste("https://data.neonscience.org/api/v0/samples/view?sampleUuid=", 
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
    sampInfo <- sampFoc$data$sampleViews[c("sampleUuid",
                                             "sampleTag",
                                             "barcode",
                                             "archiveGuid",
                                             "sampleClass")]
    
    return(sampInfo)
    
  }
  
}
