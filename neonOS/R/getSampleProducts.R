##############################################################################################
#' @title Get the full list of data products a given sample has appeared in.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Get the full list of data products containing data related to a given sample..
#'
#' @param sampleNode A NEON sample identifier. [character]
#' @param idType Is sampleNode a tag, barcode, or guid? Defaults to tag. [character]
#' @param sampleClass The NEON sampleClass of sampleNode. Required if sampleNode is a tag and there are multiple valid classes. [character]
#' @param token User specific API token (generated within data.neonscience.org user accounts). [character]

#' @return A data frame listing data products where the sample in question appears. Data frame contains the following columns: data product ID, data product name, table name, field name of the sample's ID field, and sample node (the input value).
#' 
#' @examples	
#' # Find data for a tagged plant.
#' \dontrun{
#' plantsamp <- getSampleProducts(sampleNode="B00000123538", 
#'                                idType="tag", sampleClass="taggedPlant",
#'                                token=token)
#' }

#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire K. Lunch (2026-11-25): Original creation

##############################################################################################

getSampleProducts <- function(sampleNode, 
                              idType="tag", 
                              sampleClass=NA_character_, 
                              token=NA_character_) {
  
  # if using tag, first check classes
  if(idType=="tag" & is.na(sampleClass)) {
    req <- getAPI(paste(nu.globals$baseurl, "samples/classes?sampleTag=", 
                        utils::URLencode(sampleNode, reserved=TRUE), sep=""), 
                  token=token)
    
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
    req <- getAPI(paste(nu.globals$baseurl, "samples/view?sampleTag=", 
                        utils::URLencode(sampleNode, reserved=TRUE), 
                        "&sampleClass=", sampleClass, sep=""), 
                  token=token)
  } else {
    if(idType=="barcode") {
      req <- getAPI(paste(nu.globals$baseurl, "samples/view?barcode=", 
                          sampleNode, sep=""), token=token)
    } else {
      if(idType=="guid") {
        req <- getAPI(paste(nu.globals$baseurl, "samples/view?archiveGuid=", 
                            sampleNode, sep=""), token=token)
      } else {
        if(idType=="uuid") {
          req <- getAPI(paste(nu.globals$baseurl, "samples/view?sampleUuid=", 
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
    
    sampFoc <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), 
                                  simplifyVector=TRUE, flatten=TRUE)
    sampTables <- sampFoc$data$sampleViews$sampleEvents[[1]]$ingestTableName
    sampTables <- unique(sampTables)
    
    tabrel <- utils::read.csv("https://raw.githubusercontent.com/NEONScience/NEON-OS-data-processing/refs/heads/main/helper_files/table_relationships.csv")
    
    out <- character()
    for(i in 1:length(sampTables)) {
      tabi <- sampTables[i]
      pubi <- tabrel[which(tabrel$ingestTableName==tabi),]
      if(nrow(pubi)==0) {
        next
      }
      outi <- unique(pubi[,c("dataProductId", "dataProductName", "tableName")])
      for(j in 1:nrow(outi)) {
        pubj <- pubi[which(pubi$dataProductId==outi$dataProductId[j] & 
                             pubi$tableName==outi$tableName[j]),]
        fieldj <- pubj$fieldName[grep(sampFoc$data$sampleViews$sampleClass,
                                      pubj$ingestSourceTypeParameter)]
        if(length(fieldj)==0) {
          next
        }
        fieldj <- grep("ID", fieldj, value=TRUE)
        outj <- cbind(outi[j,], fieldj, sampleNode)
        out <- rbind(out, outj)
      }
    }
    
    out <- data.frame(out)
    names(out) <- c("DPID", "DPName", "tableName", "idFieldName", "sampleNode")
    out$tableName <- gsub("[_]pub", "", out$tableName)
    
    return(out)
    
  }
  
}
