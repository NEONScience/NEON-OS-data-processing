#' Per-point data table from Breeding landbird point counts (DP1.10003.001)
#'
#' An example set of NEON observational data. Contains the point metadata associated with bird observations from Niwot Ridge (NIWO) in 2019, as published in RELEASE-2021.
#'
#' @format A data frame with 54 rows and 31 columns
#' \describe{
#'   \item{uid}{Unique ID within NEON database; an identifier for the record}
#'   \item{namedLocation}{Name of the measurement location in the NEON database}
#'   \item{domainID}{Unique identifier of the NEON domain}
#'   \item{siteID}{NEON site code}
#'   \item{plotID}{Plot identifier (NEON site code_XXX)}
#'   \item{plotType}{NEON plot type in which sampling occurred: tower, distributed or gradient}
#'   \item{pointID}{Identifier for a point location}
#'   \item{nlcdClass}{National Land Cover Database Vegetation Type Name}
#'   \item{decimalLatitude}{The geographic latitude (in decimal degrees, WGS84) of the geographic center of the reference area}
#'   \item{decimalLongitude}{The geographic longitude (in decimal degrees, WGS84) of the geographic center of the reference area}
#'   \item{geodeticDatum}{Model used to measure horizontal position on the earth}
#'   \item{coordinateUncertainty}{The horizontal distance (in meters) from the given decimalLatitude and decimalLongitude describing the smallest circle containing the whole of the Location. Zero is not a valid value for this term}
#'   \item{elevation}{Elevation (in meters) above sea level}
#'   \item{elevationUncertainty}{Uncertainty in elevation values (in meters)}
#'   \item{startDate}{The start date-time or interval during which an event occurred}
#'   \item{samplingImpracticalRemarks}{Technician notes; free text comments accompanying the sampling impractical record}
#'   \item{samplingImpractical}{Samples and/or measurements were not collected due to the indicated circumstance}
#'   \item{eventID}{An identifier for the set of information associated with the event, which includes information about the place and time of the event}
#'   \item{startCloudCoverPercentage}{Observer estimate of percent cloud cover at start of sampling}
#'   \item{endCloudCoverPercentage}{Observer estimate of percent cloud cover at end of sampling}
#'   \item{startRH}{Relative humidity as measured by handheld weather meter at the start of sampling}
#'   \item{endRH}{Relative humidity as measured by handheld weather meter at the end of sampling}
#'   \item{observedHabitat}{Observer assessment of dominant habitat at the sampling point at sampling time}
#'   \item{observedAirTemp}{The air temperature measured with a handheld weather meter}
#'   \item{kmPerHourObservedWindSpeed}{The average wind speed measured with a handheld weather meter, in kilometers per hour}
#'   \item{laboratoryName}{Name of the laboratory or facility that is processing the sample}
#'   \item{samplingProtocolVersion}{The NEON document number and version where detailed information regarding the sampling method used is available; format NEON.DOC.######vX}
#'   \item{remarks}{Technician notes; free text comments accompanying the record}
#'   \item{measuredBy}{An identifier for the technician who measured or collected the data}
#'   \item{publicationDate}{Date of data publication on the NEON data portal}
#'   \item{release}{Identifier for data release}
#' }
#' @source \url{https://data.neonscience.org/api/v0/products/DP1.10003.001}
"brd_perpoint"


#' Count data table from Breeding landbird point counts (DP1.10003.001)
#'
#' An example set of NEON observational data. Contains the individual bird observations from Niwot Ridge (NIWO) in 2019, as published in RELEASE-2021.
#'
#' @format A data frame with 472 rows and 26 columns
#' \describe{
#'   \item{uid}{Unique ID within NEON database; an identifier for the record}
#'   \item{namedLocation}{Name of the measurement location in the NEON database}
#'   \item{domainID}{Unique identifier of the NEON domain}
#'   \item{siteID}{NEON site code}
#'   \item{plotID}{Plot identifier (NEON site code_XXX)}
#'   \item{plotType}{NEON plot type in which sampling occurred: tower, distributed or gradient}
#'   \item{pointID}{Identifier for a point location}
#'   \item{startDate}{The start date-time or interval during which an event occurred}
#'   \item{eventID}{An identifier for the set of information associated with the event, which includes information about the place and time of the event}
#'   \item{pointCountMinute}{The minute of sampling within the point count period}
#'   \item{targetTaxaPresent}{Indicator of whether the sample contained individuals of the target taxa}
#'   \item{taxonID}{Species code, based on one or more sources}
#'   \item{scientificName}{Scientific name, associated with the taxonID. This is the name of the lowest level taxonomic rank that can be determined}
#'   \item{taxonRank}{The lowest level taxonomic rank that can be determined for the individual or specimen}
#'   \item{vernacularName}{A common or vernacular name}
#'   \item{family}{The scientific name of the family in which the taxon is classified}
#'   \item{nativeStatusCode}{The process by which the taxon became established in the location}
#'   \item{observerDistance}{Radial distance between the observer and the individual(s) being observed}
#'   \item{detectionMethod}{How the individual(s) was (were) first detected by the observer}
#'   \item{visualConfirmation}{Whether the individual(s) was (were) seen after the initial detection}
#'   \item{sexOrAge}{Sex of individual if detectable, age of individual if individual can not be sexed}
#'   \item{clusterSize}{Number of individuals in a cluster (a group of individuals of the same species)}
#'   \item{clusterCode}{Alphabetic code (A-Z) linked to clusters (groups of individuals of the same species) spanning multiple records}
#'   \item{identifiedBy}{An identifier for the technician who identified the specimen}
#'   \item{publicationDate}{Date of data publication on the NEON data portal}
#'   \item{release}{Identifier for data release}
#' }
#' @source \url{https://data.neonscience.org/api/v0/products/DP1.10003.001}
"brd_countdata"


#' Lignin data table from Plant foliar traits (DP1.10026.001)
#'
#' An example set of NEON observational data, containing duplicate records. NOT APPROPRIATE FOR ANALYTICAL USE. Contains foliar lignin data from Moab and Toolik in 2017, with artificial duplicates introduced to demonstrate the removeDups() function.
#'
#' @format A data frame with 26 rows and 25 columns
#' \describe{
#'   The variable names, descriptions, and units can be found in the cfc_lignin_variables table
#' }
#' @source \url{https://data.neonscience.org/api/v0/products/DP1.10026.001}
"cfc_lignin_test_dups"


#' Variables file, subset to lignin table, from Plant foliar traits (DP1.10026.001)
#'
#' The foliar lignin table's variables file from NEON observational data. Example to illustrate use of removeDups().
#'
#' @format A data frame with 26 rows and 8 columns
#' \describe{
#'   \item{table}{The table name of the NEON data table}
#'   \item{fieldName}{Field name within the table; corresponds to column names in cfc_lignin_test_dups}
#'   \item{description}{Description for each field name}
#'   \item{dataType}{Type of data for each field name}
#'   \item{units}{Units for each field name}
#'   \item{downloadPkg}{Is the field published in the basic or expanded data package?}
#'   \item{pubFormat}{Publication formatting, e.g. date format or rounding}
#'   \item{primaryKey}{Fields indicated by Y, when combined, should identify a unique record. Used by removeDups() to identify duplicate records.}
#' }
#' @source \url{https://data.neonscience.org/api/v0/products/DP1.10026.001}
"cfc_lignin_variables"
