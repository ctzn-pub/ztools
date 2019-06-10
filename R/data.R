library(rio)
library(usethis)

#From https://unstats.un.org/unsd/methodology/m49/overview/
#UNSD<- import("/Users/umahuggins/Dropbox/ON/Projects/World/Meta/data/UNSD — Methodology.xlsx")
#use_data(UNSD, overwrite = TRUE)


#' UNSD Data
#'
#' The list of countries or areas contains the names of countries or areas in alphabetical order, their three-digit numerical codes used for statistical processing purposes by the Statistics Division of the United Nations Secretariat, and their three-digit alphabetical codes assigned by the International Organization for Standardization (ISO).1 In general, this list of countries or areas includes those countries or areas for which statistical data are compiled by the Statistics Division of the United Nations Secretariat.

#'The names of countries or areas refer to their short form used in day-to-day operations of the United Nations and not necessarily to their official name as used in formal documents. These names are based on the United Nations Terminology Database (UNTERM).2 The designations employed and the presentation of material at this site do not imply the expression of any opinion whatsoever on the part of the Secretariat of the United Nations concerning the legal status of any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries.

#' @format A data frame with 249 rows and 15 variables:
#' \describe{
#'   \item{Sub-region Name}{Sub-continental name (eg, Southern Europe)}
#'   \item{Region Name}{Continent}
#'   ...
#' }
#' @source \url{https://unstats.un.org/unsd/methodology/m49/overview/}
"UNSD"


#countrydata<- import("/Users/umahuggins/Dropbox/ON/Projects/World/Meta/data/country_meta_use.rds")
#use_data(countrydata, overwrite = TRUE)


#' Our Country Metadata
#' Our personal country-metdata
#' @format A data frame with 261 rows and 74 variables:
#' \describe{
#'   \item{Region Name}{Coming soon}
#'   ...
#' }
"countrydata"

#globalMapData<- import("/Users/umahuggins/Dropbox/ON/Projects/World/visualization/globalMapData.rds")
#use_data(globalMapData, overwrite = TRUE)


#mapDat <- download_map_data("custom/world-highres")
#export(mapDat, "./../globalMapData.rds")


#' Global Map List Object
#' Downloaded highcharts map-data objects
#' @format A list
#' \describe{
#'   \item{Global Map Data}{Coming soon}
#'   ...
#' }
"globalMapData"



#library(highcharter)
#euroMapData <- download_map_data("custom/europe")
#use_data(euroMapData, overwrite = TRUE)


#' European Map List Object
#' Downloaded highcharts map-data objects
#' @format A list
#' \describe{
#'   \item{Europe Map Data}{Coming soon}
#'   ...
#' }
"globalMapData"
