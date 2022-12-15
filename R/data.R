#' A site table for Sea-bird CTD data
#'
#' @format A tbl data frame:
#' \describe{
#'   \item{SiteID}{Site Identifyer}
#'   \item{SiteNumber}{Site Number}
#'   \item{SiteName}{Common Name or description the Site}
#'   \item{BasinArm}{Which arm or Basin it is located in}
#'   \item{Depth}{The depth of the Seabird}
#'   \item{geometry}{Coordinates for the location of the site}
#' }
"ctdSites"

#' A Basin-Arm table for Sea-bird CTD data
#'
#' @format A tbl data frame:
#' \describe{
#'   \item{Lake}{The name of the lake}
#'   \item{BasinArm}{What arms or basins are within each lake}
#' }
"basinArm"

#' A lakes table for Sea-bird CTD data
#'
#' @format A tbl data frame:
#' \describe{
#'   \item{Lake}{The name of the lake}
#'   \item{Area}{the area of te lake in meters squared}
#'   \item{geometry}{Spatial polygon data}
#' }
"lakes"

#' A lookup table for matching historical file names with Site IDs and Dates
#'
#' @format A tbl data frame:
#' \describe{
#'   \item{File}{File name}
#'   \item{Date}{Date data was collected}
#'   \item{SiteID}{Site ID}
#' }
"site_date_lookup"

#' A lookup table for for selecting the desired columns from EMS
#'
#' @format A tbl data frame:
#' \describe{
#'   \item{PARAMETER_CODE}{PARAMETER_CODE}
#'   \item{PARAMETER}{PARAMETER}
#'   \item{ANALYTICAL_METHOD}{ANALYTICAL_METHOD}
#'   \item{chem.make.names}{chem.make.names}
#'   \item{chem.names}{chem.names}
#'   \item{Comment}{Comment}
#' }
"ems_param_lookup"

#' A site table for EMS data
#'
#' @format A tbl data frame:
#' \describe{
#'   \item{SiteID}{Site ID}
#'   \item{EmsSite}{EMS database site name}
#'   \item{SiteName}{Site name}
#' }
"emsSites"

#' An initialized table for EMS standard data
#'
#' @format A tbl data frame:
"ems_standard_init"

#' An initialized table for EMS metals data
#'
#' @format A tbl data frame:
"ems_metals_init"

#' A named character vector with the correct column names, data types,  and order for raw zooplankton data
#'
#' @format A character vector:
"zoo_input_cols"

#' A named character vector with the correct column names, data types, and order for raw mysid data
#'
#' @format A character vector:
"mysid_input_cols"

#' A named character vector with the correct column names, data types, and order for raw phytoplankton data
#'
#' @format A character vector:
"phyto_input_cols"

#' A character vector of all parameters for the zooplankton data in the database
#'
#' @format A character vector:
"zoo_params"

#' A character vector of all parameters for the mysid data in the database
#'
#' @format A character vector
"mysid_params"

#' A reference table for phytoplankton species
#'
#' @format A tbl data frame:
#' \describe{
#'   \item{Taxa}{The species or sub-species}
#'   \item{Genus}{The genus}
#'   \item{ClassName}{The formal class name(s)}
#'   \item{ClassAlias}{The informal class name(s)}
#' }
"phyto_species"
