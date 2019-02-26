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
"sites"

#' A lake table for Sea-bird CTD data
#'
#' @format A tbl data frame:
#' \describe{
#'   \item{Lake}{The name of the lake}
#'   \item{BasinArm}{What arms or basins are within each lake}
#' }
"lakes"

#' A lookup table fr matching historical file names with Site IDs and Dates
#'
#' @format A tbl data frame:
#' \describe{
#'   \item{File}{File name}
#'   \item{Date}{Date data was collected}
#'   \item{SiteID}{Site ID}
#' }
"site_date_lookup"
