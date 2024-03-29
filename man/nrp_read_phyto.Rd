% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phyto.R
\name{nrp_read_phyto}
\alias{nrp_read_phyto}
\title{Read phytoplankton raw data files}
\usage{
nrp_read_phyto(
  path = ".",
  db_path = getOption("nrp.db_path", file.choose()),
  recursive = FALSE,
  regexp = "[.]xlsx$",
  fail = TRUE
)
}
\arguments{
\item{path}{A string of the path to the directory.}

\item{db_path}{The SQLite connection object or path to the nrp SQLite database.}

\item{recursive}{(Deprecated) If \code{TRUE} recurse fully.}

\item{regexp}{A regular expression (e.g. \verb{[.]csv$}) passed on to \code{\link[=grep]{grep()}} to filter paths.}

\item{fail}{Should the call fail (the default) or warn if a file cannot be
accessed.}
}
\value{
A tibble.
}
\description{
Read phytoplankton raw data files
}
