% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mysid.R
\name{nrp_download_mysid}
\alias{nrp_download_mysid}
\title{Download Mysid data table from database}
\usage{
nrp_download_mysid(
  start_date = NULL,
  end_date = NULL,
  sites = NULL,
  parameters = "all",
  db_path = getOption("nrp.db_path", file.choose())
)
}
\arguments{
\item{start_date}{The start date}

\item{end_date}{The end date}

\item{sites}{A character vector of the Site IDs}

\item{parameters}{A character vector of the parameters to include.
Permissible values can be found in the nrp::mysid_params}

\item{db_path}{The SQLite connection object or path to the SQLite database}
}
\value{
CTD data table
}
\description{
Download Mysid data table from database
}
