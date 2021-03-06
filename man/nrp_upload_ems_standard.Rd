% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ems.R
\name{nrp_upload_ems_standard}
\alias{nrp_upload_ems_standard}
\title{Upload standard EMS data to nrp database}
\usage{
nrp_upload_ems_standard(
  data,
  db_path = getOption("nrp.db_path", file.choose()),
  commit = TRUE,
  strict = TRUE,
  silent = TRUE,
  replace = FALSE
)
}
\arguments{
\item{data}{the object name of the data to be uploaded}

\item{db_path}{An Sqlite Database Connection, or path to an SQLite Database}

\item{commit}{A flag specifying whether to commit the operations
(calling with commit = FALSE can be useful for checking data).}

\item{strict}{A flag specifying whether to error if x has extraneous columns or if exists = TRUE extraneous data frames.}

\item{silent}{A flag specifying whether to suppress messages and warnings.}

\item{replace}{A flag specifying whether to replace any existing rows whose inclusion would violate unique or primary key constraints.}
}
\description{
Upload standard EMS data to nrp database
}
