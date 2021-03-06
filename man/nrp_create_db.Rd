% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-db.R
\name{nrp_create_db}
\alias{nrp_create_db}
\title{Create NRP SQLite Database}
\usage{
nrp_create_db(path, ask = getOption("nrp.ask", TRUE))
}
\arguments{
\item{path}{A string of the path to the database to create.
It must end with extension '.sqlite'.}

\item{ask}{A flag specifying whether to ask before deleting an existing file.}
}
\value{
The path to the newly created database.
}
\description{
Create NRP SQLite Database
}
\examples{
\dontrun{
nrp_create_db("new_database.sqlite", TRUE)
}
}
