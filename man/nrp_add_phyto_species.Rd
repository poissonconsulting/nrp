% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phyto.R
\name{nrp_add_phyto_species}
\alias{nrp_add_phyto_species}
\title{Add new phytoplankton species to the 'PhytoplanktonSpecies' table in the database}
\usage{
nrp_add_phyto_species(data, db_path = getOption("nrp.db_path", file.choose()))
}
\arguments{
\item{data}{a tibble or data frame of new species data.
Must have columns "Taxa", "Genus", "ClassName", "ClassAlias". "ClassAlias" can be NA.}

\item{db_path}{The SQLite connection object or path to the SQLite database}
}
\description{
Add new phytoplankton species to the 'PhytoplanktonSpecies' table in the database
}
