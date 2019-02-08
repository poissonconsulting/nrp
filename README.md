
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![BCDevExchange
Status](https://assets.bcdevexchange.org/images/badges/exploration.svg)](https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md)
[![Travis build
status](https://travis-ci.com/poissonconsulting/nrp.svg?branch=master)](https://travis-ci.com/poissonconsulting/nrp)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/poissonconsulting/nrp?branch=master&svg=true)](https://ci.appveyor.com/project/poissonconsulting/nrp)

# nrp

`nrp` is an R package that is being developed for the Nutrient
Restoration Program (NRP).

It provides functions to

  - read the various data files used by the program
  - upload data to the NRP SQLite database
  - produce key tables and plots based on the data in the NRP SQLite
    database

## Installation

To install the latest development version from
[GitHub](https://github.com/poissonconsulting/nrp)

    # install.packages("devtools")
    devtools::install_github("poissonconsulting/dts")
    devtools::install_github("poissonconsulting/dttr")
    devtools::install_github("poissonconsulting/readwritesqlite")
    devtools::install_github("poissonconsulting/nrp")
