
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![BCDevExchange
Status](https://assets.bcdevexchange.org/images/badges/exploration.svg)](https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md)
[![Travis Build
Status](https://www.travis-ci.com/poissonconsulting/nrp.svg?token=LCuTqqVUfUECxm1xTQLb&branch=master)](https://www.travis-ci.com/poissonconsulting/nrp)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/nf8qrbm7imvkuj0q?svg=true)](https://ci.appveyor.com/project/joethorley/nrp)
[![codecov](https://codecov.io/gh/poissonconsulting/nrp/branch/master/graph/badge.svg?token=BYPzzOPDrd)](https://codecov.io/gh/poissonconsulting/nrp)

# nrp

`nrp` is an R package that is being developed for the Nutrient
Restoration Program (NRP) to

  - read the various data files used by the program;
  - upload data to the NRP SQLite database; and
  - produce key tables and plots based on the data in the NRP SQLite
    database.

Currently we are working on functions to read CTD files.

## Installation

To install the latest development version from
[GitHub](https://github.com/poissonconsulting/nrp)

    # install.packages("drat")
    drat::addRepo("poissonconsulting")
    devtools::install_github("poissonconsulting/nrp")

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/poissonconsulting/nrp/issues/).

## How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms

## License

The code is released under the Apache License 2.0

    Copyright 2015 Province of British Columbia
    
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 
    
       http://www.apache.org/licenses/LICENSE-2.0
    
    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
