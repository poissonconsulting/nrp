
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis Build
Status](https://www.travis-ci.com/poissonconsulting/nrp.svg?token=LCuTqqVUfUECxm1xTQLb&branch=master)](https://www.travis-ci.com/poissonconsulting/nrp)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/nf8qrbm7imvkuj0q?svg=true)](https://ci.appveyor.com/project/joethorley/nrp)
[![codecov](https://codecov.io/gh/poissonconsulting/nrp/branch/master/graph/badge.svg?token=BYPzzOPDrd)](https://codecov.io/gh/poissonconsulting/nrp)

# nrp

`nrp` is an R package developed for the Nutrient Restoration Program
(NRP). It currently provides the following functionality for CTD and EMS
data:

**Read in raw data files**<br /> The package checks the validity of
input data and does some cleaning and manipulation to make the data more
informative and useful to the user.

For CTD data, the date and parameter units are extracted from the file
meta data. The SiteID is pulled from the raw data filename, and
validated by cross referencing it to known sites in the database.

For EMS data, the data is filtered to only include the parameters and
sites of interest for the Nutrient Restoration Program. The data is then
restructured in a ‘wide’ format that lends itself to easy
analysis/plotting. During the process, parameters are assigned the
correct units, and values that were measured as the detection limit are
set to 0. Detection limit columns for each parameter are created to
inform the user of the detection limit each time a 0 value is
encountered.

**Upload data to the NRP SQLite database**<br /> When uploading new data
to the database, the package checks to ensure that no duplicate data is
present, and numerous checks are performed to ensure that the data’s
structure is correct and compatible with the database.

**Download CTD data**<br /> Data can then be downloaded from the
database with optional filtering by SiteID, date range, etc.

## Installation

To install the latest development version from
[GitHub](https://github.com/poissonconsulting/nrp)

    # install.packages("drat")
    drat::addRepo("poissonconsulting")
    devtools::install_github("poissonconsulting/nrp")

## Demonstration

For simplicity, this demo uses an empty, temporary database to read and
write to, and uses some test data that travels with the package. When
connecting to a proper database, you don’t need to create a database
connection at the start, just provide the file path to the database for
the argument “db\_path” in all function calls.

``` r
library(nrp)

# CTD data

# create empty database
# path = ":memory:" creates a temporory, in memory database
conn <- nrp_create_db(path = ":memory:") 

# provide a path to a .cnv file
path <-  system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
                       package = "nrp", mustWork = TRUE)

# read in file
ctd_data <- nrp_read_ctd_file(path, db_path = conn)
#> 32 negative depths removed from data

# upload data to the database
nrp_upload_ctd(data = ctd_data, db_path = conn)
#> 131 duplicate depths removed from data

# download data from database, filtering by Date and SiteID and parameter
db_ctd_data <- nrp_download_ctd(start_date = "2018-08-27",
                            end_date = "2018-08-27",
                            sites = "KL1",
                            parameters = "all",
                            db_path = conn)


# EMS data

# read in raw ems data
path <-  system.file("extdata", "ems/test_ems.rds", package = "nrp", mustWork = TRUE)
ems_raw <- readRDS(path)

# extract into wide fromat that fits the nrp database, chooing either the
# 'metals' or 'standard' analysis_type
ems_data <- nrp_extract_ems(data = ems_raw, db_path = conn, analysis_type = "standard")

# upload data to database
nrp_upload_ems_standard(data = ems_data, db_path = conn)

# download data from database, filtering by Date, SiteID, and analysis_type. 
# You can also choose to exlude detection limit columns
db_ems_data_standard <- nrp_download_ems(start_date_time = "2018-07-03 13:48:00",
                                     end_date_time = "2018-07-31 13:28:00",
                                     sites = NULL,
                                     db_path = conn,
                                     analysis_type = "standard",
                                     show_detection_limits = TRUE)

# You can also use global options for connecting to the database. Once you've set
# the global option "nrp.db_path" for the database you want to connect to,
# you don't have to provide the db_path argument any more within that R session
options(nrp.db_path = conn) # This could also be a file path to a database

# Now we dont need to provide db_path:
db_ems_data_standard <- nrp_download_ems(start_date_time = "2018-07-03 13:48:00",
                                     end_date_time = "2018-07-31 13:28:00",
                                     sites = NULL,
                                     analysis_type = "standard",
                                     show_detection_limits = TRUE)

# close connection
readwritesqlite::rws_disconnect(conn)
```

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
