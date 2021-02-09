
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R build
status](https://github.com/poissonconsulting/nrp/workflows/R-CMD-check/badge.svg)](https://github.com/poissonconsulting/nrp/actions)
[![Codecov test
coverage](https://codecov.io/gh/poissonconsulting/nrp/branch/master/graph/badge.svg)](https://codecov.io/gh/poissonconsulting/nrp?branch=master)
[![Apache
license](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
<!-- badges: end -->

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
encountered. EMS metals and EMS standard are stored in separate tables
in the database.

**Upload data to the NRP SQLite database**<br /> When uploading new data
to the database, the package checks to ensure that no duplicate data is
present, and numerous checks are performed to ensure that the data’s
structure is correct and compatible with the database.

**Download CTD data**<br /> Data can then be downloaded from the
database with optional filtering by SiteID, date range, etc.

## Installation

To install the latest version from
[GitHub](https://github.com/poissonconsulting/nrp)

``` r
# install.packages("remotes")
remotes::install_github("poissonconsulting/nrp")
```

## Demonstration

For simplicity, this demonstration uses the function `nrp_create_db` to
create an empty, temporary database to read and write to, and uses some
test data that travels with the package. When connecting to a proper
database, just provide the file path to the database for the argument
`db_path` in all function calls and the connection will be made
automatically. Alternatively you can connect to a database in R and then
supply that connection object for the argument `db_path`.

``` r
library(nrp)

# create empty database
# path = ":memory:" creates a temporary, in memory database
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
```

When updating EMS data, the user must supply their own data set which
can be easily obtained using a function from the `rems` package:
`get_ems_data(which = "4yr")`. The four year data set must be used as
the two year data has different structure that does not contain all the
parameters needed for use with nrp functions. Once you have the four
year data set loaded in the environment, you can easily upload it to the
database with nrp functions. You don’t have to worry about uploading
duplicate data as the nrp function for uploading will automatically trim
the input data so that only new data is added to the database. For this
demonstration we use a light weight sample of raw EMS data that travels
with the package.

``` r
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
```

There are additional functions in the nrp package that allow for easy
downloading of other tables in the database.

``` r
# Lakes and BasinArm tables
lakes <- nrp_download_lakes(db_path = conn)
basinArm <- nrp_download_ctd_basin_arm(db_path = conn)

# site table
sites <- nrp_download_sites(db_path = conn)

# ctd visit table
ctd_visit <- nrp_download_ctd_visit(db_path = conn)

# Tip: the nrp functions can also use global options to automatically know
# what database to connect to. Once you've set the global option "nrp.db_path"
# for the database you want to connect to, you don't have to provide
# the db_path argument any more within that R session

options(nrp.db_path = conn) # "conn" can also be a file path to a database

# now we can do things like:
lakes <- nrp_download_lakes()


# we conclude the demo by closing the database connection
# If you are providing a file path to the database, you do not need to do this
readwritesqlite::rws_disconnect(conn)
```

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/poissonconsulting/nrp/issues/).

## How to Contribute

If you would like to contribute to the package, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

## Code of Conduct

Please note that the nrp project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## License

The code is released under the Apache License 2.0

    Copyright 2019 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
