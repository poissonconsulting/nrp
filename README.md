
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
Restoration Program (NRP). It currently provides the following
functionality for CTD data:

**Read in raw data files**<br /> The package checks the validity of
input data, and references the the CTD site table in the database to
ensure the correct SiteID is assigned for each file. the date and units
are extracted from the file metadata, and any missing units are filled
in.

**Upload data to the NRP SQLite database**<br /> When uploading new data
to the database, the package checks to ensure that no duplicate data is
present, that values fall within acceptable ranges, and that units are
correct.

**Download CTD data**<br /> CTD data can be downloaded from the database
with optional filtering by SiteID, date range, and parameter name. The
CTD site table can also be downloaded from the database.

## Installation

To install the latest development version from
[GitHub](https://github.com/poissonconsulting/nrp)

    # install.packages("drat")
    drat::addRepo("poissonconsulting")
    devtools::install_github("poissonconsulting/nrp")

## Demonstration

For simplicity, this demo uses an empty, temporary database to read and
write to, and uses some test data that travels with the package.

``` r
library(nrp)

# create empty database with automatically populated Site and Lake tables
# path = ":memory:" creates a temporory, in memory database
conn <- nrp_create_db(path = ":memory:", ask = FALSE) 

# provide a path to a .cnv file
path <-  system.file("extdata", "ctd/2018/KL1_27Aug2018008downcast.cnv",
                       package = "nrp", mustWork = TRUE)

# read in file
# function looks at the database site table to assign SiteID for each file
data <- nrp_read_ctd_file(path, db_path = conn)
#> 132 out of 1445 duplicate depth readings removed from file /Library/Frameworks/R.framework/Versions/3.5/Resources/library/nrp/extdata/ctd/2018/KL1_27Aug2018008downcast.cnv

data
#> # A tibble: 1,313 x 15
#>    SiteID Date       Time  Depth Temperature Oxygen Oxygen2 Conductivity
#>    <chr>  <date>     <tim> <S3:> <S3: units> <S3: > <S3: u> <S3: units> 
#>  1 KL1    2018-08-27 18:05 -0.6… 15.6633 [°… 9.573… 96.299… 2.698 [uS/c…
#>  2 KL1    2018-08-27 18:05 -0.6… 15.6636 [°… 9.573… 96.303… 2.733 [uS/c…
#>  3 KL1    2018-08-27 18:05 -0.6… 15.6642 [°… 9.575… 96.324… 2.733 [uS/c…
#>  4 KL1    2018-08-27 18:05 -0.6… 15.6658 [°… 9.574… 96.316… 2.733 [uS/c…
#>  5 KL1    2018-08-27 18:05 -0.6… 15.6669 [°… 9.575… 96.325… 2.663 [uS/c…
#>  6 KL1    2018-08-27 18:05 -0.6… 15.6668 [°… 9.575… 96.334… 2.838 [uS/c…
#>  7 KL1    2018-08-27 18:05 -0.6… 15.6670 [°… 9.572… 96.295… 3.260 [uS/c…
#>  8 KL1    2018-08-27 18:05 -0.6… 15.6700 [°… 9.574… 96.324… 5.015 [uS/c…
#>  9 KL1    2018-08-27 18:05 -0.6… 15.6798 [°… 9.571… 96.320… 4.909 [uS/c…
#> 10 KL1    2018-08-27 18:05 -0.6… 15.6880 [°… 9.569… 96.313… 5.013 [uS/c…
#> # … with 1,303 more rows, and 7 more variables: Conductivity2 <S3: units>,
#> #   Salinity <S3: units>, Backscatter <S3: units>, Fluorescence <S3:
#> #   units>, Frequency <S3: units>, Flag <dbl>, Pressure <S3: units>

# upload data to the database
nrp_upload_ctd(data = data, db_path = conn)

# download data from database, filtering by Date and SiteID
db_data <- nrp_download_ctd(start_date = "2018-08-27",
                            end_date = "2018-08-27",
                            sites = "KL1",
                            db_path = conn)
db_data
#> # A tibble: 1,313 x 15
#>    SiteID Date       Time   Depth Temperature Oxygen Oxygen2 Conductivity
#>    <chr>  <date>     <tim>  <dbl>       <dbl>  <dbl>   <dbl>        <dbl>
#>  1 KL1    2018-08-27 18:05 -0.667        15.7   9.57    96.3         4.91
#>  2 KL1    2018-08-27 18:05 -0.665        15.7   9.57    96.3         5.01
#>  3 KL1    2018-08-27 18:05 -0.662        16.2   9.46    96.3         9.39
#>  4 KL1    2018-08-27 18:05 -0.661        15.7   9.57    96.3         5.01
#>  5 KL1    2018-08-27 18:05 -0.66         16.6   9.38    96.4         6.99
#>  6 KL1    2018-08-27 18:05 -0.659        16.5   9.40    96.3         7.80
#>  7 KL1    2018-08-27 18:05 -0.658        15.7   9.57    96.3         3.26
#>  8 KL1    2018-08-27 18:05 -0.656        15.7   9.57    96.3         2.70
#>  9 KL1    2018-08-27 18:05 -0.653        15.7   9.58    96.3         2.84
#> 10 KL1    2018-08-27 18:05 -0.652        15.8   9.55    96.3        33.7 
#> # … with 1,303 more rows, and 7 more variables: Conductivity2 <dbl>,
#> #   Salinity <dbl>, Backscatter <dbl>, Fluorescence <dbl>,
#> #   Frequency <dbl>, Flag <dbl>, Pressure <dbl>
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
