# Read CTD File

Read CTD File

## Usage

``` r
nrp_read_ctd_file(
  path,
  db_path = getOption("nrp.db_path", file.choose()),
  lookup = nrp::site_date_lookup
)
```

## Arguments

- path:

  A string of the path to the file.

- db_path:

  The SQLite connection object or path to the nrp SQLite database

- lookup:

  The lookup table for assigning site names/dates (used when reading
  files that cannot be read by oce package). this defaults to a dataset
  provided with the package that is used for reading historical data

## Value

A tibble
