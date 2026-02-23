# Read phytoplankton raw data file

Read phytoplankton raw data file

## Usage

``` r
nrp_read_phyto_file(path, db_path = getOption("nrp.db_path", file.choose()))
```

## Arguments

- path:

  A string of the path to the file.

- db_path:

  The SQLite connection object or path to the nrp SQLite database.

## Value

A tibble
