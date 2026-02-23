# Read CTD Files

Read CTD Files

## Usage

``` r
nrp_read_ctd(
  path = ".",
  db_path = getOption("nrp.db_path", file.choose()),
  recursive = FALSE,
  regexp = "[.]cnv$",
  fail = TRUE,
  lookup = nrp::site_date_lookup
)
```

## Arguments

- path:

  A string of the path to the directory.

- db_path:

  The SQLite connection object or path to the SQLite database

- recursive:

  (Deprecated) If `TRUE` recurse fully.

- regexp:

  A regular expression (e.g. `[.]csv$`) passed on to
  [`grep()`](https://rdrr.io/r/base/grep.html) to filter paths.

- fail:

  Should the call fail (the default) or warn if a file cannot be
  accessed.

- lookup:

  The lookup table for assigning site names/dates (used when reading
  files that cannot be read by oce package). this defaults to a dataset
  provided with the package that is used for reading historical data

## Value

A tibble.
