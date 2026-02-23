# Read phytoplankton raw data files

Read phytoplankton raw data files

## Usage

``` r
nrp_read_phyto(
  path = ".",
  db_path = getOption("nrp.db_path", file.choose()),
  recursive = FALSE,
  regexp = "[.]xlsx$",
  fail = TRUE
)
```

## Arguments

- path:

  A string of the path to the directory.

- db_path:

  The SQLite connection object or path to the nrp SQLite database.

- recursive:

  (Deprecated) If `TRUE` recurse fully.

- regexp:

  A regular expression (e.g. `[.]csv$`) passed on to
  [`grep()`](https://rdrr.io/r/base/grep.html) to filter paths.

- fail:

  Should the call fail (the default) or warn if a file cannot be
  accessed.

## Value

A tibble.
