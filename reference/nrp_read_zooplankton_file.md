# Read zooplankton raw data file

Read zooplankton raw data file

## Usage

``` r
nrp_read_zooplankton_file(
  path,
  db_path = getOption("nrp.db_path", file.choose()),
  system = NULL
)
```

## Arguments

- path:

  A string of the path to the file.

- db_path:

  The SQLite connection object or path to the nrp SQLite database.

- system:

  The system 'arrow' or 'kootenay'. If null, the system is detected from
  the file name.

## Value

A tibble
