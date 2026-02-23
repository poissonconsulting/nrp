# Upload phyto data to the nrp database

Upload phyto data to the nrp database

## Usage

``` r
nrp_upload_phyto(
  data,
  db_path = getOption("nrp.db_path", file.choose()),
  commit = TRUE,
  strict = TRUE,
  silent = TRUE,
  replace = FALSE
)
```

## Arguments

- data:

  the object name of the data to be uploaded

- db_path:

  An SQLite Database Connection, or path to an SQLite Database

- commit:

  A flag specifying whether to commit the operations (calling with
  commit = FALSE can be useful for checking data).

- strict:

  A flag specifying whether to error if x has extraneous columns or if
  exists = TRUE extraneous data frames.

- silent:

  A flag specifying whether to suppress messages and warnings.

- replace:

  A flag specifying whether to replace any existing rows whose inclusion
  would violate unique or primary key constraints.
