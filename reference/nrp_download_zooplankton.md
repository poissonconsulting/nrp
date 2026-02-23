# Download Zooplankton data table from database

Download Zooplankton data table from database

## Usage

``` r
nrp_download_zooplankton(
  start_date = NULL,
  end_date = NULL,
  sites = NULL,
  parameters = "all",
  counts = FALSE,
  db_path = getOption("nrp.db_path", file.choose())
)
```

## Arguments

- start_date:

  The start date

- end_date:

  The end date

- sites:

  A character vector of the Site IDs

- parameters:

  A character vector of the parameters to include. Permissable values
  can be found in the nrp::zoo_params

- counts:

  A flag indicating whether to return the raw counts instead.

- db_path:

  The SQLite connection object or path to the SQLite database

## Value

CTD data table
