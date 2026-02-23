# Download CTD data table from database

Download CTD data table from database

## Usage

``` r
nrp_download_ctd(
  start_date = NULL,
  end_date = NULL,
  sites = NULL,
  parameters = "all",
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

  A character vector of the parameters to include. Permissable values:
  "Temperature", "Oxygen", "Oxygen2", "Conductivity", "Conductivity2",
  "Salinity", "Backscatter", "Fluorescence", "Frequency", "Flag",
  "Pressure"

- db_path:

  The SQLite connection object or path to the SQLite database

## Value

CTD data table
