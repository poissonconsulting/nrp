# Download Phytoplankton data table from database

Download Phytoplankton data table from database

## Usage

``` r
nrp_download_phyto(
  start_date = NULL,
  end_date = NULL,
  sites = NULL,
  species = "all",
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

- species:

  A character vector of the species to include. Defaults to 'all'
  Permissible values can be found in the PhytoplanktonSpecies table.

- db_path:

  The SQLite connection object or path to the SQLite database

## Value

Phytoplankton data table
