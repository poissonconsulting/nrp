# Dowload EMS data table from database

Dowload EMS data table from database

## Usage

``` r
nrp_download_ems(
  db_path = getOption("nrp.db_path", file.choose()),
  start_date_time = NULL,
  end_date_time = NULL,
  sites = NULL,
  analysis_type = "standard",
  show_detection_limits = FALSE
)
```

## Arguments

- db_path:

  The SQLite connection object or path to the SQLite database

- start_date_time:

  The start date

- end_date_time:

  The end date

- sites:

  A character vector of the Site IDs

- analysis_type:

  EMS data of interest. Must be either "standard" or "metals"

- show_detection_limits:

  Whether to include detection limit columns for each parameter

## Value

EMS data table
