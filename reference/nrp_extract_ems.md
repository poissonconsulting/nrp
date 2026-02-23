# Extract and clean EMS data

Extract and clean EMS data

## Usage

``` r
nrp_extract_ems(
  data,
  db_path = getOption("nrp.db_path", file.choose()),
  analysis_type = "standard"
)
```

## Arguments

- data:

  The EMS data to be extracted. Data must first be downloaded from
  server by using rems package

- db_path:

  The SQLite connection object or path to the nrp SQLite database

- analysis_type:

  EMS data of interest. Must be either "standard" or "metals"

## Value

A tibble.
