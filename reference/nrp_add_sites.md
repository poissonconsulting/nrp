# Add new sites to the 'Sites' table in the database.

Add new sites to the 'Sites' table in the database.

## Usage

``` r
nrp_add_sites(data, db_path = getOption("nrp.db_path", file.choose()))
```

## Arguments

- data:

  a tibble or data frame of new site data. Must have columns "SiteID",
  "SiteNumber", "SiteName", "BasinArm", "Depth", as well as columns
  "Easting" and "Northing" with coordinates in projection UTM zone 11N.

- db_path:

  The SQLite connection object or path to the SQLite database
