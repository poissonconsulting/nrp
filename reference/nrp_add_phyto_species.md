# Add new phytoplankton species to the 'PhytoplanktonSpecies' table in the database

Add new phytoplankton species to the 'PhytoplanktonSpecies' table in the
database

## Usage

``` r
nrp_add_phyto_species(data, db_path = getOption("nrp.db_path", file.choose()))
```

## Arguments

- data:

  a tibble or data frame of new species data. Must have columns "Taxa",
  "Genus", "ClassName", "ClassAlias". "ClassAlias" can be NA.

- db_path:

  The SQLite connection object or path to the SQLite database
