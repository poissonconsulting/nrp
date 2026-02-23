# Create NRP SQLite Database

Create NRP SQLite Database

## Usage

``` r
nrp_create_db(path, ask = getOption("nrp.ask", TRUE))
```

## Arguments

- path:

  A string of the path to the database to create. It must end with
  extension '.sqlite'.

- ask:

  A flag specifying whether to ask before deleting an existing file.

## Value

The path to the newly created database.

## Examples

``` r
if (FALSE) { # \dontrun{
nrp_create_db("new_database.sqlite", TRUE)
} # }
```
