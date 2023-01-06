#' Initialize tables for database
#'
#' @return initialized table with units
initialize_ctd <- function(){
  tibble(FileID = integer(),
         SiteID = character(),
         Date = as.Date(character()),
         Time = dttr2::dtt_time(character()),
         Depth = units::set_units(numeric(), "m"),
         Temperature = units::set_units(numeric(), "degC"),
         Oxygen = units::set_units(numeric(), "mg/l"),
         Oxygen2 = units::set_units(numeric(), "percent"),
         Conductivity = units::set_units(numeric(), "uS/cm"),
         Conductivity2 = units::set_units(numeric(), "mu * S/cm"),
         Salinity = units::set_units(numeric(), "PSU"),
         Backscatter = units::set_units(numeric(), "NTU"),
         Fluorescence = units::set_units(numeric(), "ug/L"),
         Frequency = units::set_units(numeric(), "Hz"),
         Flag = numeric(),
         Pressure = units::set_units(numeric(), "dbar"))
}

initialize_ctd_visit <- function(){
  tibble(SiteID = character(),
         Date = as.Date(character()),
         Time = dttr2::dtt_time(character()),
         DepthDuplicates = integer(),
         File = character())
}

initialize_zoo_sample <- function(){
  tibble(Date = as.Date(as.character()),
         SiteID = as.character(),
         Replicate = as.integer(),
         FileName = as.character(),
         MonthCat = as.character(),
         EndRev = as.integer(),
         StartRev = as.integer(),
         SplMade = as.integer(),
         SplCount = as.integer(),
         FundingSource = as.character(),
         FieldCollection = as.character(),
         Analyst = as.character())
}

initialize_zoo <- function(){
  tibble(Date = as.Date(as.character()),
         SiteID = as.character(),
         Replicate = as.integer(),
         FileName = as.character(),
         Parameter = as.character(),
         Value = as.numeric(),
         RawCount = as.integer())
}

initialize_mysid_sample <- function(){
  tibble(Date = as.Date(as.character()),
         SiteID = as.character(),
         Replicate = as.integer(),
         FileName = as.character(),
         MonthCat = as.character(),
         Time = as.integer(),
         Depth = units::set_units(numeric(), "m"),
         DepthCat = factor(as.character(), levels = c("Shallow", "Deep")),
         SideLake = as.character(),
         SplMade = as.integer(),
         SplCount = as.integer(),
         FundingSource = as.character(),
         FieldCollection = as.character(),
         Analyst = as.character(),
         Comment = as.character())
}

initialize_mysid <- function(){
  tibble(Date = as.Date(as.character()),
         SiteID = as.character(),
         Replicate = as.integer(),
         Parameter = as.character(),
         Value = as.numeric())
}

initialize_phyto_sample <- function(){
  tibble(Date = as.Date(as.character()),
         SiteID = as.character(),
         Depth = as.character(),
         FileName = as.character())
}

initialize_phyto <- function(){
  tibble(Date = as.Date(as.character()),
         SiteID = as.character(),
         Depth = as.character(),
         Taxa = as.character(),
         SpeciesBvol = as.numeric(),
         Biomass = as.numeric(),
         Abundance = as.numeric(),
         Biovolume = as.numeric(),
         CellCount = as.integer())
}
