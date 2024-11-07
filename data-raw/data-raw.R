library(readxl)
library(readr)
library(usethis)
library(poisspatial)
library(dplyr)
library(sf)
library(units)
library(magrittr)
devtools::load_all()
# NOTE on ems test data: test_ems.rds
# to update the test ems data use rems::get_ems_data(which = "4yr") to download
# from the EMS database. subset to a smaller size that can travel with the package
# the data set is long format, so be sure to include at least 1 observation of each parameter

emsSites <- read_csv("data-raw/emsSites.csv")
emsSitesJoin <- emsSites %>% select(SiteID, EmsSiteName = SiteName)

ctdSites <- read_csv("data-raw/ctdSites.csv") %>%
  left_join(emsSitesJoin, by = "SiteID") %>%
  ps_coords_to_sfc(crs = 26911) %>%
  st_transform(4326) %>%
  mutate(MaxDepth = set_units(MaxDepth, "m")) %>%
  select(SiteID, EmsSiteNumber = SiteNumber, SiteName, EmsSiteName, BasinArm, MaxDepth)

emsSites %<>%
  ps_coords_to_sfc(crs = 26911) %>%
  st_transform(4326)

ems_param_lookup <- read_excel("data-raw/Chem_parameter_lookup.xlsx")

ems_param_lookup %<>%
  mutate(
    PARAMETER = ifelse(PARAMETER %in% c("Phosphorus Total") &
      ANALYTICAL_METHOD == "Total Metals in Water by ICPMS (Ultra)",
    "Phosphorus Total metals", PARAMETER
    ),
    PARAMETER = ifelse(PARAMETER %in% c("Phosphorus Total Dissolved") &
      ANALYTICAL_METHOD %in% c("ICP"), "Phosphorus Total Dissolved metals", PARAMETER),
    Comment = ifelse(Comment == "metals; note that this parameter name is the same as the standard analysis - needs re-naming",
      "metals", Comment
    )
  )

basinArm <- as_tibble(ctdSites) %>%
  select(SiteID, BasinArm)

basinArm$Lake[grepl("AR", basinArm$SiteID) | grepl("HL", basinArm$SiteID)] <- "Arrow"
basinArm$Lake[grepl("KL", basinArm$SiteID)] <- "Kootenay"

basinArm %<>% select(Lake, BasinArm) %>%
  filter(!is.na(Lake)) %>%
  unique()

lakes <- st_read("data-raw/lakes.gpkg")
lakes$Area <- st_area(lakes)
lakes %<>% as_tibble() %>%
  select(Lake, Area, geometry = geom) %>%
  ps_activate_sfc() %>%
  st_transform(4326)

kl_lookup <- read.csv("data-raw/KL-site-lookup.csv")
ar_lookup <- read.csv("data-raw/AR-site-lookup.csv")
site_date_lookup <- rbind(kl_lookup, ar_lookup)


conn <- nrp_create_db(path = ":memory:", ask = FALSE)

# ems_standard <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "standard") %>%
#   mutate(ReplicateID = as.numeric(1), LOWER_DEPTH = units::set_units(.data$LOWER_DEPTH, "m"),
#          UPPER_DEPTH = units::set_units(.data$UPPER_DEPTH, "m")) %>%
#   select(SiteID, COLLECTION_START, COLLECTION_END, REQUISITION_ID,
#          ANALYZING_AGENCY, UPPER_DEPTH, LOWER_DEPTH, ReplicateID, everything())
# ems_standard_init <- ems_standard[0, ]
#
# ems_metals <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "metals") %>%
#   mutate(ReplicateID = as.numeric(1),
#          LOWER_DEPTH = units::set_units(.data$LOWER_DEPTH, "m"),
#          UPPER_DEPTH = units::set_units(.data$UPPER_DEPTH, "m")) %>%
#   select(SiteID, COLLECTION_START, COLLECTION_END, REQUISITION_ID,
#          ANALYZING_AGENCY, UPPER_DEPTH, LOWER_DEPTH, ReplicateID, everything())
# ems_metals_init <- ems_metals[0, ]

zoo_input_cols <- c(
  "FileName" = "character",
  "MonthCat" = "character",
  "Date" = "date",
  "Station" = "integer",
  "Replicate" = "integer",
  "SexFecCode" = "character",
  "CopStageCode" = "character",
  "DenTotal" = "numeric",
  "DCopep" = "numeric",
  "DClad" = "numeric",
  "DClad other than Daph" = "numeric",
  "DDash" = "numeric",
  "DDkenai" = "numeric",
  "DEpi" = "numeric",
  "DCycl" = "numeric",
  "DNaup" = "numeric",
  "DDaph" = "numeric",
  "DDiaph" = "numeric",
  "DBosm" = "numeric",
  "DScap" = "numeric",
  "DLepto" = "numeric",
  "DCerio" = "numeric",
  "DChyd" = "numeric",
  "DOtherCopep" = "numeric",
  "DOtherClad" = "numeric",
  "DDashM" = "numeric",
  "DDashF" = "numeric",
  "DDash5" = "numeric",
  "DDash4" = "numeric",
  "DDash3" = "numeric",
  "DDash2" = "numeric",
  "DDash1" = "numeric",
  "DDashC" = "numeric",
  "DDkenaiM" = "numeric",
  "DDkenaiF" = "numeric",
  "DDkenaiC" = "numeric",
  "DEpiM" = "numeric",
  "DEpiF" = "numeric",
  "DEpiC" = "numeric",
  "DCyclM" = "numeric",
  "DCyclF" = "numeric",
  "DCycl5" = "numeric",
  "DCycl4" = "numeric",
  "DCycl3" = "numeric",
  "DCycl2" = "numeric",
  "DCycl1" = "numeric",
  "DCyclC" = "numeric",
  "BiomTotal" = "numeric",
  "BCopep" = "numeric",
  "BClad" = "numeric",
  "BClad other than Daph" = "numeric",
  "BDash" = "numeric",
  "BDkenai" = "numeric",
  "BEpi" = "numeric",
  "BCycl" = "numeric",
  "BNaup" = "numeric",
  "BDaph" = "numeric",
  "BDiaph" = "numeric",
  "BBosm" = "numeric",
  "BScap" = "numeric",
  "BLepto" = "numeric",
  "BCerio" = "numeric",
  "BChyd" = "numeric",
  "BOtherCopep" = "numeric",
  "BOtherClad" = "numeric",
  "BDashM" = "numeric",
  "BDashF" = "numeric",
  "BDash5" = "numeric",
  "BDash4" = "numeric",
  "BDash3" = "numeric",
  "BDash2" = "numeric",
  "BDash1" = "numeric",
  "BDashC" = "numeric",
  "BDkenaiM" = "numeric",
  "BDkenaiF" = "numeric",
  "BDkenaiC" = "numeric",
  "BEpiM" = "numeric",
  "BEpiF" = "numeric",
  "BEpiC" = "numeric",
  "BCyclM" = "numeric",
  "BCyclF" = "numeric",
  "BCycl5" = "numeric",
  "BCycl4" = "numeric",
  "BCycl3" = "numeric",
  "BCycl2" = "numeric",
  "BCycl1" = "numeric",
  "BCyclC" = "numeric",
  "F1Dash" = "numeric",
  "F1Dkenai" = "numeric",
  "F1Epi" = "numeric",
  "F1Cycl" = "numeric",
  "F1Daph" = "numeric",
  "F1Diaph" = "numeric",
  "F1Bosm" = "numeric",
  "F1Scap" = "numeric",
  "F1Lepto" = "numeric",
  "F1Cerio" = "numeric",
  "F1Chyd" = "numeric",
  "F2Dash" = "numeric",
  "F2Dkenai" = "numeric",
  "F2Epi" = "numeric",
  "F2Cycl" = "numeric",
  "F2Daph" = "numeric",
  "F2Diaph" = "numeric",
  "F2Bosm" = "numeric",
  "F2Scap" = "numeric",
  "F2Lepto" = "numeric",
  "F2Cerio" = "numeric",
  "F2Chyd" = "numeric",
  "F3Dash" = "numeric",
  "F3Dkenai" = "numeric",
  "F3Epi" = "numeric",
  "F3Cycl" = "numeric",
  "F3Daph" = "numeric",
  "F3Diaph" = "numeric",
  "F3Bosm" = "numeric",
  "F3Scap" = "numeric",
  "F3Lepto" = "numeric",
  "F3Cerio" = "numeric",
  "F3Chyd" = "numeric",
  "F4Dash" = "numeric",
  "F4Dkenai" = "numeric",
  "F4Epi" = "numeric",
  "F4Cycl" = "numeric",
  "F4Daph" = "numeric",
  "F4Diaph" = "numeric",
  "F4Bosm" = "numeric",
  "F4Scap" = "numeric",
  "F4Lepto" = "numeric",
  "F4Cerio" = "numeric",
  "F4Chyd" = "numeric",
  "F5Dash" = "numeric",
  "F5Dkenai" = "numeric",
  "F5Epi" = "numeric",
  "F5Cycl" = "numeric",
  "F5Daph" = "numeric",
  "F5Diaph" = "numeric",
  "F5Bosm" = "numeric",
  "F5Scap" = "numeric",
  "F5Lepto" = "numeric",
  "F5Cerio" = "numeric",
  "F5Chyd" = "numeric",
  "F6Dash" = "numeric",
  "F6Dkenai" = "numeric",
  "F6Epi" = "numeric",
  "F6Cycl" = "numeric",
  "F6Daph" = "numeric",
  "F6Diaph" = "numeric",
  "F6Bosm" = "numeric",
  "F6Scap" = "numeric",
  "F6Lepto" = "numeric",
  "F6Cerio" = "numeric",
  "F6Chyd" = "numeric",
  "HaulTime" = "numeric",
  "ENDREV" = "integer",
  "STARTREV" = "integer",
  "TOTREV" = "integer",
  "SPLmade" = "integer",
  "SPLcount" = "integer",
  "vertical haul depth_m" = "numeric",
  "vertical haul net size_m" = "numeric",
  "FundingSource" = "character",
  "FieldCollection" = "character",
  "Analyst" = "character"
)

zoo_params <- c(
  "SexFecCode", "CopStageCode",
  "DenTotal", "DCopep", "DClad", "DClad other than Daph", "DDash",
  "DDkenai", "DEpi", "DCycl", "DNaup", "DDaph", "DDiaph", "DBosm",
  "DScap", "DLepto", "DCerio", "DChyd", "DOtherCopep", "DOtherClad",
  "DDashM", "DDashF", "DDash5", "DDash4", "DDash3", "DDash2", "DDash1",
  "DDashC", "DDkenaiM", "DDkenaiF", "DDkenaiC", "DEpiM", "DEpiF",
  "DEpiC", "DCyclM", "DCyclF", "DCycl5", "DCycl4", "DCycl3", "DCycl2",
  "DCycl1", "DCyclC", "BiomTotal", "BCopep", "BClad", "BClad other than Daph",
  "BDash", "BDkenai", "BEpi", "BCycl", "BNaup", "BDaph", "BDiaph",
  "BBosm", "BScap", "BLepto", "BCerio", "BChyd", "BOtherCopep",
  "BOtherClad", "BDashM", "BDashF", "BDash5", "BDash4", "BDash3",
  "BDash2", "BDash1", "BDashC", "BDkenaiM", "BDkenaiF", "BDkenaiC",
  "BEpiM", "BEpiF", "BEpiC", "BCyclM", "BCyclF", "BCycl5", "BCycl4",
  "BCycl3", "BCycl2", "BCycl1", "BCyclC", "F1Dash", "F1Dkenai",
  "F1Epi", "F1Cycl", "F1Daph", "F1Diaph", "F1Bosm", "F1Scap", "F1Lepto",
  "F1Cerio", "F1Chyd", "F2Dash", "F2Dkenai", "F2Epi", "F2Cycl",
  "F2Daph", "F2Diaph", "F2Bosm", "F2Scap", "F2Lepto", "F2Cerio",
  "F2Chyd", "F3Dash", "F3Dkenai", "F3Epi", "F3Cycl", "F3Daph",
  "F3Diaph", "F3Bosm", "F3Scap", "F3Lepto", "F3Cerio", "F3Chyd",
  "F4Dash", "F4Dkenai", "F4Epi", "F4Cycl", "F4Daph", "F4Diaph",
  "F4Bosm", "F4Scap", "F4Lepto", "F4Cerio", "F4Chyd", "F5Dash",
  "F5Dkenai", "F5Epi", "F5Cycl", "F5Daph", "F5Diaph", "F5Bosm",
  "F5Scap", "F5Lepto", "F5Cerio", "F5Chyd", "F6Dash", "F6Dkenai",
  "F6Epi", "F6Cycl", "F6Daph", "F6Diaph", "F6Bosm", "F6Scap", "F6Lepto",
  "F6Cerio", "F6Chyd"
)


mysid_input_cols <- c(
  "FileName" = "character",
  "Date" = "date",
  "MonthCat" = "character",
  "Day" = "integer",
  "Station" = "integer",
  "Replicate" = "integer",
  "Time" = "integer",
  "Depth" = "integer",
  "DepthCat" = "character",
  "SideLake" = "character",
  "#splitsCounted" = "integer",
  "#splitsMade" = "integer",
  "DenTotal" = "numeric",
  "Djuv" = "numeric",
  "DimmM" = "numeric",
  "DmatM" = "numeric",
  "DbreedM" = "numeric",
  "DimmF" = "numeric",
  "DmatF" = "numeric",
  "DbroodF" = "numeric",
  "DspentF" = "numeric",
  "DdistBrF" = "numeric",
  "BiomTotal" = "numeric",
  "Bjuv" = "numeric",
  "BimmM" = "numeric",
  "BmatM" = "numeric",
  "BbreedM" = "numeric",
  "BimmF" = "numeric",
  "BmatF" = "numeric",
  "BbroodF" = "numeric",
  "BspentF" = "numeric",
  "BdistBrF" = "numeric",
  "VolDenTotal" = "numeric",
  "VolDjuv" = "numeric",
  "VolDimmM" = "numeric",
  "VolDmatM" = "numeric",
  "VolDbreedM" = "numeric",
  "VolDimmF" = "numeric",
  "VolDmatF" = "numeric",
  "VolDbroodF" = "numeric",
  "VolDspentF" = "numeric",
  "VolDdisBrF" = "numeric",
  "Eggs/BroodF" = "numeric",
  "Eggs/DistBrF" = "numeric",
  "Eggs/Total#Mysids" = "numeric",
  "PropFemGravid" = "numeric",
  "FundingSource" = "character",
  "FieldCollection" = "character",
  "Analyst" = "character",
  "Comment" = "character"
)

mysid_params <- c(
  "DenTotal", "Djuv", "DimmM", "DmatM", "DbreedM", "DimmF", "DmatF",
  "DbroodF", "DspentF", "DdistBrF", "BiomTotal", "Bjuv", "BimmM",
  "BmatM", "BbreedM", "BimmF", "BmatF", "BbroodF", "BspentF", "BdistBrF",
  "VolDenTotal", "VolDjuv", "VolDimmM", "VolDmatM", "VolDbreedM",
  "VolDimmF", "VolDmatF", "VolDbroodF", "VolDspentF", "VolDdisBrF",
  "Eggs/BroodF", "Eggs/DistBrF", "Eggs/Total#Mysids", "PropFemGravid"
)

phyto_input_cols <- c(
  "Samp_Date" = "date",
  "Site_Name" = "character",
  "SiteLoc_LocName" = "character",
  "Samp_Depth" = "character",
  "Class_Name" = "character",
  "Class_Alias" = "character",
  "Species_Name" = "character",
  "Count_Number" = "numeric",
  "NCU/mL" = "numeric",
  "Species_Bvol" = "numeric",
  "Biovolume (mm3/L)" = "numeric",
  "Biomass" = "numeric",
  "Edibility" = "character",
  "FileName" = "character"
)

phyto_species <- read_csv("data-raw/phyto_species.csv")

use_data(phyto_species, overwrite = TRUE)
use_data(zoo_input_cols, overwrite = TRUE)
use_data(mysid_input_cols, overwrite = TRUE)
use_data(phyto_input_cols, overwrite = TRUE)
use_data(zoo_params, overwrite = TRUE)
use_data(mysid_params, overwrite = TRUE)
use_data(basinArm, overwrite = TRUE)
use_data(ctdSites, overwrite = TRUE)
use_data(lakes, overwrite = TRUE)
use_data(site_date_lookup, overwrite = TRUE, internal = TRUE)
# use_data(ems_standard_init, overwrite = TRUE)
# use_data(ems_metals_init, overwrite = TRUE)
