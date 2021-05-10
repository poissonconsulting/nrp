library(readxl)
library(readr)
library(usethis)
library(poisspatial)
library(dplyr)
library(sf)
library(units)
library(magrittr)


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
  mutate(PARAMETER = ifelse(PARAMETER %in% c("Phosphorus Total") &
                              ANALYTICAL_METHOD == "Total Metals in Water by ICPMS (Ultra)",
                            "Phosphorus Total metals", PARAMETER),
         PARAMETER = ifelse(PARAMETER %in% c("Phosphorus Total Dissolved") &
                              ANALYTICAL_METHOD %in% c("ICP"), "Phosphorus Total Dissolved metals", PARAMETER),
         Comment = ifelse(Comment == "metals; note that this parameter name is the same as the standard analysis - needs re-naming",
                          "metals", Comment))

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

path <-  system.file("extdata", "ems/test_ems.rds", package = "nrp", mustWork = TRUE)
ems <- readRDS(path)
conn <- nrp_create_db(path  = ":memory:", ask = FALSE)

ems_standard <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "standard") %>%
  mutate(ReplicateID = as.numeric(1), LOWER_DEPTH = units::set_units(.data$LOWER_DEPTH, "m"),
         UPPER_DEPTH = units::set_units(.data$UPPER_DEPTH, "m")) %>%
  select(SiteID, COLLECTION_START, COLLECTION_END, REQUISITION_ID,
         ANALYZING_AGENCY, UPPER_DEPTH, LOWER_DEPTH, ReplicateID, everything())
ems_standard_init <- ems_standard[0, ]

ems_metals <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "metals") %>%
  mutate(ReplicateID = as.numeric(1),
         LOWER_DEPTH = units::set_units(.data$LOWER_DEPTH, "m"),
         UPPER_DEPTH = units::set_units(.data$UPPER_DEPTH, "m")) %>%
  select(SiteID, COLLECTION_START, COLLECTION_END, REQUISITION_ID,
         ANALYZING_AGENCY, UPPER_DEPTH, LOWER_DEPTH, ReplicateID, everything())
ems_metals_init <- ems_metals[0, ]


zoo_input_cols <- c("FileName", "YearComp", "Year", "Month", "MonthCat", "Date1", "Date2",
              "Station", "Season", "Basin", "Replicate", "SexFecCode", "CopStageCode",
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
              "F6Cerio", "F6Chyd", "HaulTime", "ENDREV", "STARTREV", "TOTREV",
              "SPLmade", "SPLcount", "vertical haul depth_m", "vertical haul net size_m",
              "FundingSource", "FieldCollection", "Analyst", "MaxDepth")

zoo_params <- c("SexFecCode", "CopStageCode",
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
                "F6Cerio", "F6Chyd")

mysid_input_cols <- c("FileName", "Date", "Year", "Month", "MonthCat", "Day", "Station", "Replicate",
                               "Time", "Depth", "DepthCat", "SideLake", "#splitsCounted", "#splitsMade",
                               "DenTotal", "Djuv", "DimmM", "DmatM", "DbreedM", "DimmF", "DmatF",
                               "DbroodF", "DspentF", "DdistBrF", "BiomTotal", "Bjuv", "BimmM",
                               "BmatM", "BbreedM", "BimmF", "BmatF", "BbroodF", "BspentF", "BdistBrF",
                               "VolDenTotal", "VolDjuv", "VolDimmM", "VolDmatM", "VolDbreedM",
                               "VolDimmF", "VolDmatF", "VolDbroodF", "VolDspentF", "VolDdisBrF",
                               "Eggs/BroodF", "Eggs/DistBrF", "Eggs/Total#Mysids", "PropFemGravid",
                               "FundingSource", "FieldCollection", "Analyst", "Comment")

mysid_params <- c("DenTotal", "Djuv", "DimmM", "DmatM", "DbreedM", "DimmF", "DmatF",
                  "DbroodF", "DspentF", "DdistBrF", "BiomTotal", "Bjuv", "BimmM",
                  "BmatM", "BbreedM", "BimmF", "BmatF", "BbroodF", "BspentF", "BdistBrF",
                  "VolDenTotal", "VolDjuv", "VolDimmM", "VolDmatM", "VolDbreedM",
                  "VolDimmF", "VolDmatF", "VolDbroodF", "VolDspentF", "VolDdisBrF",
                  "Eggs/BroodF", "Eggs/DistBrF", "Eggs/Total#Mysids", "PropFemGravid")

use_data(zoo_input_cols, overwrite = TRUE)
use_data(mysid_input_cols, overwrite = TRUE)
use_data(zoo_params, overwrite = TRUE)
use_data(mysid_params, overwrite = TRUE)
use_data(basinArm, overwrite = TRUE)
use_data(ctdSites, overwrite = TRUE)
use_data(lakes, overwrite = TRUE)
use_data(emsSites, overwrite = TRUE)
use_data(ems_param_lookup, overwrite = TRUE)
use_data(site_date_lookup, overwrite = TRUE, internal = TRUE)
use_data(ems_standard_init, overwrite = TRUE)
use_data(ems_metals_init, overwrite = TRUE)
