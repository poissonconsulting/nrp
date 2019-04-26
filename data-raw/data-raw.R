library(poispkgs)
library(usethis)

emsSites <- read_csv("data-raw/emsSites.csv")
emsSitesJoin <- emsSites %>% select(SiteID, EmsSiteName = SiteName)

ctdSites <- read_csv("data-raw/ctdSites.csv") %>%
  left_join(emsSitesJoin, by = "SiteID") %>%
  ps_coords_to_sfc(crs = 26911) %>%
  mutate(MaxDepth = set_units(MaxDepth, "m")) %>%
  select(SiteID, EmsSiteNumber = SiteNumber, SiteName, EmsSiteName, BasinArm, MaxDepth)

emsSites %<>%
  ps_coords_to_sfc(crs = 26911)

# mapview(emsSites, color = "yellow") +
#   mapview(ctdSites)


ems_param_lookup <- read_excel("~/Poisson/Data/nrp-database-19/Chem Data/Reference files/Chem_parameter_lookup.xlsx")
ems_param_lookup %<>%  mutate(PARAMETER = ifelse(PARAMETER %in% c("Phosphorus Total") &
                                              ANALYTICAL_METHOD == "Total Metals in Water by ICPMS (Ultra)",
                                              "Phosphorus Total metals", PARAMETER),
                          PARAMETER = ifelse(PARAMETER %in% c("Phosphorus Total Dissolved") &
                                               ANALYTICAL_METHOD %in% c("ICP"), "Phosphorus Total Dissolved metals", PARAMETER),
                          Comment = ifelse(Comment == "metals; note that this parameter name is the same as the standard analysis - needs re-naming",
                          "metals", Comment))

basinArm <- ps_deactivate_sfc(ctdSites) %>%
  select(SiteID, BasinArm)

basinArm$Lake[grepl("AR", basinArm$SiteID) | grepl("HL", basinArm$SiteID)] <- "Arrow"
basinArm$Lake[grepl("KL", basinArm$SiteID)] <- "Kootenay"

basinArm %<>% select(Lake, BasinArm) %>%
  filter(!is.na(Lake)) %>%
  unique()

lakes <- st_read("data-raw/lakes.gpkg")
lakes$Area <- st_area(lakes)
lakes %<>% ps_deactivate_sfc() %>%
  select(Lake, Area, geometry = geom) %>%
  ps_activate_sfc()

kl_lookup <- read.csv("data-raw/KL-site-lookup.csv", stringsAsFactors = FALSE)
ar_lookup <- read.csv("data-raw/AR-site-lookup.csv", stringsAsFactors = FALSE)
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


use_data(basinArm, overwrite = TRUE)
use_data(ctdSites, overwrite = TRUE)
use_data(lakes, overwrite = TRUE)
use_data(emsSites, overwrite = TRUE)
use_data(ems_param_lookup, overwrite = TRUE)
use_data(site_date_lookup, overwrite = TRUE, internal = TRUE)
use_data(ems_standard_init, overwrite = TRUE)
use_data(ems_metals_init, overwrite = TRUE)

