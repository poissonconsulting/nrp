library(poispkgs)
library(usethis)

sites <- read_excel("data-raw/Sites.xlsx", skip = 1)

sites %<>% rename(SiteID = `Site ID`, SiteNumber = `EMS Site No.`, SiteName = `Site Name`,
                     BasinArm = Basin, X = `UTM Easting`, Y = `UTM Northing`) %>%
  filter(SiteID %in% c("AR1", "AR2", "AR3", "AR4", "AR5", "AR6", "AR7", "AR8", "AR9", "KL1", "KL2", "KL3",
                          "KL4", "KL5", "KL6", "KL7", "KL8", "HL1", "HL2", "HL3", "HL4", "TR3")) %>%
  mutate(BasinArm = gsub(" .*", '' , BasinArm), SiteNumber = gsub(" .*", '' , SiteNumber),
         SiteName = gsub("Kootenay Lake at ", '' , SiteName), SiteName = gsub("Arrow Lake at ", '' , SiteName),
         SiteName = gsub("Arrow Lake, ", '' , SiteName), SiteName = gsub("Kootenay Lake – ", '' , SiteName),
         X = as.numeric(X), Y = as.numeric(Y), Depth = as.numeric(Depth), Depth = units::set_units(Depth, "m")) %>%
  ps_coords_to_sfc(crs = 26911)

sites$BasinArm[sites$BasinArm == "Syringa"] <- "Lower"

emsSites <- sites %>%
  ps_deactivate_sfc() %>%
  select(SiteID, EmsSite = SiteNumber) %>%
  filter(!SiteID %in% c("HL2", "HL3", "HL4", "AR9", "TR3"))

emsSites$SiteName <- c("ARROW LAKE AT ALBERT POINT (AR1)",
                    "ARROW LAKE AT ANN POINT (AR2)",
                    "ARROW LAKE AT TURNER CREEK (AR3)",
                    "ARROW LAKE AT SLEWISKIN CREEK (AR4)",
                    "ARROW LAKE D/S MOSQUITO CREEK (AR5)",
                    "ARROW LAKE AT JOHNSON CREEK (AR6)",
                    "ARROW LAKE AT BOWMAN CREEK (AR7)",
                    "ARROW LAKE AT CAYUSE CREEK (AR8)",
                    "ARROW LAKE; BEATON ARM (HL1)",
                    "KOOTENAY LAKE AT JOHNSON`S LANDING (KLF 1)",
                    "KOOTENAY LAKE AT KEMBELL CR. (KLF 2)",
                    "KOOTENAY LAKE AT BJERKENESS CR. (KLF 3)",
                    "KOOTENAY LAKE AT HENDRYX CR. (KLF 4)",
                    "KOOTENAY LAKE AT CRAWFORD BAY (KLF 5)",
                    "KOOTENAY LAKE AT RHINOCEROS POINT (KLF 6)",
                    "KOOTENAY LAKE AT REDMAN PT; KLF7",
                    "KOOTENAY LAKE WEST ARM (KLF8)")

path <-  system.file("extdata", "ems/test_ems.rds", package = "nrp", mustWork = TRUE)
ems <- readRDS(path)

emsSites %<>% select(SiteID, EmsSite, SiteName) %>%
  left_join(select(ems, LATITUDE, LONGITUDE, MONITORING_LOCATION), by = c("SiteName" = "MONITORING_LOCATION")) %>%
  distinct() %>%
  ps_coords_to_sfc(coords = c("LONGITUDE", "LATITUDE")) %>%
  st_transform(26911)



ems_param_lookup <- read_excel("~/Poisson/Data/nrp-database-19/Chem Data/Reference files/Chem_parameter_lookup.xlsx")
ems_param_lookup %<>%  mutate(PARAMETER = ifelse(PARAMETER %in% c("Phosphorus Total") &
                                              ANALYTICAL_METHOD == "Total Metals in Water by ICPMS (Ultra)",
                                              "Phosphorus Total metals", PARAMETER),
                          PARAMETER = ifelse(PARAMETER %in% c("Phosphorus Total Dissolved") &
                                               ANALYTICAL_METHOD %in% c("ICP"), "Phosphorus Total Dissolved metals", PARAMETER),
                          Comment = ifelse(Comment == "metals; note that this parameter name is the same as the standard analysis - needs re-naming",
                          "metals", Comment))

basinArm <- ps_deactivate_sfc(sites) %>%
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


conn <- nrp_create_db(path  = ":memory:", ask = FALSE)
ems_standard <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "standard") %>%
  mutate(REQUISITION_ID = as.numeric(REQUISITION_ID))
ems_standard_init <- ems_standard[0, ]
ems_metals <- nrp_extract_ems(data = ems, db_path = conn, analysis_type = "metals") %>%
  mutate(REQUISITION_ID = as.numeric(REQUISITION_ID))
ems_metals_init <- ems_metals[0, ]


use_data(basinArm, overwrite = TRUE)
use_data(sites, overwrite = TRUE)
use_data(lakes, overwrite = TRUE)
use_data(emsSites, overwrite = TRUE)
use_data(ems_param_lookup, overwrite = TRUE)
use_data(site_date_lookup, overwrite = TRUE, internal = TRUE)
use_data(ems_standard_init, overwrite = TRUE)
use_data(ems_metals_init, overwrite = TRUE)

