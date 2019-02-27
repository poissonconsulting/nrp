library(devtools)
library(readxl)
library(dplyr)
library(magrittr)
library(poisspatial)

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

lakes <- ps_deactivate_sfc(sites) %>%
select(SiteID, BasinArm)

lakes$Lake[grepl("AR", lakes$SiteID) | grepl("HL", lakes$SiteID)] <- "Arrow"
lakes$Lake[grepl("KL", lakes$SiteID)] <- "Kootenay"

lakes %<>% select(Lake, BasinArm) %>%
  filter(!is.na(Lake)) %>%
  unique()


kl_lookup <- read.csv("data-raw/KL-site-lookup.csv", stringsAsFactors = FALSE)
ar_lookup <- read.csv("data-raw/AR-site-lookup.csv", stringsAsFactors = FALSE)
site_date_lookup <- rbind(kl_lookup, ar_lookup)

use_data(lakes, overwrite = TRUE)
use_data(sites, overwrite = TRUE)
use_data(site_date_lookup, overwrite = TRUE)
