# Use cached RACEBASE data? Will always download edit tables fresh
use_cached <- TRUE


## Load packages -----------------------------------------------------------
pkg <- c("tidyverse", 
         "RODBC",
         "here", 
         "janitor", 
         "getPass",
         "dbscan",
         "ggrepel",
         "gapindex",
         "ggforce",
         "mgcv",
         "GAPsurvey"
)


for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}
rm(p, pkg)


# loading bespoke functions
source("code/functions.R")



## Download and clean Oracle data ---------------------------------------------------

# connecting to Oracle database
if (file.exists("Z:/Projects/ConnectToOracle.R")) {
  source("Z:/Projects/ConnectToOracle.R")
} else {
  channel <- gapindex::get_connected()
}


# downloads data from oracle
source("code/00_download_data.R")


# reading, cleaning, and formatting data
source("code/01_clean_data.R")




## Run Checks -----------------------------------------------------------

# hauls with depth issues
depth_issues <- new_haul %>%
  dplyr::filter(gear_depth == 0 | bottom_depth == 0 | gear_depth >= bottom_depth) %>%
  dplyr::select(haul_id, cruise:station, performance, contains("depth")) %>%
  dplyr::arrange(region, cruise, vessel_id, haul)



# detailing haul issues (abundance_haul = "N")
abundance_haul_issues <- new_catch %>%
  dplyr::group_by(haul_id) %>%
  dplyr::mutate(haul_weight_t = round(sum(total_weight) / 1000, 1)) %>%
  dplyr::right_join(new_haul, by = join_by(haul_id)) %>%
  dplyr::ungroup() %>%
  dplyr::select(cruise, region, vessel_id, haul:duration, haul_weight_t) %>%
  unique() %>%
  dplyr::group_by(cruise, region, station, stratum) %>%
  dplyr::add_count(name = "n_station") %>% 
  dplyr::mutate(issue = case_when(
    !accessories %in% c(15, 129) ~ "improper accessories",
    !gear %in% c(44, 172) ~ "improper gear",
    haul_type != 3 ~ "haul type not 3",
    performance < 0 ~ "performance inadequate",
    duration < 10 & haul_weight_t < duration ~ "catch too small for tow duration",
    n_station > 1 & sum(haul_type == 3) > 1 & sum(performance >= 0) > 1 ~ "station duplicated",
    TRUE ~ "none"
  )) %>%
  dplyr::select(-n_station) %>%
  dplyr::filter(issue != "none") %>%
  dplyr::arrange(region, cruise, vessel_id, haul)


View(abundance_haul_issues)

