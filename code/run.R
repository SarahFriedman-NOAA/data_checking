# toggle if in season data checking or data finalization post-survey
in_season <- TRUE


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
         "googlesheets4"
)

for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}
rm(p, pkg)



## Download and clean Oracle data ---------------------------------------------------

# loading bespoke functions
source("code/functions.R")


# connecting to Oracle database
if (file.exists("Z:/Projects/ConnectToOracle.R")) {
  source("Z:/Projects/ConnectToOracle.R")
} else {
  gapindex::get_connected()
}

# downloads data and reads in csvs
source("code/00_download_data.R")


# downloading, cleaning, and formatting racebase data
source("code/00_clean_data.R")





## Abundance Haul issues --------------------------------------------------

# table of problematic hauls where abundance haul needs to be set to "N" during finalization
if(!in_season){
  source("code/01_abundance_haul_checks.R")
  View(abundance_haul_issues)
}



## Catch data issues --------------------------------------------------

# generates plots of problematic specimen lengths/weights
source("code/02_specimen_checks.R")

# length_plot
# weight_plot



## Species range issues --------------------------------------------------

# checks for geographical outliers based on historical data
# code takes a while to run!
source("code/03_range_checks.R")




## Writing output --------------------------------------------------

# writing all issues to csv output
out <- outlier_df %>%
  dplyr::full_join(length_outliers) %>%
  dplyr::full_join(catch_outliers) %>%
  dplyr::full_join(specimen_outliers) %>%
  dplyr::select(cruise, region, vessel, haul, issue,
                species_name, common_name, species_code,
                vouchered, length_mm, weight_kg) %>%
  dplyr::mutate(weight_kg = round(weight_kg, 3)) %>%
  dplyr::arrange(cruise, region, vessel, haul) 

readr::write_csv(out, paste0(out_dir, "/all_catch_outliers_", this_year, ".csv"), na = "")


# download current drive version
drive_file <- "1Slgd3A94RfzKzwfilxgrs4NSA9HxT4NEgiqIK4sFoVg"
drive_version <- googlesheets4::read_sheet(drive_file)

# combine drive version and current version
new_rows <- anti_join(out, drive_version) %>%
  dplyr::arrange(cruise, region, vessel, haul) 

# append new rows to sheet
googlesheets4::sheet_append(drive_file, new_rows)

