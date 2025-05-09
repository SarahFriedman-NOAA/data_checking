# Use cached RACEBASE data? Will always download edit tables fresh
use_cached <- TRUE

# spreadsheet on google drive where outliers will be written
drive_file <- "1Wgz3uu4h8X1NAQPdBFtdESF9f8RDKORLaCMjUSfAn4I"


## Load packages & functions -----------------------------------------------------------
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




## Catch data issues --------------------------------------------------

# generates plots of problematic specimen lengths/weights
source("code/02_specimen_checks.R")

# checks for geographical outliers based on historical data
# code takes a while to run!
source("code/03_range_checks.R")

# length_plot
# weight_plot



## Writing output --------------------------------------------------

# writing all issues to csv output
out <- outlier_df %>%
  dplyr::full_join(length_outliers) %>%
  dplyr::full_join(catch_outliers) %>%
  dplyr::full_join(specimen_outliers) %>%
  dplyr::mutate(avg_weight_kg = round(weight_kg, 2)) %>%
  dplyr::select(
    cruise, region, vessel, haul, issue,
    species_name, common_name, species_code,
    vouchered, length_mm, avg_weight_kg
  ) %>%
  dplyr::arrange(cruise, region, vessel, haul)
readr::write_csv(out, paste0(out_dir, "/all_catch_outliers_", this_year, ".csv"), na = "")




# authorize googlesheets4
googlesheets4::gs4_auth()
1

# download current drive version
drive_version <- googlesheets4::read_sheet(drive_file,
  range = "A:L",
  col_types = "Ddcddcccdcdd"
) %>%
  janitor::clean_names()

# combine drive version and current version
new_rows <- dplyr::anti_join(
  out, drive_version,
  join_by(cruise, vessel, haul, issue, species_code)
) %>%
  dplyr::mutate(date_script_run = Sys.Date()) %>%
  dplyr::select(date_script_run, everything()) %>%
  dplyr::arrange(cruise, region, vessel, haul)


# append new rows to google sheet
googlesheets4::sheet_append(drive_file, new_rows)

# add checkmarks to google sheet
googlesheets4:::range_add_validation(drive_file,
  range =
    paste0("outliers!M", nrow(drive_version) + 1, ":M", nrow(drive_version) + nrow(new_rows) + 1),
  rule = rule_checkbox
)
