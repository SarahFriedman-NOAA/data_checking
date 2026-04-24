## =============================================================================
## Load packages & functions
## =============================================================================
# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, RODBC, here, janitor, dbscan, 
  ggforce, mgcv, googlesheets4, sf, assertr
)


# Load custom functions
source("code/functions.R")


# Toggle to download tables from racebase (this should be done annually); will always download edit tables fresh
use_cached <- TRUE 


# Authenticate Google Sheets
gs4_auth()



## =============================================================================
## Read existing Google Sheet data
## =============================================================================

# Google Sheet ID (outlier tracking)
drive_file <- "1TWDGeviQqf20bvhnqrZKBNzuno6UPlJuHVInN3sNHDg"


drive_version <- read_sheet(
  drive_file,
  range = "A:M",
  col_types = "Ddcddcccdcddl"
) %>%
  clean_names()




## =============================================================================
## Connect to Oracle
## =============================================================================

oracle_script <- "Z:/Projects/ConnectToOracle.R"

if (file.exists(oracle_script)) {
  source(oracle_script)
} else {
  channel <- gapindex::get_connected()
}



## =============================================================================
## Download & clean data
## =============================================================================

source("code/00_download_data.R")
source("code/01_clean_data.R")




## =============================================================================
## Run data quality checks
## =============================================================================

# Specimen-level checks (length/weight issues)
source("code/02_specimen_checks.R")

# Spatial/range checks (can be slow)
source("code/03_range_checks.R")



## =============================================================================
## Combine and export outliers
## =============================================================================

out <- outlier_df %>%
  full_join(length_outliers) %>%
  full_join(catch_outliers) %>%
  full_join(specimen_outliers) %>%
  mutate(avg_weight_kg = round(weight_kg, 2)) %>%
  select(
    cruise, region, vessel, haul, issue,
    species_name, common_name, species_code,
    vouchered, length_mm, avg_weight_kg
  ) %>%
  arrange(cruise, region, vessel, haul)

# Write CSV output
output_file <- file.path(out_dir, paste0("all_catch_outliers_", this_year, ".csv"))
write_csv(out, output_file, na = "")



## =============================================================================
## Identify new rows for Google Sheet
## =============================================================================

join_keys <- join_by(cruise, vessel, haul, issue, species_code)

new_rows <- out %>%
  anti_join(select(drive_version, -checked), by = join_keys) %>%
  mutate(date_script_run = Sys.Date()) %>%
  relocate(date_script_run) %>%
  arrange(cruise, region, vessel, haul)



## =============================================================================
## Append new rows to Google Sheet
## =============================================================================

if (nrow(new_rows) > 0) {
  sheet_append(drive_file, new_rows)
  
  # Add checkbox validation for new rows
  start_row <- nrow(drive_version) + 1
  end_row   <- start_row + nrow(new_rows)
  
  range <- sprintf("outliers!M%d:M%d", start_row, end_row)
  
  googlesheets4:::range_add_validation(
    drive_file,
    range = range,
    rule  = rule_checkbox
  )
}
