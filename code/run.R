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
         "mgcv")

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
source("code/01_abundance_haul_checks.R")

View(abundance_haul_issues)




## Catch data issues --------------------------------------------------

# generates plots of problematic specimen lengths/weights
source("code/02_specimen_checks.R")

length_plot
weight_plot



## Species range issues --------------------------------------------------

# checks for geographical outliers based on historical data
# code takes a while to run!
source("code/03_range_checks.R")



# writing all issues to csv output
outlier_df %>%
  dplyr::full_join(length_outliers) %>%
  dplyr::full_join(catch_outliers) %>%
  dplyr::full_join(specimen_outliers) %>%
  dplyr::select(issue, cruise, region, vessel, haul, 
                species_name, common_name, species_code, everything()) %>%
  dplyr::arrange(cruise, region, vessel, haul) %>%
  readr::write_csv(paste0("output/all_biological_outliers_", this_year, ".csv"))
