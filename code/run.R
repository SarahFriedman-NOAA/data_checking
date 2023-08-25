
## Load packages -----------------------------------------------------------
pkg <- c("tidyverse", 
         "RODBC",
         "here", 
         "janitor", 
         "getPass")

for (p in pkg) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}
rm(p, pkg)



## Download Oracle Data ---------------------------------------------------

source("code/connect_to_oracle.R")
#source("C:/Users/sarah.friedman/Work/Rfunctions/ConnectToOracle_STF.R")
source("code/00_download_data.R")




## Abundance Haul issues --------------------------------------------------

# table of problematic hauls where abundance haul needs to be set to "N" during finalization
source("code/01_abundance_haul_checks.R")

View(abundance_haul_issues)




## Catch issues --------------------------------------------------

# table of problematic specimen lengths/weights
source("code/02_specimen_checks.R")

length_plot
weight_plot
