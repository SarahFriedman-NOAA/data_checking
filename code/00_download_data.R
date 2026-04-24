## =============================================================================
## Download data sets to local machine
## =============================================================================

# RACEBASE tables to query
locations <- c(
  # Biological edit data
  "RACE_DATA.EDIT_CATCH_SPECIES",
  "RACE_DATA.EDIT_CATCH_SAMPLES",
  "RACE_DATA.EDIT_LENGTHS",
  "RACE_DATA.EDIT_SPECIMENS",
  
  # Effort edit data
  "RACE_DATA.EDIT_HAULS",
  "RACE_DATA.EDIT_EVENTS",
  "RACE_DATA.EDIT_HAUL_MEASUREMENTS"
)

# Historical/reference tables
historical_data <- c(
  "RACE_DATA.V_CRUISES",
  "RACE_DATA.RACE_SPECIES_CODES",
  "RACEBASE.CATCH",
  "RACEBASE.LENGTH",
  "RACEBASE.HAUL",
  "RACEBASE.SPECIMEN",
  "RACEBASE.CRUISE"
)


## =============================================================================
## Prepare local cache
## =============================================================================

data_dir <- "data/oracle"

if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

# Helper for consistent naming
format_name <- function(x) gsub("\\.", "-", x)

existing_tables <- list.files(data_dir) %>%
  tools::file_path_sans_ext() %>%
  toupper()

historical_needed <- format_name(historical_data)

# Include historical tables if cache is incomplete or bypassed
if (!use_cached || !all(historical_needed %in% existing_tables)) {
  locations <- c(locations, historical_data)
}

required_tables <- format_name(locations)


## =============================================================================
## Download or use cached data
## =============================================================================

if (exists("channel")) {
  
  # Download tables from Oracle
  for (table in locations) {
    
    message("Downloading data from ", table)
    
    filename  <- tolower(format_name(table))
    file_path <- here::here(data_dir, paste0(filename, ".csv"))
    
    query <- paste0("SELECT * FROM ", table)
    dat   <- RODBC::sqlQuery(channel, query)
    
    readr::write_csv(dat, file_path)
    
    rm(dat)
  }
  
} else {
  
  # Use cached tables if available
  if (!all(required_tables %in% existing_tables)) {
    
    cat(
      "Not connected to Oracle database and cannot locate required cached tables.\n",
      "Connect to Oracle and re-run the download script to proceed.\n"
    )
    
    use_cached <- FALSE
    gapindex::get_connected()
    
  } else {
    
    cat("Not connected to Oracle database. Using cached tables.\n")
    
  }
}