## =============================================================================
## Download data sets to local machine
## =============================================================================

# RACEBASE tables to query
edit_data <- c(
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

all_tables <- c(edit_data, historical_data)

## =============================================================================
## Prepare local cache
## =============================================================================
current_year <- format(Sys.Date(), "%Y")

data_dir <- "data/oracle"
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

# Helper for consistent naming
format_name <- function(x) gsub("\\.", "-", x)

table_status <- purrr::map_dfr(all_tables, function(tbl) {
  
  file_name <- paste0(tolower(format_name(tbl)), ".csv")
  file_path <- file.path(data_dir, file_name)
  
  if (!file.exists(file_path)) {
    return(tibble::tibble(
      table = tbl,
      file = file_name,
      status = "missing"
    ))
  }
  
  file_year <- format(file.info(file_path)$mtime, "%Y")
  
  if (file_year != current_year) {
    return(tibble::tibble(
      table = tbl,
      file = file_name,
      status = "stale"
    ))
  }
  
  tibble::tibble(
    table = tbl,
    file = file_name,
    status = "current"
  )
})


tables_to_download <- table_status %>%
  dplyr::filter(
    table %in% edit_data | status != "current"
  ) %>%
  dplyr::pull(table)


## =============================================================================
## Download or use cached data
## =============================================================================

message("Downloading ", length(tables_to_download), " table(s):")
print(tables_to_download)
  
  if (!exists("channel")) {
    stop("Not connected to Oracle. Cannot download missing/stale tables.")
  }
  
  for (tbl in tables_to_download) {
    
    message("Downloading ", tbl)
    
    file_name <- paste0(tolower(format_name(tbl)), ".csv")
    file_path <- file.path(data_dir, file_name)
    
    query <- paste0("SELECT * FROM ", tbl)
    dat <- RODBC::sqlQuery(channel, query)
    
    readr::write_csv(dat, file_path)
    rm(dat)
  }
  
