## Download data sets to local machine -------------------------------------------------------

# RACEBASE tables to query
locations <- c(
  # biological edit data
  "RACE_DATA.EDIT_CATCH_SPECIES",
  "RACE_DATA.EDIT_CATCH_SAMPLES",
  "RACE_DATA.EDIT_LENGTHS",
  "RACE_DATA.EDIT_SPECIMENS",

  # effort edit data
  "RACE_DATA.EDIT_HAULS",
  "RACE_DATA.EDIT_EVENTS",
  "RACE_DATA.EDIT_HAUL_MEASUREMENTS"
)

historical_data <- c(
  "RACE_DATA.V_CRUISES",
  "RACE_DATA.RACE_SPECIES_CODES",
  "RACEBASE.CATCH",
  "RACEBASE.LENGTH",
  "RACEBASE.HAUL",
  "RACEBASE.SPECIMEN",
  "RACEBASE.CRUISE",
  "RACEBASE.CATCH"
)

if (!file.exists("data/oracle")) dir.create("data/oracle", recursive = TRUE)
files <- tools::file_path_sans_ext(toupper(list.files("data/oracle")))
if (!use_cached | !all(gsub("\\.", "-", historical_data) %in% files)) locations <- c(locations, historical_data)

if (exists("channel")) {
  # downloads tables in "locations"
  for (i in 1:length(locations)) {
    print(paste0("Downloading data from ", locations[i]))
    filename <- tolower(gsub("\\.", "-", locations[i]))
    a <- RODBC::sqlQuery(channel, paste0("SELECT * FROM ", locations[i]))
    readr::write_csv(
      x = a,
      here::here("data", "oracle", paste0(filename, ".csv"))
    )
    remove(a)
  }
} else {
  # reads downloaded tables into R environment
  if (!all(gsub("\\.", "-", locations) %in% files)) {
    cat("Not connected to Oracle database and can not locate proper tables in cache.
        Connect to Oracle and re-run script to proceed.\n")
    gapindex::get_connected()
  } else {
    cat("Not connected to Oracle database. Will use cached tables.\n")
  }
}
