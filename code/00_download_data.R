## Download data sets to local machine -------------------------------------------------------

# RACEBASE tables to query
locations <- c(
  "RACE_DATA.V_CRUISES",
  "RACE_DATA.EDIT_CATCH_SPECIES",
  "RACE_DATA.EDIT_CATCH_SAMPLES",
  "RACE_DATA.EDIT_HAULS",
  "RACE_DATA.EDIT_HAUL_MEASUREMENTS",
  "RACE_DATA.EDIT_LENGTHS",
  "RACE_DATA.RACE_SPECIES_CODES",
  
  "RACEBASE.CATCH",
  "RACE_DATA.V_EXTRACT_FINAL_LENGTHS"
)

if (!file.exists("data/oracle")) dir.create("data/oracle", recursive = TRUE)


# downloads tables in "locations"
for (i in 1:length(locations)) {
  print(locations[i])
  filename <- tolower(gsub("\\.", "-", locations[i]))
  a <- RODBC::sqlQuery(channel, paste0("SELECT * FROM ", locations[i]))
  write_csv(
    x = a,
    here("data", "oracle", paste0(filename, ".csv"))
  )
  remove(a)
}



# reads downloaded tables into R environment
a <- list.files(
  path = here::here("data", "oracle"),
  pattern = "\\.csv"
)

for (i in 1:length(a)) {
  b <- read_csv(file = here::here("data", "oracle", a[i]))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  assign(x = paste0(str_extract(a[i], "[^-]*(?=\\.)"), "0"), value = b)
  rm(b)
}

