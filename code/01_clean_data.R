## =============================================================================
## Load Oracle tables into R
## =============================================================================

oracle_dir <- here::here("data", "oracle")

files <- list.files(
  path = oracle_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

for (file in files) {
  dat <- suppressWarnings(
    readr::read_csv(file, show_col_types = FALSE)
  ) %>%
    janitor::clean_names()
  
  # Remove accidental index column if present
  if ("x1" %in% names(dat)) {
    dat$x1 <- NULL
  }
  
  # Create object name (match original behavior)
  obj_name <- paste0(stringr::str_extract(basename(file), "[^-]*(?=\\.)"), "0")
  
  assign(obj_name, dat, envir = .GlobalEnv)
}


## =============================================================================
## Survey year + output setup
## =============================================================================

this_year <- as.numeric(format(Sys.Date(), "%Y"))

out_dir <- file.path("output", this_year)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)


## =============================================================================
## Cruise + species metadata
## =============================================================================

survey_def_ids <- c(
  "AI" = 52,
  "GOA" = 47,
  "GOA" = 39,
  "EBS" = 98,
  "BSS" = 78,
  "NBS" = 143
)

cruises <- v_cruises0 %>%
  filter(year >= 2000, survey_definition_id %in% survey_def_ids) %>%
  select(year, survey_definition_id, cruisejoin, region,
         cruise, cruise_id, vessel_id)

new_cruise <- cruises %>%
  filter(year == this_year)

species_codes <- race_species_codes0 %>%
  select(species_code, species_name, common_name)


## =============================================================================
## Historical data
## =============================================================================

old_haul <- haul0 %>%
  filter(abundance_haul == "Y") %>%
  right_join(cruises, by = join_by(cruisejoin, region, cruise)) %>%
  select(cruisejoin:haul, start_latitude, start_longitude,
         depth = bottom_depth) %>%
  filter(!cruise %in% new_cruise$cruise)


old_catch <- catch0 %>%
  right_join(cruises, by = join_by(cruisejoin, region, cruise)) %>%
  group_by(cruise, vessel, haul, species_code) %>%
  mutate(avg_specimen_weight = weight / number_fish) %>%
  select(
    cruise, vessel, haul, species_code,
    avg_specimen_weight,
    total_weight = weight,
    number_fish,
    year
  ) %>%
  filter(year != this_year)


old_lengths <- length0 %>%
  right_join(cruises, by = join_by(cruisejoin, region, cruise)) %>%
  select(species_code, sex, length, year) %>%
  bind_rows(
    specimen0 %>%
      right_join(cruises, by = join_by(cruisejoin, region, cruise)) %>%
      select(species_code, sex, length, year, weight)
  ) %>%
  filter(year != this_year)


## =============================================================================
## New (unfinalized) data
## =============================================================================

# Haul data
new_haul <- edit_events0 %>%
  filter(event_type_id == 4) %>%  # EQ position
  mutate(
    start_latitude  = ddm_to_dd(edit_latitude, "lat"),
    start_longitude = ddm_to_dd(edit_longitude, "long")
  ) %>%
  select(haul_id, contains("start")) %>%
  right_join(edit_hauls0, by = join_by(haul_id)) %>%
  right_join(new_cruise, by = join_by(cruise_id)) %>%
  left_join(edit_haul_measurements0, by = join_by(haul_id)) %>%
  mutate(duration = edit_duration_ob_fb * 60) %>%
  select(
    haul_id, cruise_id, cruise, region, year, vessel_id,
    haul, station, stratum, haul_type, performance,
    accessories, gear, duration,
    start_latitude, start_longitude,
    bottom_depth = edit_bottom_depth,
    gear_depth   = edit_gear_depth,
    net_height   = edit_net_height,
    net_height_method
  )


# Catch data
new_catch <- edit_catch_species0 %>%
  left_join(edit_catch_samples0, by = join_by(catch_sample_id)) %>%
  select(
    haul_id, species_code, voucher_number,
    total_weight = total_weight_in_haul,
    number_fish  = total_number_in_haul
  ) %>%
  group_by(haul_id, species_code) %>%
  mutate(avg_specimen_weight = total_weight / number_fish) %>%
  filter(haul_id %in% new_haul$haul_id)


# Length + specimen data
new_lengths <- edit_lengths0 %>%
  bind_rows(edit_specimens0) %>%
  filter(haul_id %in% new_haul$haul_id) %>%
  select(
    haul_id, species_code, specimen_number,
    sex,
    length = edit_length,
    weight = edit_weight
  )