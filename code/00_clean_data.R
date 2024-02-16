## Load Oracle tables in R --------------------------------------------------
a <- list.files(
  path = here::here("data", "oracle"),
  pattern = "\\.csv"
)

for (i in 1:length(a)) {
  b <- readr::read_csv(file = here::here("data", "oracle", a[i]))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1") {
    b$x1 <- NULL
  }
  assign(x = paste0(str_extract(a[i], "[^-]*(?=\\.)"), "0"), value = b)
  rm(b)
}



## Load GAP cruise information --------------------------------------------------

# this_year <- as.numeric(format(Sys.Date(), "%Y"))
this_year <- 2023

# get all catch and taxonomy info together and filtering to just groundfish surveys after 2000
survey_def_ids <- c(
  "AI" = 52, "GOA" = 47, "EBS" = 98,
  "BSS" = 78, "NBS" = 143
)

cruises <- v_cruises0 %>%
  dplyr::filter(year >= 2000 & survey_definition_id %in% survey_def_ids) %>%
  dplyr::select(year, survey_definition_id, cruisejoin, region, cruise, cruise_id, vessel_id)
new_cruise <- cruises %>%
  dplyr::filter(year == this_year)

species_codes <- race_species_codes0 %>%
  dplyr::select(species_code, species_name, common_name)




## Load/clean historical data --------------------------------------------------

# historical effort/biological data
old_haul <- haul0 %>%
  dplyr::filter(abundance_haul == "Y") %>%
  dplyr::right_join(cruises, by = join_by(cruisejoin, region, cruise)) %>%
  dplyr::select(cruisejoin:haul, start_latitude, start_longitude, depth = bottom_depth) %>%
  filter(!cruise %in% new_cruise$cruise)

old_catch <- catch0 %>%
  dplyr::right_join(cruises, by = join_by(cruisejoin, region, cruise)) %>%
  dplyr::group_by(cruise, vessel, haul, species_code) %>%
  dplyr::mutate(avg_specimen_weight = weight / number_fish) %>%
  dplyr::select(cruise, vessel, haul, species_code, avg_specimen_weight, total_weight = weight, number_fish, year) %>%
  dplyr::filter(year != this_year)


old_lengths <- length0 %>%
  dplyr::right_join(cruises, by = join_by(cruisejoin, region, cruise)) %>%
  dplyr::select(species_code, sex, length, year) %>%
  dplyr::bind_rows(specimen0 %>%
    dplyr::right_join(cruises, by = join_by(cruisejoin, region, cruise)) %>%
    dplyr::select(species_code, sex, length, year, weight)) %>%
  dplyr::filter(year != this_year)





## Load/clean new (un-finalized) data --------------------------------------------------

# compiling haul data and lat/lon
new_haul <- edit_events0 %>%
  dplyr::filter(event_type_id == 4) %>% # taking lat/lon at EQ for each haul
  dplyr::mutate(
    start_latitude = ddm_to_dd(edit_latitude, "lat"), # converting ddm to dd
    start_longitude = ddm_to_dd(edit_longitude, "long")
  ) %>%
  dplyr::select(haul_id, contains("start")) %>%
  dplyr::right_join(edit_hauls0) %>%
  dplyr::right_join(new_cruise) %>%
  dplyr::left_join(edit_haul_measurements0, by = join_by(haul_id)) %>%
  dplyr::mutate(duration = edit_duration_ob_fb * 60) %>%
  dplyr::select(
    haul_id, cruise_id, cruise, region, year, vessel_id, haul, station, stratum, haul_type,
    performance, accessories, gear, duration, start_latitude, start_longitude,
    depth = edit_bottom_depth
  )


new_catch <- edit_catch_species0 %>%
  dplyr::left_join(edit_catch_samples0, by = join_by(catch_sample_id)) %>%
  dplyr::select(haul_id, species_code, voucher_number,
    total_weight = total_weight_in_haul, number_fish = total_number_in_haul
  ) %>%
  dplyr::group_by(haul_id, species_code) %>%
  dplyr::mutate(avg_specimen_weight = total_weight / number_fish) %>%
  dplyr::filter(haul_id %in% new_haul$haul_id)


new_lengths <- edit_lengths0 %>%
  dplyr::bind_rows(edit_specimens0) %>%
  dplyr::filter(haul_id %in% new_haul$haul_id) %>%
  dplyr::select(haul_id, species_code, specimen_number, sex, length = edit_length, weight = edit_weight)
