# set the year for data finalization
this_year <- format(Sys.time(), "%Y")


# cruises for AI/GOA regions
cruises <- v_cruises0 %>%
  dplyr::filter(year == this_year & survey_definition_id %in% c(47, 52)) %>% 
  dplyr::select(year, region, cruise, cruise_id, vessel_id)

# haul information for this year's AI/GOA cruises
haul_data <- edit_hauls0 %>%
  dplyr::right_join(cruises, by = join_by(cruise_id)) %>%
  dplyr::left_join(edit_haul_measurements0, by = join_by(haul_id)) %>% 
  dplyr::mutate(duration = edit_duration_ob_fb * 60) %>%
  dplyr::select(haul_id, cruise_id, cruise, region, year, vessel_id, haul, station, stratum, haul_type, 
                performance, accessories, gear, duration) 

# catch information for this year's AI/GOA cruises
catch_data <- edit_catch_species0 %>%
  dplyr::left_join(edit_catch_samples0, by = join_by(catch_sample_id)) %>% 
  dplyr::select(haul_id, species_code, species_name = edit_species_name,
                total_weight_in_haul, total_number_in_haul) %>%
  dplyr::mutate(avg_specimen_weight = total_weight_in_haul/total_number_in_haul) %>%
  dplyr::group_by(haul_id) %>%
  dplyr::mutate(haul_weight = sum(total_weight_in_haul) / 1000) %>%
  dplyr::right_join(haul_data, by = join_by(haul_id))


# detailing haul issues (abundance_haul = "N")
abundance_haul_issues <- catch_data %>%
  dplyr::select(-c(species_code:total_number_in_haul)) %>%
  unique() %>%
  dplyr::group_by(station, stratum) %>%
  dplyr::add_count(name = "n_station") %>%
  dplyr::mutate(issue = case_when(
    accessories != 129 ~ "accessories not 129",
    gear != 172 ~ "gear not 172",
    haul_type != 3 ~ "haul type not 3",
    performance < 0 ~ "performance inadequate",
    duration < 10 & haul_weight < duration ~ "catch too low for tow duration",
    n_station > 1 & sum(haul_type == 3) > 1 & sum(performance >= 0) > 1 ~ "station duplicated",
    TRUE ~ "none"
  ) ) %>% 
  dplyr::select(cruise, vessel = vessel_id, haul:duration, haul_weight, issue) %>%
  dplyr::filter(issue != "none") %>% 
  dplyr::arrange(vessel, haul) 

  
  