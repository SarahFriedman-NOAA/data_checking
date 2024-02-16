# detailing haul issues (abundance_haul = "N")
abundance_haul_issues <- new_catch %>%
  dplyr::right_join(new_haul, by = join_by(haul_id)) %>%
  dplyr::group_by(haul_id) %>%
  dplyr::mutate(haul_weight = sum(total_weight) / 1000) %>%
  dplyr::select(cruise, region, vessel_id, haul:duration, haul_weight) %>%
  unique() %>%
  dplyr::group_by(cruise, region, station, stratum) %>%
  dplyr::add_count(name = "n_station") %>% 
  dplyr::mutate(issue = case_when(
    !accessories %in% c(15, 129) ~ "improper accessories",
    !gear %in% c(44, 172) ~ "improper gear",
    haul_type != 3 ~ "haul type not 3",
    performance < 0 ~ "performance inadequate",
    duration < 10 & haul_weight < duration ~ "catch too low for tow duration",
    n_station > 1 & sum(haul_type == 3) > 1 & sum(performance >= 0) > 1 ~ "station duplicated",
    TRUE ~ "none"
  )) %>%
  dplyr::select(-n_station) %>%
  dplyr::filter(issue != "none") %>%
  dplyr::arrange(region, cruise, vessel_id, haul)
