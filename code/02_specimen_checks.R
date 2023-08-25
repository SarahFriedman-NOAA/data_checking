# only data from the GAP bottom trawl surveys
survey_def_ids <- c(
  "AI" = 52, "GOA" = 47, "EBS" = 98,
  "BSS" = 78, "NBS" = 143
)


# compiling historical length/weight data from racebase
old_catch <- catch0 %>%
  janitor::clean_names() %>%
  dplyr::left_join(v_cruises0, by = join_by(cruisejoin, region, cruise)) %>%
  dplyr::filter(year > 2000 & survey_definition_id %in% survey_def_ids) %>%
  dplyr::select(species_code, total_weight = weight, number_fish, year) %>%
  dplyr::mutate(avg_specimen_weight = total_weight / number_fish) %>%
  dplyr::group_by(species_code) %>%
  dplyr::add_count()

old_lengths <- v_extract_final_lengths0 %>%
  dplyr::left_join(v_cruises0, by = join_by(region, cruise),
                   relationship = "many-to-many") %>%
  dplyr::filter(year > 2000 & survey_definition_id %in% survey_def_ids) %>%
  dplyr::select(species_code, sex, length, year)



# length and catch data collected this year
new_lengths <- edit_lengths0 %>%
  dplyr::right_join(haul_data, by = join_by(haul_id)) %>%
  dplyr::select(species_code, sex, length = edit_length, year, haul_id)

new_catch <- catch_data %>%
  dplyr::ungroup() %>%
  dplyr::select(species_code, avg_specimen_weight)



## Stats to detect outliers --------------------------------------------------
species <- race_species_codes0 %>%
  dplyr::select(species_code, species_name, common_name)

# species-specific stats for outliers
length_stats <- old_lengths %>%
  dplyr::bind_rows(new_lengths) %>%
  dplyr::group_by(species_code) %>%
  dplyr::mutate(outlier = abs(length - median(length)) > 4 * mad(length) & year == this_year) %>%
  dplyr::left_join(species, by = join_by(species_code))

length_outliers <- length_stats %>%
  dplyr::filter(outlier) %>%
  dplyr::arrange(species_code) %>%
  dplyr::left_join(haul_data, by = join_by(year, haul_id))




catch_stats <- catch_data %>%
  dplyr::ungroup() %>%
  dplyr::select(species_code,
    total_weight = total_weight_in_haul,
    number_fish = total_number_in_haul, year, avg_specimen_weight,
    cruise, cruise_id, vessel_id, haul, haul_id
  ) %>%
  dplyr::bind_rows(old_catch) %>%
  dplyr::filter(n > 10 | is.na(n)) %>% #filtering out species we don't have enough data on
  dplyr::group_by(species_code) %>%
  dplyr::mutate(outlier = abs(avg_specimen_weight - median(avg_specimen_weight)) > 4 * mad(avg_specimen_weight) & year == this_year) %>%
  dplyr::left_join(species, by = join_by(species_code))


catch_outliers <- catch_stats %>%
  dplyr::filter(outlier)



# writing results to csv
if (!file.exists("output/")) dir.create("output/", recursive = TRUE)

length_outliers %>%
  select(cruise, vessel_id, haul, species_name, common_name, species_code, length) %>%
  arrange(vessel_id, haul) %>%
  write_csv(paste0("output/length_outliers_", this_year, ".csv"))

catch_outliers %>%
  select(cruise, vessel_id, haul, species_name, common_name, species_code, avg_specimen_weight, total_weight, number_fish) %>%
  arrange(vessel_id, haul) %>%
  write_csv(paste0("output/catch_outliers_", this_year, ".csv"))



## Plot --------------------------------------------------

length_plot <- length_stats %>%
  dplyr::filter(year != this_year & species_code %in% length_outliers$species_code) %>%
  ggplot2::ggplot(aes(x = length)) +
  ggplot2::geom_density(linewidth = 0.7, col = "grey40", fill = "grey70", alpha = 0.6) +
  ggplot2::facet_wrap(~species_name, scales = "free") +
  ggplot2::theme_classic() +
  ggplot2::theme(strip.background = element_blank()) +
  ggplot2::geom_vline(
    data = length_outliers, aes(xintercept = length),
    col = "salmon", linewidth = 0.7
  )

length_plot
ggsave(paste0("output/length_outliers_", this_year, ".pdf"),  width = 10, height = 20)


weight_plot <- catch_stats %>%
  dplyr::filter(year != this_year & species_code %in% catch_outliers$species_code) %>%
  ggplot2::ggplot(aes(x = avg_specimen_weight)) +
  ggplot2::geom_density(linewidth = 0.7, col = "grey40", fill = "grey70", alpha = 0.6) +
  ggplot2::facet_wrap(~species_name, scales = "free") +
  ggplot2::theme_classic() +
  ggplot2::theme(strip.background = element_blank()) +
  ggplot2::geom_vline(
    data = catch_outliers, aes(xintercept = avg_specimen_weight),
    col = "salmon", linewidth = 0.7
  )

weight_plot
ggsave(paste0("output/weight_outliers_", this_year, ".pdf"), width = 10, height = 10)



