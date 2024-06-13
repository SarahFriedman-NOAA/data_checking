# non species indicator strings
rm_bits <- paste0(
  c(
    "egg", "egg case", "larva", "larvae", "tubes", "sp\\.$", "empty ",
    "\\(juvenile\\)", "unid\\."
  ),
  collapse = "| "
)


# Getting species from year ----------------------------------------------------

# filtering data to only species-level info (no higher taxonomic levels)
new_records <- new_catch %>%
  dplyr::left_join(new_haul, by = join_by(haul_id)) %>%
  dplyr::ungroup() %>%
  unique() %>%
  dplyr::left_join(species_codes, by = join_by(species_code)) %>%
  dplyr::mutate(
    species_name = trimws(species_name),
    level = case_when(
      stringr::str_detect(species_name, rm_bits) |
        stringr::str_detect(common_name, "morphotype") |
        !stringr::str_detect(species_name, " ") |
        is.na(species_name) ~ "",
      TRUE ~ "species"
    )
  ) %>%
  dplyr::filter(level == "species") %>%
  dplyr::select(-level) %>%
  dplyr::select(
    species_code, species_name, common_name, start_longitude,
    start_latitude, depth, voucher = voucher_number, year,
    cruise, region, vessel = vessel_id, haul
  )



# Finding outliers ----------------------------------------------------

# checking for species that were caught this year that are suspicious/need manual checking using DBSCAN/past confirmed records

# all catch/haul data to check against
racebase_records <- old_catch %>%
  dplyr::filter(species_code %in% new_records$species_code) %>%
  dplyr::left_join(old_haul, by = join_by(cruise, vessel, haul)) %>%
  dplyr::left_join(species_codes, by = join_by(species_code)) %>%
  dplyr::select(
    species_code, species_name, common_name, start_longitude,
    start_latitude, year, depth, cruise, region, vessel, haul
  )


# outlier species from this year
outlier_spp <- new_records %>%
  dplyr::group_by(species_name) %>%
  tidyr::nest() %>%
  dplyr::mutate(outlier = purrr::map(data, ~check_outlier(.x, this_year, racebase_records))) %>%
  tidyr::unnest(cols = outlier) %>%
  dplyr::left_join(species_codes, by = join_by(species_name, species_code, common_name)) %>%
  dplyr::select(-data)




# plots outliers to pdf document
pdf(paste0(out_dir, "/range_outliers_", this_year, ".pdf"))
new_records %>%
  dplyr::filter(species_name %in% outlier_spp$species_name) %>%
  dplyr::group_by(species_name) %>%
  tidyr::nest() %>%
  dplyr::mutate(outlier = purrr::map(data, ~check_outlier(.x, this_year, racebase_records, plot = T)))
dev.off()


# associated data frame of outlier specimens
outlier_df <- outlier_spp %>%
  dplyr::mutate(vouchered = ifelse(!is.na(voucher), "yes", "")) %>%
  dplyr::select(
    species_name, common_name, species_code, region, year, cruise,
    vessel, haul, vouchered
  ) %>%
  dplyr::arrange(region, species_name) %>%
  dplyr::mutate(issue = "range")
