## =============================================================================
## 1. Setup configuration
## =============================================================================

# Regex to remove non-species labels
rm_bits <- c(
  "egg", "egg case", "larva", "larvae", "tubes",
  "sp\\.$", "empty", "\\(juvenile\\)", "unid\\."
) %>%
  paste(collapse = "|")


## =============================================================================
## 2. Process new records
## =============================================================================

new_records <- new_catch %>%
  left_join(new_haul, by = "haul_id") %>%
  distinct() %>%
  left_join(species_codes, by = "species_code") %>%
  mutate(
    species_name = trimws(species_name),
    is_species =
      !str_detect(species_name, rm_bits) &
      !str_detect(common_name, "morphotype") &
      str_detect(species_name, " ") &
      !is.na(species_name)
  ) %>%
  filter(is_species) %>%
  select(
    species_code, species_name, common_name,
    start_longitude, start_latitude,
    voucher = voucher_number,
    year, cruise, region,
    vessel = vessel_id, haul
  )


## =============================================================================
## 3. Prepare historical reference data
## =============================================================================

target_species <- unique(new_records$species_code)

racebase_records <- old_catch %>%
  filter(species_code %in% target_species) %>%
  left_join(old_haul, by = join_by(cruise, vessel, haul)) %>%
  left_join(species_codes, by = join_by(species_code)) %>%
  select(
    species_code, species_name, common_name,
    start_longitude, start_latitude,
    year, depth,
    cruise, region, vessel, haul
  )


## =============================================================================
## 4. Outlier detection
## =============================================================================

outlier_spp <- new_records %>%
  group_by(species_name) %>%
  nest() %>%
  mutate(
    outlier = purrr::map(
      data,
      ~ check_outlier(.x, this_year, racebase_records)
    )
  ) %>%
  unnest(cols = outlier) %>%
  left_join(
    species_codes,
    by = join_by(species_name, species_code, common_name)
  ) %>%
  select(-data)


## =============================================================================
## 5. Reporting & PDF generation
## =============================================================================

# Pre-filter to only species with outliers
outlier_species <- unique(outlier_spp$species_name)

new_outliers <- new_records %>%
  filter(species_name %in% outlier_species) %>%
  left_join(
    drive_version,
    by = join_by(
      species_code, species_name, common_name,
      cruise, region, vessel, haul
    )
  ) %>%
  filter(!checked | is.na(checked))

pdf_path <- file.path(out_dir, paste0("range_outliers_", this_year, ".pdf"))

if (nrow(new_outliers) > 0) {
  
  pdf(pdf_path)
  
  new_outliers %>%
    group_by(species_name) %>%
    group_walk(~ check_outlier(.x, this_year, racebase_records, plot = TRUE))
  
  dev.off()
  
} else if (file.exists(pdf_path)) {
  
  file.remove(pdf_path)
}


## =============================================================================
## 6. Final data export
## =============================================================================

outlier_df <- outlier_spp %>%
  mutate(
    vouchered = if_else(!is.na(voucher), "yes", "")
  ) %>%
  select(
    species_name, common_name, species_code,
    region, year, cruise, vessel, haul,
    vouchered
  ) %>%
  arrange(region, species_name) %>%
  mutate(issue = "range")