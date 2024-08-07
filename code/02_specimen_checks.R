## Stats to detect outliers --------------------------------------------------

# length outliers
length_stats <- new_lengths %>%
  dplyr::right_join(new_haul, by = join_by(haul_id)) %>%
  dplyr::bind_rows(old_lengths) %>%
  dplyr::group_by(species_code) %>%
  dplyr::mutate(outlier = abs(length - median(length, na.rm = T)) > (4.5 * mad(length, na.rm = T)) & year == this_year) %>%
  dplyr::left_join(species_codes, by = join_by(species_code))

length_outliers <- length_stats %>%
  dplyr::filter(outlier & species_code != 21741) %>%
  dplyr::arrange(species_code) %>%
  dplyr::select(cruise, region,
    vessel = vessel_id, haul, species_name, common_name, species_code,
    length_mm = length, sex
  ) %>%
  dplyr::mutate(issue = "length")


# weight outliers
catch_stats <- new_catch %>%
  dplyr::right_join(new_haul, by = join_by(haul_id)) %>%
  dplyr::bind_rows(old_catch) %>%
  dplyr::group_by(species_code) %>%
  dplyr::filter(!is.na(avg_specimen_weight) & !species_code %in% c(21721, 21741)) %>%
  dplyr::add_count() %>%
  dplyr::filter(n > 20 | is.na(n)) %>% # filtering out species we don't have enough data on
  mutate(log_weight = log(avg_specimen_weight)) %>%
  dplyr::mutate(outlier = abs(log_weight - median(log_weight, na.rm = T) > (3 * mad(log_weight, na.rm = T))) & year == this_year | total_weight < 0 & year == this_year) %>%
  dplyr::left_join(species_codes, by = join_by(species_code))

catch_outliers <- catch_stats %>%
  dplyr::filter(outlier) %>%
  dplyr::select(
    cruise, region,
    vessel = vessel_id, haul, species_name, common_name, species_code,
    weight_kg = avg_specimen_weight, number_fish
  ) %>%
  dplyr::mutate(issue = "weight")



# length-weight outliers (specimen only)
specimen_stats <- new_lengths %>%
  dplyr::right_join(new_haul, by = join_by(haul_id)) %>%
  dplyr::bind_rows(old_lengths) %>%
  dplyr::filter(!is.na(weight) & !is.na(length)) %>%
  dplyr::mutate(weight = weight / 1000) %>%
  dplyr::group_by(species_code) %>%
  dplyr::add_count() %>%
  dplyr::filter(n > 10) %>% # filtering out species we don't have enough data on
  tidyr::nest() %>%
  dplyr::mutate(res = purrr::map(data, gam_outliers)) %>%
  tidyr::unnest(cols = c(data, res)) %>%
  dplyr::mutate(outlier = res > quantile(res, probs = .99) * 2 & year == this_year) %>%
  dplyr::left_join(species_codes, by = join_by(species_code))


specimen_outliers <- specimen_stats %>%
  dplyr::filter(outlier) %>%
  dplyr::select(
    cruise, region,
    vessel = vessel_id, haul, species_name,
    common_name, species_code, length_mm = length, weight_kg = weight, sex
  ) %>%
  dplyr::mutate(issue = "length-weight")



## Plot --------------------------------------------------
if (nrow(length_outliers) > 0) {
  pg <- ceiling(length(unique(length_outliers$species_code)) / 16)
  pdf(paste0(out_dir, "/length_outliers_", this_year, ".pdf"), width = 10, height = 10)
  for (i in 1:pg) {
    length_plot <- length_stats %>%
      dplyr::filter(year != this_year & species_code %in% length_outliers$species_code) %>%
      ggplot2::ggplot(aes(x = length)) +
      ggplot2::geom_density(linewidth = 0.7, col = "grey40", fill = "grey70", alpha = 0.6) +
      ggforce::facet_wrap_paginate(~species_name, scales = "free", ncol = 4, nrow = 4, page = i) +
      ggplot2::geom_vline(
        data = length_outliers, aes(xintercept = length_mm, col = region),
        linewidth = 0.8
      ) +
      xlab("length (mm)") +
      ylab("") +
      theme_blue_strip()
    print(length_plot)
  }
  dev.off()
}


if (nrow(catch_outliers) > 0) {
  pg <- ceiling(length(unique(catch_outliers$species_code)) / 16)
  pdf(paste0(out_dir, "/weight_outliers_", this_year, ".pdf"), width = 10, height = 10)
  for (i in 1:pg) {
    weight_plot <- catch_stats %>%
      dplyr::filter(year != this_year & species_code %in% catch_outliers$species_code) %>%
      ggplot2::ggplot(aes(x = avg_specimen_weight)) +
      ggplot2::geom_density(linewidth = 0.7, col = "grey40", fill = "grey70", alpha = 0.6) +
      ggforce::facet_wrap_paginate(~species_name, scales = "free", ncol = 4, nrow = 4, page = i) +
      ggplot2::geom_vline(
        data = catch_outliers, aes(xintercept = weight_kg, col = region),
        linewidth = 0.8
      ) +
      xlab("weight (kg)") +
      ylab("") +
      theme_blue_strip()
    print(weight_plot)
  }
  dev.off()
}


if (nrow(specimen_outliers) > 0) {
  pg <- ceiling(length(unique(specimen_outliers$species_code)) / 4)
  pdf(paste0(out_dir, "/specimen_outliers_", this_year, ".pdf"), width = 10, height = 10)
  for (i in 1:pg) {
    specimen_plot <- specimen_stats %>%
      dplyr::filter(year != this_year & species_code %in% specimen_outliers$species_code) %>%
      ggplot2::ggplot(aes(x = length, y = weight)) +
      ggforce::facet_wrap_paginate(~species_name, scales = "free", ncol = 2, nrow = 2, page = i) +
      ggplot2::geom_point(alpha = 0.4, col = "grey80") +
      ggplot2::geom_smooth(method = "gam", col = "black", se = FALSE, lwd = 1) +
      ggplot2::geom_point(
        data = specimen_outliers, aes(x = length_mm, y = weight_kg, col = region), size = 2
      ) +
      xlab("length (mm)") +
      ylab("weight (kg)") +
      theme_blue_strip()
    print(specimen_plot)
  }
  dev.off()
}

cat(paste0("Disgnostic plots written to output folder at:\n", here::here(), "/", out_dir))
