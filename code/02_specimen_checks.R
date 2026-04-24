## =============================================================================
## 1. Length outliers
## =============================================================================

length_stats <- new_lengths %>%
  right_join(new_haul, by = join_by(haul_id)) %>%
  bind_rows(old_lengths) %>%
  mutate(length_mm = length) %>%   # <- FIX: standardize early
  group_by(species_code) %>%
  mutate(
    outlier = abs(length_mm - median(length_mm, na.rm = TRUE)) >
      (4.5 * mad(length_mm, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  left_join(species_codes, by = join_by(species_code))

length_outliers <- length_stats %>%
  filter(outlier, species_code != 21741, year == this_year) %>%
  arrange(species_code) %>%
  select(
    cruise, region,
    vessel = vessel_id,
    haul,
    species_name,
    common_name,
    species_code,
    length_mm,
    sex
  ) %>%
  mutate(issue = "length")


## =============================================================================
## 2. Catch weight outliers
## =============================================================================

catch_stats <- new_catch %>%
  right_join(new_haul, by = join_by(haul_id)) %>%
  bind_rows(old_catch) %>%
  rename(weight_kg = avg_specimen_weight) %>%
  filter(
    !is.na(weight_kg),
    weight_kg > 0, # Log(0) is undefined
    !species_code %in% c(21721, 21741)
  ) %>%
  group_by(species_code) %>%
  filter(n() > 20) %>%
  mutate(
    log_w = log(weight_kg),
    # Calculate Interquartile Range (IQR) on log scale
    Q1 = quantile(log_w, 0.25),
    Q3 = quantile(log_w, 0.75),
    IQR_log = Q3 - Q1,
    
    # Use a wider multiplier (3 instead of 1.5) for "Extreme Outliers"
    # Adding a small constant (0.1) prevents the fence from collapsing 
    # when all fish are nearly the same size.
    lower_fence = Q1 - (3.5 * IQR_log) - 0.1, 
    upper_fence = Q3 + (3 * IQR_log) + 0.1,
    
    is_extreme = log_w < lower_fence | log_w > upper_fence,
    
    outlier = (is_extreme & year == this_year) | 
      (total_weight < 0 & year == this_year)
  ) %>%
  ungroup() %>%
  left_join(species_codes, by = "species_code")


catch_outliers <- catch_stats %>%
  filter(outlier) %>%
  select(
    cruise, region,
    vessel = vessel_id,
    haul,
    species_name,
    common_name,
    species_code,
    weight_kg,
    number_fish
  ) %>%
  mutate(issue = "weight")



## =============================================================================
## 3. Length-weight specimen outliers
## =============================================================================

specimen_stats <- new_lengths %>%
  right_join(new_haul, by = join_by(haul_id)) %>%
  bind_rows(old_lengths) %>%
  filter(!is.na(weight), !is.na(length)) %>%
  mutate(weight = weight / 1000) %>%
  group_by(species_code) %>%
  add_count() %>%
  filter(n > 10) %>%
  tidyr::nest() %>%
  mutate(res = purrr::map(data, gam_outliers)) %>%
  tidyr::unnest(cols = c(data, res)) %>%
  mutate(
    outlier = res > quantile(res, probs = 0.99, na.rm = TRUE) * 2 &
      year == this_year
  ) %>%
  ungroup() %>%
  left_join(species_codes, by = join_by(species_code))

specimen_outliers <- specimen_stats %>%
  filter(outlier) %>%
  select(
    cruise, region,
    vessel = vessel_id,
    haul,
    species_name,
    common_name,
    species_code,
    length_mm = length,
    weight_kg = weight,
    sex
  ) %>%
  mutate(issue = "length-weight")


## =============================================================================
## Plot helper
## =============================================================================

plot_outlier_pdf <- function(outlier_df, stats_df, file_name,
                             x_var = NULL,
                             y_var = NULL,
                             ncol, nrow,
                             join_cols,
                             xlab_text,
                             ylab_text = "",
                             point_plot = FALSE) {
  
  if (nrow(outlier_df) == 0) return(NULL)
  
  new_outliers <- outlier_df %>%
    left_join(drive_version,
              relationship = "many-to-many",
              by = join_cols) %>%
    filter(!checked | is.na(checked))
  
  file_path <- file.path(out_dir, file_name)
  
  if (nrow(new_outliers) == 0) {
    if (file.exists(file_path)) file.remove(file_path)
    return(NULL)
  }
  
  pages <- ceiling(length(unique(new_outliers$species_code)) / (ncol * nrow))
  
  pdf(file_path, width = 10, height = 10)
  
  for (i in seq_len(pages)) {
    
    plot_data <- stats_df %>%
      filter(year != this_year,
             species_code %in% new_outliers$species_code)
    
    if (!point_plot) {
      
      p <- ggplot(plot_data, aes(x = .data[[x_var]])) +
        geom_density(
          linewidth = 0.7,
          col = "grey40",
          fill = "grey70",
          alpha = 0.6
        ) +
        ggforce::facet_wrap_paginate(
          ~species_name,
          scales = "free",
          ncol = ncol,
          nrow = nrow,
          page = i
        ) +
        geom_vline(
          data = new_outliers,
          aes(xintercept = .data[[x_var]], col = region),
          linewidth = 0.8
        ) +
        xlab(xlab_text) +
        ylab(ylab_text) +
        theme_blue_strip()
      
    } else {
      
      p <- ggplot(plot_data, aes(x = length, y = weight)) +
        ggforce::facet_wrap_paginate(
          ~species_name,
          scales = "free",
          ncol = ncol,
          nrow = nrow,
          page = i
        ) +
        geom_point(alpha = 0.4, col = "grey80") +
        geom_smooth(method = "gam", col = "black",
                    se = FALSE, linewidth = 1) +
        geom_point(
          data = new_outliers,
          aes(x = length_mm, y = weight_kg, col = region),
          size = 2
        ) +
        xlab(xlab_text) +
        ylab(ylab_text) +
        theme_blue_strip()
    }
    
    print(p)
  }
  
  dev.off()
}


## =============================================================================
## Generate diagnostic plots
## =============================================================================

plot_outlier_pdf(
  length_outliers,
  length_stats,
  paste0("length_outliers_", this_year, ".pdf"),
  x_var = "length_mm",
  ncol = 4,
  nrow = 4,
  join_cols = join_by(
    cruise, region, vessel, haul,
    species_name, common_name,
    species_code, length_mm, issue
  ),
  xlab_text = "length (mm)"
)

plot_outlier_pdf(
  catch_outliers,
  catch_stats,
  paste0("weight_outliers_", this_year, ".pdf"),
  x_var = "weight_kg",
  ncol = 4,
  nrow = 4,
  join_cols = join_by(
    cruise, region, vessel, haul,
    species_name, common_name,
    species_code, issue
  ),
  xlab_text = "weight (kg)"
)

plot_outlier_pdf(
  specimen_outliers,
  specimen_stats,
  paste0("specimen_outliers_", this_year, ".pdf"),
  x_var = NULL,
  ncol = 2,
  nrow = 2,
  join_cols = join_by(
    cruise, region, vessel, haul,
    species_name, common_name,
    species_code, length_mm, issue
  ),
  xlab_text = "length (mm)",
  ylab_text = "weight (kg)",
  point_plot = TRUE
)

cat(
  paste0(
    "Diagnostic plots written to output folder at:\n",
    here::here(), "/", out_dir, "\n"
  )
)