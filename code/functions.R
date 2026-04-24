## =============================================================================
## Utility functions for survey data processing and outlier detection
## =============================================================================


#' Convert Degree Decimal Minutes (DDM) to Decimal Degrees (DD)
#'
#' Converts coordinates in degree decimal minutes format (DDM) to
#' decimal degrees (DD). Handles both latitude and longitude formats.
#'
#' @param x Character vector of coordinates in DDM format (e.g., "5830.25")
#' @param type Character string specifying coordinate type: "lat" or "long"
#'
#' @return Numeric vector of decimal degree coordinates
#' @examples
#' ddm_to_dd("5830.25", "lat")
#'
#' @export
ddm_to_dd <- function(x, type) {
  stopifnot(type %in% c("lat", "long"))
  
  # Define split pattern based on coordinate type
  split_pattern <- if (type == "lat") {
    "(?<=[0-9]{2})"
  } else {
    "(?<=[0-9]{3})"
  }
  
  tibble::tibble(ddm = x) %>%
    tidyr::separate(ddm, into = c("deg", "min"),
                    sep = split_pattern, extra = "merge") %>%
    mutate(
      deg = as.numeric(deg),
      min = as.numeric(min),
      dd  = deg + (min / 60)
    ) %>%
    pull(dd)
}


#' Identify spatial outliers for a species using DBSCAN clustering
#'
#' This function compares spatial locations of a focal species in a given
#' year against historical data and flags outliers based on clustering.
#'
#' @param data Data frame of focal observations (must include species_code, year, lat/long)
#' @param check_year Numeric or integer vector of years to evaluate
#' @param catch_data Historical catch dataset
#' @param plot Logical; if TRUE, generates a map of observations and outliers
#' @param eps Numeric; DBSCAN epsilon parameter (neighborhood size)
#' @param minPts Integer; minimum points for DBSCAN clustering
#'
#' @return Data frame of flagged outliers (or NA if no data)
#'
#' @details
#' Uses \code{dbscan::dbscan()} to identify spatial clusters. Points not assigned
#' to a cluster (cluster == 0) are flagged as outliers.
#'
#' @export
check_outlier <- function(data, check_year, catch_data,
                          plot = FALSE, eps = 7, minPts = 2) {
  
  sp_catch <- catch_data %>%
    dplyr::filter(species_code == data$species_code[1]) %>%
    dplyr::bind_rows(., data) %>%
    tidyr::fill(species_name) %>%
    dplyr::filter(!is.na(start_latitude))
  
  
  # if (!any(sp_catch$year %in% check_year)) {
  #   sp_catch <- dplyr::bind_rows(sp_catch, data) %>%
  #     tidyr::fill(species_name) 
  # }
  
  if (nrow(sp_catch) > 0) {
    # clustering <- dbscan(sp_catch[,c("start_longitude", "start_latitude")],
    #                      eps = 3, minPts = 2, borderPoints = FALSE)
    clustering <- dbscan::dbscan(sp_catch[, c("start_longitude", "start_latitude")],
                                 eps = eps, minPts = minPts, borderPoints = FALSE
    )
    
    # flag anything that is only observed from check year
    if (length(check_year) == 1 & all(sp_catch$year %in% check_year)) {
      tmp <- sp_catch %>%
        dplyr::mutate(
          cluster = 0,
          outlier = "flag"
        )
    } else {
      tmp <- sp_catch %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          cluster = clustering$cluster,
          outlier = ifelse(cluster == 0, "flag", "")
        ) %>%
        dplyr::filter(year %in% check_year)
    }
    
    # vector of outliers to plot as red
    o <- tmp[tmp$outlier != "", ]
    
    # data frame to return
    out <- tmp %>%
      dplyr::select(-cluster) %>%
      dplyr::filter(outlier != "") %>%
      dplyr::select(-species_name, -outlier)
    
    # if( nrow(o) == nrow(sp_catch) ) plot <- F
    
    if (plot & length(o) > 0) {
      world <- ggplot2::map_data("world2", wrap = c(40, 400)) %>%
        dplyr::filter(region %in% c("Russia", "USA", "Canada"))
      ss <- sp_catch$species_name[1]
      if(is.na(ss)) ss <- species_codes$species_name[species_codes$species_code == sp_catch$species_code[1]]
      sp <- paste0(ss, " (", sp_catch$species_code[1], ")")
      
      p <- ggplot2::ggplot() +
        ggplot2::geom_polygon(
          data = world, aes(x = long, y = lat, group = group),
          col = "grey60", fill = "grey90", lwd = 0
        ) +
        ggplot2::coord_map(ylim = c(45, 70), xlim = c(150, 250)) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Longitude", y = "Latitude") +
        ggplot2::geom_point(
          data = sp_catch,
          aes(x = start_longitude, y = start_latitude), cex = 1
        ) +
        ggplot2::geom_point(
          data = o, aes(x = start_longitude, y = start_latitude),
          col = "red", cex = 1.5
        ) +
        # geom_text_repel(data = o, aes(x = start_longitude, y = start_latitude,
        #                               label = year)) +
        # ggtitle(label = sp, subtitle = sp_catch$common_name[1])
        ggplot2::ggtitle(
          label = sp,
          subtitle = ifelse(!is.na(out$voucher), "vouchered", "")
        )
      print(p)
    }
  } else {
    out <- NA
  }
  
  return(out)
}


#' Compute residual-based outlier scores using GAM or LOESS
#'
#' Fits a smoothing model (GAM or LOESS) to length-weight data and returns
#' absolute residuals as an outlier metric.
#'
#' @param data Data frame containing `length` and `weight` columns
#'
#' @return Numeric vector of absolute residuals
#'
#' @details
#' Uses GAM for large datasets (>1000 rows) and LOESS for smaller datasets.
#'
#' @export
gam_outliers <- function(data) {
  if (nrow(data) > 1000) {
    model <- mgcv::gam(weight ~ s(length, bs = "cs", fx = TRUE, k = 10), data = data)
  } else {
    model <- suppressWarnings(
      stats::loess(weight ~ length, data = data)
    )
  }
  
  abs(model$residuals)
}


#' Custom ggplot theme with blue facet strips
#'
#' A modified version of \code{theme_bw()} with styled facet strips and
#' simplified grid/legend formatting.
#'
#' @return A ggplot2 theme object
#'
#' @export
theme_blue_strip <- function() {
  theme_bw() %+replace%
    theme(
      axis.title = element_text(color = "black", face = "bold"),
      axis.text  = element_text(color = "black", size = 9),
      axis.ticks = element_line(color = "black"),
      panel.grid = element_blank(),
      legend.title = element_blank(),
      legend.text  = element_text(size = 11),
      strip.text = element_text(
        size = 10,
        color = "white",
        face = "bold",
        margin = margin(0.5, 0, 0.5, 0, "mm")
      ),
      strip.background = element_rect(
        fill = "#0055a4",
        color = NA
      )
    )
}


#' Google Sheets checkbox validation rule
#'
#' Creates a reusable checkbox validation rule for use with
#' \code{googlesheets4::range_add_validation()}.
#'
#' @return A DataValidationRule object
#'
#' @export
rule_checkbox <- googlesheets4:::new(
  "DataValidationRule",
  condition = googlesheets4:::new_BooleanCondition(type = "BOOLEAN"),
  inputMessage = "Lorem ipsum dolor sit amet",
  strict = TRUE,
  showCustomUi = TRUE
)