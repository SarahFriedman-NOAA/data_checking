# convert degree decimal minutes to decimal degrees
ddm_to_dd <- function(x, type) {
  if (type == "lat") {
    out <- tibble::tibble(ddm = x) %>%
      tidyr::separate(ddm, into = c("deg", "min"), sep = "(?<=[0-9]{2})", extra = "merge") %>%
      dplyr::mutate(
        deg = as.numeric(deg),
        min = as.numeric(min)
      ) %>%
      dplyr::mutate(dd = deg + (min / 60))
  }
  if (type == "long") {
    out <- tibble::tibble(ddm = x) %>%
      tidyr::separate(ddm, into = c("deg", "min"), sep = "(?<=[0-9]{3})", extra = "merge") %>%
      dplyr::mutate(
        deg = as.numeric(deg),
        min = as.numeric(min)
      ) %>%
      dplyr::mutate(dd = deg + (min / 60))
  }
  out$dd
}


# function to identify outliers in speices IDs for each year
check_outlier <- function(data, check_year, catch_data, plot = FALSE,
                          eps = 7, minPts = 2) {
  sp_catch <- catch_data %>%
    dplyr::filter(species_code == data$species_code[1])

  if (!any(sp_catch$year %in% check_year)) {
    sp_catch <- dplyr::bind_rows(sp_catch, data) %>%
      tidyr::fill(species_name) %>%
      dplyr::filter(!is.na(start_latitude))
  }

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


# GAM outlier check
gam_outliers <- function(data){
  if(nrow(data) > 1000) {
    mod <- mgcv::gam(weight ~ s(length, bs = "cs", fx = TRUE, k = 10), data = data)
  } else {
    suppressWarnings(mod <- stats::loess(weight ~ length, data = data))
  }
  abs(mod$residuals)
}


# borrowing ggplot theme from Sean's esrindex package
theme_blue_strip <- function() {
  theme_bw() %+replace%
    theme(axis.title = element_text(color = "black", face = "bold"),
          axis.text = element_text(color = "black", size = 9),
          axis.ticks = element_line(color = "black"),
          panel.grid = element_blank(),  
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          # legend.position = "bottom",
          strip.text = element_text(size = 10,
                                    color = "white",
                                    face = "bold",
                                    margin = margin(0.5, 0, 0.5, 0, "mm")),
          strip.background = element_rect(fill = "#0055a4",
                                          color = NA))
}
