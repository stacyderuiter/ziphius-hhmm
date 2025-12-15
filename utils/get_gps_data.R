get_gps_data <- function(nc_fname) {
  this_data <- tagtools::load_nc(nc_fname)
  these_locs <- data.frame(this_data$GPS_position$data)
  
  
  if (ncol(these_locs) < 3) {
    #   these_locs <- data.frame(this_data$GPS_position$sampling_rate,
    #                            these_locs[1,1],
    #                            these_locs[2,1])
    these_locs <- data.frame(t(this_data$GPS_position$data))
  }
  
  these_sats <- data.frame(this_data$GPS_satellites$data)
  if (ncol(these_sats) < 2) {
    # these_sats <- data.frame(this_data$GPS_satellites$sampling_rate,
    # these_sats)
    these_sats <- data.frame(t(this_data$GPS_satellites$data))
  }
  
  these_resids <- data.frame(this_data$GPS_residual$data)
  if (ncol(these_resids) < 2) {
    # these_resids <- data.frame(this_data$GPS_residual$sampling_rate,
    #                          these_resids)
    these_resids <- data.frame(t(this_data$GPS_residual$data))
  }
  these_timeerr <- data.frame(this_data$GPS_time_error$data)
  if (ncol(these_timeerr) < 2) {
    # these_timeerr <- data.frame(this_data$GPS_time_error$sampling_rate,
    #                            these_timeerr)
    these_timeerr <- data.frame(t(this_data$GPS_time_error$data))
  }
  
  names(these_locs) <- c('sec_since_tagon', 'latitude', 'longitude')
  names(these_sats) <- c('sec_since_tagon', 'satellites')
  names(these_resids) <- c('sec_since_tagon', 'residual')
  names(these_timeerr) <- c('sec_since_tagon', 'time_error')
  
  these_locs <-
    dplyr::left_join(these_locs, these_sats, by = 'sec_since_tagon')
  these_locs <-
    dplyr::left_join(these_locs, these_resids, by = 'sec_since_tagon')
  these_locs <-
    dplyr::left_join(these_locs, these_timeerr, by = 'sec_since_tagon')
  
  init <- data.frame(
    sec_since_tagon = 0,
    latitude = this_data$info$dephist_deploy_location_lat,
    longitude = this_data$info$dephist_deploy_location_lon)
  
  these_locs <- these_locs |>
    dplyr::filter(time_error < 3 &
                    time_error > -3 &
                    residual < 35 &
                    satellites >= 4)
  
  these_locs <- these_locs |>
    arrange(sec_since_tagon) |>
    filter(abs(latitude - lag(latitude)) < 0.5 & abs(latitude - lag(latitude,2)) < 0.5) |>
    filter(abs(longitude - lag(longitude)) < 0.5 & abs(longitude - lag(longitude,2)) < 0.5) |>
    dplyr::select(sec_since_tagon,
                  latitude,
                  longitude) |>
    dplyr::distinct() |>
    tidyr::drop_na(latitude, longitude)
  
  these_locs <- bind_rows(init, these_locs)
  
  return(these_locs)
}