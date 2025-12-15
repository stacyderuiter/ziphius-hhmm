# Add GPS info to coarse scale data
# this is NOT AS COMPLICATED as for dive_acoustic_summary b/c it's at dive CYCLE scale not DIVE scale

add_gps_data <- function(nc_fname, divecycle_data) {
  these_dives <- divecycle_data
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
  
  these_locs <- these_locs |>
    dplyr::filter(time_error < 3 &
                    time_error > -3 &
                    residual < 35 &
                    satellites >= 4)
  
  these_locs <- these_locs |>
    dplyr::select(sec_since_tagon,
                  latitude,
                  longitude) |>
    dplyr::filter(sec_since_tagon < max(dplyr::pull(these_dives, end_sec) + 615, na.rm = TRUE)) |>
    tidyr::drop_na(latitude, longitude)
  
  # add in all locs DURING the dive
  # note: each event must match only one time period or interval-join fails.
  these_dives <- suppressWarnings(
    interval_join(
      x = these_dives,
      y = these_locs,
      start_x = start_sec,
      end_x = end_sec,
      start_y = sec_since_tagon,
      end_y = sec_since_tagon
    )
  )
  
  these_dives <- these_dives |>
    dplyr::group_by_all() |>
    dplyr::ungroup(sec_since_tagon, latitude, longitude) |>
    dplyr::arrange(sec_since_tagon) |>
    dplyr::summarise(
      lat_initial = dplyr::first(latitude),
      lon_initial = dplyr::first(longitude),
      posn_initial_time = dplyr::first(sec_since_tagon),
      lat_final = dplyr::last(latitude),
      lon_final = dplyr::last(longitude),
      posn_final_time = dplyr::last(sec_since_tagon),
      lat_all = paste(latitude, collapse = ', '),
      lon_all = paste(longitude, collapse = ', '),
      posn_all_time = paste(sec_since_tagon, collapse = ', '),
      .groups = 'drop'
    ) |>
    dplyr::ungroup()
  
  # fill in tagon location as first position
  if (is.na(these_dives[1, 'posn_all_time'])){
    these_dives[1, 'posn_all_time'] <- '0'
    these_dives[1, 'lat_initial'] <- this_data$info$dephist_deploy_location_lat
    these_dives[1, 'lon_initial'] <- this_data$info$dephist_deploy_location_lon
    these_dives[1, 'lat_all'] <- this_data$info$dephist_deploy_location_lat
    these_dives[1, 'lon_all'] <- this_data$info$dephist_deploy_location_lon
  }else{
    these_dives[1, 'posn_all_time'] <- paste('0', these_dives[1, 'posn_all_time'], collapse = ', ')
    these_dives[1, 'lat_all'] <- paste(this_data$info$dephist_deploy_location_lat, these_dives[1, 'lat_all'], collapse = ', ')
    these_dives[1, 'lon_all'] <- paste(this_data$info$dephist_deploy_location_lon, these_dives[1, 'lon_all'], collapse = ', ')
  }
  
  return(these_dives)
}