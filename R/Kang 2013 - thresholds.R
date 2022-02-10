
# Abstraction of metadata and threshold criteria from Kang et al. 2013
# Information was sourced from the Methods section, pg 1420 and 1421
#

# initialize reference object
refvalues <- list()

# standard 30-sec epoch definition
refvalues$epoch_interval_s <- 30

# day-level initial screening criteria
refvalues$min_GPS_data_day <- 5
refvalues$min_traveldiary_day <- 6
refvalues$min_accel_day <- 6

# minimum complete days of data (accelerometry)
refvalues$min_accel_wearing_hr <- 8
refvalues$min_conseczero_m <- 20

# minimum walking bout (accelerometry)
refvalues$min_walking_bout_m <- 5

# minimum physical activity bout (accelerometry)
# need definition of lower-intensity PA and moderate-intensity PA
refvalues$min_pa_cpe <- 500 # defined as "slow walking" or light-intensity PA with avg speed of 3 km/hr
refvalues$max_light_moderate_pa_cpe <- 2863 # >2863 cpe would be non-walking. 2874 cpe? 3360 cpe?
refvalues$min_pa_window_m <- 7
refvalues$max_pa_break_m <- 2

# GPS data criteria
refvalues$min_gps_coverage_ratio <- .2
refvalues$min_gps_consec_obs <- 5
refvalues$min_gps_window_m <- 2.5
refvalues$min_gps_walking_speed_km_h <- 2
refvalues$max_gps_walking_speed_km_h <- 6


# GPS speeds
  # walking bout
  # non-walking bout
    # dwell algorithm (location relationship)
    #1) calculating the sum of distances from each point to all other points within the bout,
    #2) by selecting points having a sum distance below the 95th percentile
    # of the sum distances of all points in the bout, 
    #3) by generating a minimum bounding circle fully containing the selected points, and 
    #4) by obtaining the circle’s radius. Bouts with radii <=66 ft were considered as dwell bouts

# dwell radii
refvalues$max_dwellbout_radii_ft <- 66
refvalues$min_dwellbout_obs <- 10

refvalues$time_zone="America/Los_Angeles"

    # conversion to seconds, if possible
    refvalues_s <- list()
    refvalues_s$epoch_interval_s <- refvalues$epoch_interval_s
    
    refvalues_s$min_GPS_data_s <- refvalues$min_GPS_data_day*24*60*60
    refvalues_s$min_traveldiary_s <- refvalues$min_traveldiary_day*24*60*60
    refvalues_s$min_accel_s <- refvalues$min_accel_day*24*60*60
    
    refvalues_s$min_accel_wearing_s <- refvalues$min_accel_wearing_hr*60*60
    refvalues_s$min_conseczero_s <- refvalues$min_conseczero_m*60
    refvalues_s$min_walking_bout_s <- refvalues$min_walking_bout_m*60
    
    refvalues_s$min_pa_cpe <- refvalues$min_pa_cpe
    refvalues_s$max_light_moderate_pa_cpe <- refvalues$max_light_moderate_pa_cpe 
    refvalues_s$min_pa_window_s <- refvalues$min_pa_window_m*60
    refvalues_s$max_pa_break_s <- refvalues$max_pa_break_m*60

    refvalues_s$min_gps_coverage_ratio <- refvalues$min_gps_coverage_ratio
    refvalues_s$min_gps_consec_obs <- refvalues$min_gps_consec_obs
    refvalues_s$min_gps_window_s <- refvalues$min_gps_window_m*60

    refvalues_s$min_dwellbout_radii_meters <- refvalues$min_dwellbout_radii_ft*3.281 ##?? Meters?
    refvalues_s$max_dwellbout_radii_ft <- refvalues$max_dwellbout_radii_ft
    refvalues_s$min_dwellbout_obs <- refvalues$min_dwellbout_obs
    
    refvalues_s$min_gps_walking_speed_km_h <- 2
    refvalues_s$max_gps_walking_speed_km_h <- 6
    
    refvalues_s$time_zone="America/Los_Angeles"
