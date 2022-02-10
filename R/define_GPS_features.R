

####### ####### ####### ####### #######
#
# abstract GPS information
#
# function = fix_date_time
# function = generate_sum_by_epoch
# function = classify_accelerometry_activity
# function = summarize_epoch_activity
# function = identify_nonwearing_periods
# function = identify_pa_bouts
#
####### ####### ####### ####### #######


################ ################ ################ ################ ################ ################ ################ ################
#
# Identifying a dwell bout 
#
# 1) by calculating the sum of distances from each point to all other points within the bout
# 2) by selecting points having a sum distance below the 95th percentile of the sum distances of all points in the bout
# 3) by generating a minimum bounding circle fully containing the selected points, and finally 
# 4) by obtaining the circle’s radius. Bouts with radii <=66 ft were considered as dwell bouts. 
# Because some nondwell bouts with few GPS observations were likely to have radii <=66 ft, dwell bouts were defined as having >10 GPS points.
#
# refvalues$min_dwellbout_radii_ft: most likely area defined by a 66ft radii
# refvalues$min_dwellbout_obs: minimum definition of dwell bout defined by 10 consecutive records
#
################ ################ ################ ################ ################ ################ ################ ################


correction_for_cardinality <- function(gps_data, coordinate_field='LONGITUDE', cardinality_field='E.W'){
  #
  # Correctiong for all-positive values based on cardinality
  # If longitudinal values, E is positive, W is negative.
  # If latitude values, N is positive, S is negative.
  
  if(coordinate_field=='LONGITUDE'){
    gps_data <- gps_data %>%
      mutate('{coordinate_field}_coord' := case_when(gps_data[[cardinality_field]]=='E' ~ gps_data[[coordinate_field]],
                                                         gps_data[[cardinality_field]]=='W' ~ -1*gps_data[[coordinate_field]]))
  }
  
  if (coordinate_field=='LATITUDE'){
    gps_data <- gps_data %>%
      mutate('{coordinate_field}_coord' := case_when(gps_data[[cardinality_field]]=='N' ~ gps_data[[coordinate_field]],
                                                         gps_data[[cardinality_field]]=='S' ~ -1*gps_data[[coordinate_field]]))
  }
  return(gps_data)
}



# test for daylight savings offset
# LOOK FOR OPERATIONS TO CORRECT DAYLIGHT SAVINGS PROBLEM

# CONVERT THE LOCAL TIME TO UTC TIME (FOR ACCELEROMETRY FILES); 
# TAKE IT FROM THE TIME-ZONE
# THE MORNING THAT DAYLIGHT-SAVINGS OCCURS
correction_for_DST <- function(gps_data, UTC_datetime, time_zone="America/Los_Angeles"){
  #
  # Difference in UTC_datetime and LOCAL_datetime:
  #   for Seattle/Portland during Daylight Savings Time (spring forward) should be 8
  #   for Seattle/Portland after end of Daylight Savings Time (fall back) should be 7

  # depends on the year and switch date
  
}


convert_points_to_multipoint_polygon <- function(Point_df){
  #
  # convert two-column data.table into multipoint coordinates 
  #
  # Point_df (data.table): data.table containing two-columns for (longitude, latitude) or (x,y) values
  
  Points_mp <- Point_df %>% 
    as.matrix() %>% # - convert x and y columns to two-column matrix with n rows
    st_multipoint() # generate (x, y) coordinates
  return(Points_mp)
}


convert_bounding_circle <- function(Points_mp, gps_data, eachbout, longitude_field='LONGITUDE', latitude_field='LATITUDE'){
  #
  # generate bounding circle of most points in bout
  #
  # Points_mp (multipoint object): the multipoint object containing the list of coordinates along the bounding circle
  
  # generate the circle
  Points_boundc = Points_mp %>% st_minimum_bounding_circle(.)
  
  # bout sequence
  seq = gps_data %>% 
    filter(bout_label==eachbout & !is.na(.[[ longitude_field ]]) & !is.na(.[[ latitude_field ]]))
  
  # plot
  plot(Points_boundc, axes=TRUE) # circle
  plot(Points_mp, add=TRUE) # all points
  plot(Points_mp %>% head(1) %>% st_multipoint(), pch=13, col='green', add=TRUE) # start point
  plot(Points_mp %>% tail(1) %>% st_multipoint(), pch=19, col='red', add=TRUE) # end point
  lines(seq[[ longitude_field ]], seq[[ latitude_field ]], add=TRUE)
  
  #dev.off()
  return(Points_boundc)
}
