process_gps <- function(datadir_gps){

  csvfnames_gps <- list.files(path = datadir_gps, pattern = "*.csv", recursive = TRUE, full.names = TRUE)

  #identify id number as beginning at the first position in each file name after the end of the provided file path
  ids_gps <- str_sub(csvfnames_gps, start = nchar(datadir_gps) + 2, end = nchar(csvfnames_gps[1])) %>%
    toupper() %>% #coerce to uppercase for string matching/replacement
    str_replace_all("_GPS.CSV", "") %>% #drop suffix after idno
    str_replace_all("_CRS.CSV", "") #alternative suffix (n = 1 in Rails and Health data)

  #create copy of bout data frame

  bout_data <- phase_2_bouts

  #loop through each gps file
  # for(i in 1:length(csvfnames_gps)){
  for(i in 1:length(csvfnames_gps)){

    #pull the idno for the record in question
    id <- ids_gps[i]

    #create empty vector of ids with no bouts
    id_no_bout <- NULL

    #only attempt to join gps to bout data if individuals had valid bouts
    if(id %in% phase_2_bouts$idno){

      gps_data_temp <- read_csv(csvfnames_gps[i]) %>%
        mutate("datetime" = ymd_hms(paste(`LOCAL DATE`, `LOCAL TIME`)))


      gps_data_temp$idno <- id

      bout_data_temp <- bout_data %>%
        filter(idno == id)

      #loop through bouts to id all gps points within a bout
      for(ii in 1:nrow(bout_data_temp)){

        period_start <- bout_data_temp$begin_datetime[ii]

        period_end <- bout_data_temp$end_datetime[ii]

        gps_points <- gps_data_temp %>%
          filter(datetime >= period_start & datetime <= period_end)

        gps_bout_join_temp <- gps_points %>%
          left_join(bout_data_temp[ii,], by = "idno")

        #if the end gps bout level data set has not been created yet, create it
        if(!exists("gps_bout_join")){

          gps_bout_join <- gps_bout_join_temp

        }
        #end if(!exists("gps_bout_join"))

        #if the end gps bout level data set has been created, bind rows
        if(exists("gps_bout_join")){

          gps_bout_join <- gps_bout_join %>%
            bind_rows(gps_bout_join_temp)

        }
        #end if(exists("gps_bout_join"))

      }
      #end for loop through individual's bouts


    }
    #end if idno exists in bout data


    #CONTINGENCY NEEDED FOR THOSE WITH NO VALID BOUTS?
    if(!(id %in% phase_2_bouts$idno)){

      id_no_bout <- c(id_no_bout, id)

    }
    #end if(!(id %in% phase_2_bouts$idno))

  }
  #end for loop through each csv

  assign("gps_bout_join", gps_bout_join, envir = .GlobalEnv)

}
#end function




####### ####### ####### ####### #######
#
# abstract GPS information
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
