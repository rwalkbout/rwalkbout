# this pipeline is for processing one subject's data only
process_one_subject <- function (acc_file_path, gps_file_path, time_zone=NULL, acc_file_reader=NULL, gps_file_reader=NULL) {
  start = proc.time()

  expect_false(is.null(acc_file_reader))
  expect_false(is.null(gps_file_reader))
  expect_false(is.null(time_zone), "Must provide a time zone for proper handling of accelerometry data")

  #' Stage I: Accelerometry data processing
  # 1. read acc data
  acc_data <- acc_file_reader(acc_file_path = acc_file_path)
  acc_done = proc.time()
  message(paste0('acc_data processed in', round((acc_done - start)[3], 2), ' s\n'))

  # 2.1 assign epoch start times and generate data read epochs
  data1 <- acc_data %>%
    rename(LocalTime=date_time, Axis1=count)
  epoch_time <- min(acc_data$date_time) + floor((acc_data$date_time - min(acc_data$date_time)) / 30) * 30
  data1$epoch_time <- format(epoch_time, format='%Y-%m-%d %H:%M:%S', tz=time_zone, usetz=TRUE)
  data1 <- tibble(data1)
  epoch_done = proc.time()
  message(paste0('epoch start times processed in', round((epoch_done - acc_done)[3], 2), ' s\n'))


  # 3. classify activity epochs by that are axis1 sums over 500 cpe
  # Create Axis1 sums for each 30 second epoch
  # create threshold indicator for counts/30s epoch > 500
  data1 <- data1 %>% classify_accelerometry_activity(., epoch_field='epoch_time',  cpe_field='Axis1_epochSum',  minimum_cpe=refvalues$min_pa_cpe)
  class_done = proc.time()
  message(paste0('activity epochs part 2 classified in ', round((class_done - epoch_done)[3], 2), ' s\n'))

  # 4. Summarize into epoch
  # summarize into epoch scalesacc_done = proc.time()
  epoch_series <- data1 %>%
    select(epoch_time, Axis1_epochSum, Activity) %>%
    .[!duplicated(.)]

  epoch_rle_df <- summarize_epoch_activity(df=epoch_series, activity_field='Activity', epoch_inc=30)
  summarize_done = proc.time()
  message(paste0('summarization done in ', round((summarize_done - class_done)[3], 2), ' s\n'))

  # 5. identify nonwearing periods
  # over 20 min consecutive zero activity per epoch

  # 6. identify tolerable breaks as part of a physical activity bout
  # identify activity that is "within 2-min tolerance of low activity"

  # generate the bout labels
  epoch_rle_df <- epoch_rle_df %>%
    identify_nonwearing_periods(., activity_field='Activity', activity_value='Non_active', duration_field='duration', epoch_inc=30, threshold_lower=refvalues_s$min_conseczero_s)
  label1_done = proc.time()
  message(paste0('label 1 generation done in ', round((label1_done - summarize_done)[3], 2), ' s\n'))

  epoch_rle_df <- epoch_rle_df %>%
    identify_pa_bouts(df=.,
                      activity_field='Activity', activity_values=c('Active'), nonactivity_values= c('Low active','Non_active'),
                      duration_field='duration',
                      bout_field='bout_label',
                      epoch_inc=30,
                      accelerometry_complete_days = refvalues_s$min_accel_wearing_s,
                      min_accelerometry_window = refvalues_s$min_pa_window_s,
                      low_intense_threshold = refvalues_s$max_pa_break_s)
  label2_done = proc.time()
  message(paste0('label 2 generation done in ', round((label2_done - label1_done)[3], 2), ' s\n'))

  # 7. transfer the physical activity bout labels to the epoch_series, time_series
  if (!('bout_label' %in% colnames(epoch_rle_df))) {
    message("This subject has no physically active bouts, returning result of zero row")
    tbl_colnames <- c('bout_label','NonWalk1_ACC','NonWalk2_GPS','Walk1_GPS','n_epochs','min_accel_count','mean_accel_count','max_accel_count','complete_days','sufficient_GPS_coverage','dwell_bouts','median_speed','bout_start_date','bout_start_time','bout_end_date','bout_end_time', 'This subject has no physically active bouts, returning result of zero row')
    empty_df <- read_csv("\n", col_names = tbl_colnames)
    return(empty_df)
  }

  # extract the bout labels
  epoch_series <- propagate_bout_labels(rle_df=epoch_rle_df, epoch_series=epoch_series, bout_field='bout_label')

  # 8. Transfer on binary features tagged about the accelerometry time period, e.g., nonwearing times
  # nonwearing periods were defined as periods with consecutive zero actvity reads for >=20min

  # extract the nonwearing periods
  epoch_series <- propagate_binary_labels(rle_df=epoch_rle_df, epoch_series=epoch_series, feature_field='Nonwearing')


  # 9. Compute summary table about complete days
  # complete days are defined as 1) having at least one place record in the travel diary and 2) accelerometer wearing time >=8hrs

  # identify complete days
  epoch_date_summ <- compute_complete_days(epoch_series=epoch_series,
                                          epoch_inc=30,
                                          min_wearings_hours_per_day=refvalues$min_accel_wearing_hr,
                                          nonwearing_field='Nonwearing',
                                          epoch_datetime_field='epoch_time')

  # merge complete days info
  epoch_series <- epoch_series %>%
    mutate(epoch_date=as.Date(epoch_time)) %>%
    merge(epoch_date_summ, by='epoch_date', all=T)

  #' Stage II: GPS data processing
  # 1. read in the corresponding GPS file
  gps_data <- gps_file_reader(gps_file_path=gps_file_path)

  # 2. fix and generate local.datetime and utc.datetime
  data2 <- gps_data %>%
    data.table() %>%
    rename(LOCAL.DATETIME=date_time, LATITUDE=latitude, LONGITUDE=longitude, SPEED=speed) %>%
    rowwise() %>%
    mutate(epoch_time = find_epoch_start(reference_datetime=LOCAL.DATETIME, epoch_inc=30, time_zone=time_zone)) %>% # identify epochs
    data.table()

  # 3. full outer join of the Accelerometry data and GPS data
  gps_acc <- data2 %>%
    merge(epoch_series, by='epoch_time', all=TRUE) %>%
    mutate(bout_label = ifelse(!is.na(bout_label), bout_label, NaN)) %>%
    arrange(epoch_time, LOCAL.DATETIME)

  # 4. summarize the distribution of speeds observed during physical activity bout periods
  gps_dq <- gps_acc %>%
    group_by(bout_label) %>%
    summarise(
    # data completeness counts
    n_epochs = n_distinct(epoch_time), # number of unique epochs in bout
    n_records = n(), # number of GPS records within the bout
    n_bout_records = sum(!is.na(bout_label)), # number of mappable records to a physical activity bout
    n_speed_records = sum(!is.na(SPEED)), # speed
    n_valid_GPS_records = sum(!is.na(SPEED) & !is.na(LATITUDE) & !is.na(LONGITUDE)), # speed and GPS units
    n_nonwearing_records = sum(!is.na(Nonwearing)),

    # speed characteristics
    min_speed = ifelse(!all(is.na(SPEED)), min(SPEED, na.rm=T), NaN),
    max_speed = ifelse(!all(is.na(SPEED)), max(SPEED, na.rm=T), NaN),
    mean_speed = ifelse(!all(is.na(SPEED)), mean(SPEED, na.rm=T), NaN),
    median_speed = ifelse(!all(is.na(SPEED)), median(SPEED, na.rm=T), NaN), # median bout speed

    # speed categories
    slow = sum(SPEED >= min_speed & SPEED < refvalues_s$min_GPS_walking_speed_km_h, na.rm=T),
    walk = sum(SPEED >= refvalues_s$min_GPS_walking_speed_km_h & SPEED < refvalues_s$max_GPS_walking_speed_km_h, na.rm=T),
    fast = sum(SPEED >= refvalues_s$max_GPS_walking_speed_km_h & SPEED < max_speed, na.rm=T),

    # data quality metrics
    GPS_coverage_ratio = ifelse(n_bout_records!=0, n_valid_GPS_records/n_bout_records, NaN),
    speed_coverage_ratio = ifelse(n_bout_records!=0, n_speed_records/n_bout_records, NaN),
    sufficient_GPS_records = n_valid_GPS_records>refvalues$min_gps_obs_within_bout,
    sufficient_GPS_coverage = GPS_coverage_ratio>refvalues$min_gps_coverage_ratio)

  # unique bouts
  bouts <- gps_acc %>% filter(!is.na(bout_label)) %>% .$bout_label %>% unique()

  # loop through bouts and generate bounding circle
  for (eachbout in bouts){
    # 5. identify unique spatial points
    Point_ind <- gps_acc %>%
      filter(bout_label==eachbout & !is.na(LONGITUDE) & !is.na(LATITUDE)) %>%
      select(LONGITUDE, LATITUDE) %>%
      .[!duplicated(.)]

    # if no spatial information, skip
    if (nrow(Point_ind)<refvalues_s$min_gps_consec_obs){
      gps_acc[bout_label==eachbout, incomplete_GPS:= 1]
      next
    }

    # generate unique Points as SpatialPoint object
    # 6. generate the distance matrix
    Distances <- SpatialPoints(coords = cbind(long = Point_ind$LONGITUDE, lat = Point_ind$LATITUDE)) %>%
      spDists(., longlat = TRUE) # kilometers

    sumDist <- Distances %>% colSums() # 7. calculate sum of distance for each point
    thresh <- quantile(sumDist, c(refvalues$dwellbout_radii_quantile))
    KeepPoints <- sumDist < thresh[[1]][1] # 8. identify points below the 95% percentile of sum of distances

    # 9. identify the radius of the bounding circle containing the selected points
    P_obj <- Point_ind[KeepPoints,] %>%
      convert_points_to_multipoint_polygon(.) %>% # generate bout points
      convert_bounding_circle(., gps_data=gps_acc, eachbout=eachbout) # generate bounding circle (longlat coordinates)

    # 10 Calculate the bounding circle radius
    # https://www.rdocumentation.org/packages/geosphere/versions/1.5-10/topics/areaPolygon
    P_area <- areaPolygon(x=P_obj[[1]]) # calculate polygon area (unit: square meters)
    gps_acc[bout_label==eachbout, Point_circle_area:=P_area]
    P_radii <- sqrt(P_area/pi) %>% conv_unit(., from='m', to='ft') # Area = pi*r^2; r units should be in m
    gps_acc[bout_label==eachbout, Point_radius:=P_radii]

    # assign figure title
    title(paste0('Bout ',toString(eachbout),': ',toString(round(P_radii,3)), ' feet circle radius'), add=TRUE)

    # 11. evaluate circle radius for dwell bout category (if case_when condition met, assign 1)
    gps_acc[bout_label==eachbout, dwell_bout:= case_when(Point_radius<=refvalues_s$max_dwellbout_radii_ft & n_distinct(epoch_time)>=refvalues_s$min_dwellbout_obs ~ 1)]  # weipeng edit; should be max_dwell_bout_radii_ft
  }

  if (!("dwell_bout" %in% colnames(gps_acc)) && sum(gps_acc$incomplete_GPS[!is.na(gps_acc$incomplete_GPS)] != 1) == 0) {  # no dwell bout at all due to incomplete GPS for all bouts
    message("This subject's all bouts have incomplete GPS coverage, returning result of zero row")
    tbl_colnames <- c('bout_label','NonWalk1_ACC','NonWalk2_GPS','Walk1_GPS','n_epochs','min_accel_count','mean_accel_count','max_accel_count','complete_days','sufficient_GPS_coverage','dwell_bouts','median_speed','bout_start_date','bout_start_time','bout_end_date','bout_end_time', "This subject's all bouts have incomplete GPS coverage, returning result of zero row")
    empty_df <- read_csv("\n", col_names = tbl_colnames)
    return(empty_df)
  }

  # summarise the bout_labels for dwell_bouts
  epoch_summary <- gps_acc %>%
    group_by(bout_label) %>%
    summarise(dwell_bouts=ifelse(sum(dwell_bout, na.rm=T)>1, 1, 0)) %>% # removing NaN values
    data.table() %>%
    merge(epoch_series, ., by='bout_label', all=T) %>% # merge in the epoch_series data
    merge(., gps_dq, by='bout_label', all=T) # merge in the data quality assessment and complete days

  # find different kinds of bouts
  epoch_summary <- epoch_summary %>%
    group_by(bout_label) %>%
    mutate(
      min_accel_count = min(Axis1_epochSum, na.rm = T),
      mean_accel_count = mean(Axis1_epochSum, na.rm = T),
      max_accel_count = max(Axis1_epochSum, na.rm = T),
      NonWalk1_ACC = case_when(mean_accel_count>=refvalues_s$max_light_moderate_pa_cpe ~1), # too fast; running/driving
      NonWalk2_GPS = case_when((complete_days==T & dwell_bouts==1) | # dwell-bout
                                 (complete_days==T & dwell_bouts==0 &
                                   (median_speed < refvalues_s$min_gps_walking_speed_km_h | median_speed >refvalues_s$max_gps_walking_speed_km_h)) ~1), # treadmill or bicycle
      Walk1_GPS = case_when(complete_days==T & dwell_bouts==0 &
                              median_speed >= refvalues_s$min_gps_walking_speed_km_h & median_speed <=refvalues_s$max_gps_walking_speed_km_h ~ 1)) %>% # walk bouts
    data.table()

  # summarize each bout category
  bout_summary <- epoch_summary %>%
    select(bout_label,
           NonWalk1_ACC, NonWalk2_GPS, Walk1_GPS,
           n_epochs, min_accel_count, mean_accel_count, max_accel_count,
           complete_days, sufficient_GPS_coverage, dwell_bouts, median_speed) %>%
    .[!duplicated(.)] %>%
    filter(!is.na(bout_label))

  # with start-end date summary
  select_first_or_last_row <- function (dataframe, first_row=NULL, ...) {
    if (first_row) {
      dataframe <- head(dataframe, 1)
    } else {
      dataframe <- tail(dataframe, 1)
    }
    dataframe <- dataframe %>% select(epoch_date, epoch_time)
    return(dataframe)
  }

  bout_start <- epoch_summary %>%
    select(epoch_date, epoch_time, bout_label, NonWalk1_ACC, NonWalk2_GPS, Walk1_GPS, n_epochs, min_accel_count, mean_accel_count, max_accel_count, complete_days, sufficient_GPS_coverage, sufficient_GPS_coverage, dwell_bouts, median_speed) %>%
    group_by(bout_label, NonWalk1_ACC, NonWalk2_GPS, Walk1_GPS, n_epochs, min_accel_count, mean_accel_count, max_accel_count, complete_days, sufficient_GPS_coverage, dwell_bouts, median_speed) %>%
    group_modify(.f = select_first_or_last_row, .keep = TRUE, first_row=TRUE) %>%
    ungroup() %>%
    rename(bout_start_date=epoch_date, bout_start_time=epoch_time) %>%
    distinct() %>%
    filter(!is.na(bout_label))

  bout_end <- epoch_summary %>%
    select(epoch_date, epoch_time, bout_label, NonWalk1_ACC, NonWalk2_GPS, Walk1_GPS, n_epochs, min_accel_count, mean_accel_count, max_accel_count, complete_days, sufficient_GPS_coverage, sufficient_GPS_coverage, dwell_bouts, median_speed) %>%
    group_by(bout_label, NonWalk1_ACC, NonWalk2_GPS, Walk1_GPS, n_epochs, min_accel_count, mean_accel_count, max_accel_count, complete_days, sufficient_GPS_coverage, dwell_bouts, median_speed) %>%
    group_modify(.f = select_first_or_last_row, .keep = TRUE, first_row=FALSE) %>%
    ungroup() %>%
    rename(bout_end_date=epoch_date, bout_end_time=epoch_time) %>%
    distinct() %>%
    filter(!is.na(bout_label))

  summary_with_start_end_date <- inner_join(bout_start, bout_end, by=c( "bout_label", "NonWalk1_ACC", "NonWalk2_GPS", "Walk1_GPS", "n_epochs", "min_accel_count", "mean_accel_count", "max_accel_count", "complete_days", "sufficient_GPS_coverage", "dwell_bouts", "median_speed" ))
  #summary_with_start_end_date <- summary_with_start_end_date %>% select(-c(bout_label, sufficient_GPS_coverage, bout_start_date, bout_end_date, complete_days))
  other_done = proc.time()
  message(paste0('other done in ', round((other_done - label2_done)[3], 2), ' s\n'))



  return(summary_with_start_end_date)
}
