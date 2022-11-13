####### ####### ####### ####### #######
#
# assign time-series epochs
#
####### ####### ####### ####### #######

find_epoch_start <- function (reference_datetime, epoch_inc=30, time_zone="America/Los_Angeles"){
  ###
  #
  # find the start of the epoch
  # epoch_inc must be in seconds
  #
  # generate the prior start times within the epoch increment
  # select the most recent time that is divisible by the epoch_inc
  ###

  # generate the timepoints within the last epoch inc
  prior_times <- rep(as.numeric(reference_datetime), epoch_inc) - c((1:epoch_inc)-1)

  # select the closest and most recent epoch start time
  epoch_time <- prior_times[prior_times%%epoch_inc==0] %>%
    sort(decreasing=TRUE) %>%
    head(1) %>%
    as_datetime() %>%
    format(format='%Y-%m-%d %H:%M:%S', tz=time_zone, usetz=TRUE)
  return(epoch_time)
}


fix_date_time <- function(df, date_field, time_field, date_format, time_format, output_datetime_format='%Y-%m-%d %H:%M:%S', time_zone='America/Los_Angeles', data_source=NULL){ # datas_ource {"GPS", "ACC"}
  #
  # fix localTime and use it as future reference
  #
  # df (data.table): the source data frame containing date and time as separate fields
  # date_field (str): the name of the date field to transform
  # time_field (str): the name of the time field to transform
  # date_format (str): the format of the date information
  # time_format (str): the format of the time information
  # output_datetime_format (str): the standardized datetime output

  expect_false(is.null(data_source))
  if (data_source == "ACC") {
    fixed_df <- df %>%
      mutate('Date' := as.POSIXct(.data[['Date']], format = {{date_format}}),
             'Time' := format(as.POSIXct(.data[['Time']], format = {{time_format}}), time_format),
             'DateTIME' := as.POSIXct(paste(.data[['Date']], .data[['Time']]), format = output_datetime_format, tz=time_zone))

    start_time <- fixed_df[['DateTIME']][1]
    num_time_stamp <- nrow(fixed_df)
    epoch_interval <- fixed_df[['DateTIME']][2] - fixed_df[['DateTIME']][1]
    fixed_df[['DateTIME']] <- seq(from=start_time, length.out = num_time_stamp, by = epoch_interval, tz=time_zone)
    expect_true(sum(is.na(fixed_df[['DateTIME']])) == 0)
    return(fixed_df)
  }

  if (data_source == "GPS") {
    fixed_df <- df %>% # note that this is dataframe not datatable; acc is datatable though, because the way both files are read are different
      mutate('UTC.DATE' := as.POSIXct(.data[['UTC.DATE']], format = {{date_format}}),
             'UTC.TIME' := format(as.POSIXct(.data[['UTC.TIME']], format = {{time_format}}), time_format),
             'UTC.DATETIME' := as.POSIXct(paste(.data[['UTC.DATE']], .data[['UTC.TIME']]), format = output_datetime_format, tz = "UTC")) %>%
      mutate('LOCAL.DATE' := as.POSIXct(.data[['LOCAL.DATE']], format = {{ date_format }}),
             'LOCAL.TIME' := format(as.POSIXct(.data[['LOCAL.TIME']], format = {{ time_format }}), time_format),
             'LOCAL.DATETIME' := as.POSIXct(paste(.data[['LOCAL.DATE']], .data[['LOCAL.TIME']]), format = output_datetime_format, tz=time_zone))
    fixed_df$LOCAL.DATETIME <- with_tz(fixed_df$UTC.DATETIME, tz=time_zone)  # infer from UTC
    return(fixed_df)
  }

  if (data_source != "GPS" && data_source != "ACC") {
    stop("error")
  }
}


generate_sum_by_epoch <- function(df, epoch_field, epoch_inc, sum_field){
  #
  # calculate Activity sums per epoch
  #
  # df (data.table): the data.table containing the time-series records (accelerometry or GPS)
  # epoch_field (str): name of the column to use
  # epoch_inc (int): the seconds in an epoch
  # sum_field (str): the name of the field to summate for activity per epoch
  epoch_var = paste0({{sum_field}}, '_epochSum')

  df %>%
    group_by(.data[[ epoch_field ]]) %>%
    mutate("{epoch_var}" := sum(.data[[ sum_field ]])) %>%
    data.table()
}


summarize_epoch_activity <- function(df, activity_field, epoch_inc){
  #
  # summarize the run length encodings by epoch activities
  # Compute run-length encoding on clusters of activity/inactivity
  #
  # df (data.table): the data table containing the time-series information
  # activity_field (str): the name of the field to be used as the activity measure; must contain either Active, Low active, or Non_active
  # epoch_inc (int): the epoch increment for analyses, e.g., 30-sec
  # min_conseczero_s (int): the minimum number of seconds of consecutive zeroes to declare non-activity
  # max_pa_break_s (int): the maximum number of tolerable activity breaks to be considered a single physical activity bout
  #
  ## replace NA with 1 as only NA will be in row 1 since no previous row for lag function

  # reduce to the unique epoch summaries
  df1 = df %>% .[!duplicated(.$epoch_time)]

  rle_df <- with(rle(as.numeric(df1[[activity_field]])),
                 data.frame(tibble("values" = values,
                                   "lengths" = lengths,
                                   '{activity_field}' := factor(values, labels = levels(df1[[activity_field]])),
                                   "cumul_length" = cumsum(lengths),
                                   "begin" = replace_na(lag(cumul_length) + 1, replace = 1),
                                   "end" = cumul_length,
                                   "duration" = end - begin + 1))) %>%
    data.table()

  return(rle_df)
}


identify_nonwearing_periods <- function(df, activity_field='Activity', activity_value='Non_active',
                                        duration_field='duration', epoch_inc=30,
                                        threshold_lower=refvalues_s$min_conseczero_s){
  #
  # identify nonwearing periods
  #
  # df (data.table): the datatable containing the run-length encodings for the Activity category
  # activity_field (str): the name of the column containing activity status
  # activity_value (str): the name of activity value for non-wearing periods
  # duration_field (str): the name of the column containing duration values
  # epoch_inc (int): the number of seconds per epoch
  # threshold_lower (int): the minimum number of consecutive zero activity counts for non-wearing bouts

  return(df %>%
           mutate(Nonwearing = case_when(df[[activity_field]]=={activity_value}
                                         & df[[duration_field]]*epoch_inc>=threshold_lower ~ 1)))
}


identify_pa_bouts <- function(df,
                              activity_field='Activity',
                              activity_values=c('Active'),
                              nonactivity_values= c('Low active','Non_active'),
                              bout_field='bout_id',
                              duration_field='duration',
                              epoch_inc=30,
                              accelerometry_complete_days = refvalues_s$min_accel_wearing_s,
                              min_accelerometry_window = refvalues_s$min_pa_window_s,
                              low_intense_threshold = refvalues_s$max_pa_break_s){
  #
  # identify physical activity bouts
  # - physical activity bouts are ended when consecutive inactivity exceeds the tolerance break (low intensity threshold)
  # - physical activity bouts are recognized and stored when the net activity period exceeds the minimum activity window (min_accelerometry_window)
  #
  # accelerometry_complete_days (int): total wearing time more than 8hrs in the day
  # min_accelerometry_window (int): minimum time window for a physical activity bout e.g., 7 min
  # low_intense_threshold (int): maximum low intensity threshold e.g., 2min
  # epoch_inc (int): the seconds in an epoch

  # initial params
  Act=0
  Act_ind=c()
  Inact=0
  Inact_ind=c()
  bout_number=1

  for (ind in 1:nrow(df)){
    r = df %>% slice(ind)

    # add activity periods
    if (r[[ activity_field ]] %in% { activity_values }){
      Act = Act+(r[[ duration_field ]]*{epoch_inc})
      Act_ind = c(Act_ind,ind)
      Inact=0
    }

    # low intensity or minor non-activity
    if (r[[ activity_field ]] %in% { nonactivity_values } & Act!=0){
      # Tolerable break sequence
      if ((Inact+(r[[ duration_field ]]*epoch_inc)<low_intense_threshold) & (Act<min_accelerometry_window)){
        Act = Act+(r[[ duration_field ]]*epoch_inc)
        Act_ind = c(Act_ind,ind)
        Inact = Inact+(r[[ duration_field ]]*epoch_inc)
        Inact_ind = c(Inact_ind,ind)

        # Inactivity too high. Not enough activity to be a window. Reset.
      } else if ((Inact+(r[[ duration_field ]]*epoch_inc)>=low_intense_threshold) & (Act<min_accelerometry_window)){
        Act=0
        Act_ind=c()
        Inact=0
        Inact_ind=c()

        # Inactivity too high. Enough Activity to be a window. Record as a bout.
      } else if ((Inact+(r[[ duration_field ]]*epoch_inc)>=low_intense_threshold) & (Act>=min_accelerometry_window)){
        #print(sort(c(Act_ind,Inact_ind)))
        df[sort(c(Act_ind,Inact_ind)), {bout_field}] <- bout_number
        bout_number= bout_number + 1

        Act=0
        Act_ind=c()
        Inact=0
        Inact_ind=c()
      }
    }
  }
  return(df)
}


############# ############# ############# ############# #############
#
# Propagate labels and summarise time-series and run-length encoding
#
# function = propagate_bout_labels
# function = propagate_binary_labels
# function = compute_complete_days
#
############# ############# ############# ############# #############

propagate_bout_labels <- function(rle_df, epoch_series, bout_field='bout_id'){
  #
  # dimensions of rle_df start and end should correspond to epoch_series row_indices
  #
  # rle_df (data.table): the source compressed run-length encoding for the Activity types
  # epoch_series (data.table): the target long table that describes each epoch within the rle_df data.table
  # bout_field (str): the name of the column to be transfered

  # generate the output dimension
  epoch_series <- epoch_series %>% mutate('{bout_field}' := NaN)

  # isolate the bouts
  bouts <- rle_df %>% filter(!is.na(.data[[ bout_field ]]))

  # loop through each bout
  for (ind in (unique(bouts[[ bout_field ]]))){

    # generate bout start and bout end
    tmp = bouts %>%
      filter(.[[ bout_field ]] %in% c(ind)) %>%
      summarize(bout_start = min(begin), bout_end = max(end))

    # assign bout label
    epoch_series[tmp[['bout_start']] : tmp[['bout_end']], {bout_field}] <- ind
  }
  return(epoch_series)
}


propagate_binary_labels <- function(rle_df, epoch_series, feature_field='Nonwearing'){
  #
  # dimensions of rle_df start and end should correspond to epoch_series row_indices

  # rle_df (data.table): the source compressed run-length encoding for the Activity types
  # epoch_series (data.table): the target long table that describes each epoch within the rle_df data.table
  # feature_field (str): the name of the column to be transfered

  # generate the output dimension
  epoch_series <- epoch_series %>% mutate('{feature_field}' := NaN)

  # isolate the run length encodings with the feature of interest
  feat <- rle_df %>% filter(!is.na(.data[[ feature_field ]]))

  # loop through each bout
  for (ind in (1:nrow(feat))){

    # generate bout start and bout end
    tmp = feat %>%
      slice(ind) %>%
      summarize(feature_start = min(begin), feature_end = max(end))

    # assign bout label
    epoch_series[tmp[['feature_start']] : tmp[['feature_end']], { feature_field }] <- ind
  }
  return(epoch_series)
}


compute_complete_days <- function(epoch_series, epoch_inc=30, min_wearings_hours_per_day=8, nonwearing_field='Nonwearing', epoch_datetime_field='epoch_time'){
  #
  # complete days requires that the net activity per day exceed the minimum wearing times (min_wearings_hours_per_day)
  #
  # epoch_series (data.table):
  # epoch_inc (int):
  # min_wearings_hours_per_day (int): number of hours in the day
  # nonwearing_field (str):
  # epoch_datetime_field (str):

  e1 = epoch_series %>%
    filter(!is.na(.data[[ nonwearing_field ]])) %>%
    mutate(epoch_date = as.Date(.data[[ epoch_datetime_field ]]),
           Nonwearing_flag = case_when(!is.na(.data[[ nonwearing_field ]]) ~ 1)) %>%
    group_by(epoch_date) %>%
    summarise(nonwearing_periods = n_distinct(.data[[ nonwearing_field ]]),
              nonwearing_epochs = sum(Nonwearing_flag),
              nonwearing_seconds = (nonwearing_epochs*{ epoch_inc }),
              nonwearing_hours = nonwearing_seconds/60/60,
              wearing_hours = 24-nonwearing_hours,
              complete_days = wearing_hours>={ min_wearings_hours_per_day }) %>%
    data.table()
  return(e1)
}

