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


summarize_maybe_bout <- function(df){
  rle_df <- with(rle(as.numeric(df$non_bout)),
                 data.frame(tibble("values" = values,
                                   "lengths" = lengths,
                                   'maybe_bout' := factor(values, labels = c('T', 'F')),
                                   "cumul_length" = cumsum(lengths),
                                   "begin" = replace_na(lag(cumul_length) + 1, replace = 1),
                                   "end" = cumul_length,
                                   "duration" = end - begin + 1))) %>%
    data.table()
  return(rle_df)

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


identify_bouts <- function(df){
  ##################################################################################################
  # This is an alternate way to do Weipeng's loop but is different in that it always ends a
  # bout on the last active epoch before 2 min of consecutive inactivity.
  # This does not produce the same thing as Weipeng. Weipeng does 1 of 2 things:
  # 1. If we have 5 accumulated minutes of activity/inactivity/non-activity without 2 sequential minutes of inactivity,
  # and then 2 minutes of inactivity, Weipeng considers that entire 7 minutes a bout.
  # 2. If we have 8 accumulated minutes of activity/inactivity/non-activity without 2 sequential minutes of inactivity,
  # and then 2 minutes of inactivity, Weipeng considers the 8 minutes a bout and drops the 2 minutes of inactivity at the end.

  bout_df <- copy(df)
  bout_df$Inactive <- (bout_df$Activity != 'Active')
  bout_df$non_bout <- frollsum(bout_df$Inactive, 4) == 4
  bout_df$bout_label <- NaN
  bout_rle_df <- summarize_maybe_bout(bout_df)
  potential_bouts <- bout_rle_df[(bout_rle_df$lengths >= 17) & (bout_rle_df$maybe_bout == 'T')]
  num_bouts <- 0
  for (i in 1:nrow(potential_bouts)){
    row <- slice(potential_bouts, i)
    start_ind <- row[['begin']]
    end_ind <- row[['end']]-3
    is_bout <- sum(bout_df[start_ind:end_ind]$Activity == 'Active') > 10
    if (is_bout){
      num_bouts <- num_bouts + 1
      bout_df[start_ind:end_ind]$bout_label <- num_bouts
    }
  }
  bout_df <- bout_df[,Inactive:=NULL]
  bout_df <- bout_df[,non_bout:=NULL]
  return(bout_df)
}

############# ############# ############# ############# #############
#
# Propagate labels and summarise time-series and run-length encoding
#
# function = propagate_binary_labels
# function = compute_complete_days
#
############# ############# ############# ############# #############


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

