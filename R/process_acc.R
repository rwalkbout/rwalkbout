identify_bouts <- function(datadir, out_file, n_col = 12, return_df = F){
  
  #get list of csv file names
  csvfnames <- list.files(path = datadir, pattern = "*.csv", recursive = TRUE, full.names = TRUE)
  
  all_bouts_df <- tibble("Sum_Duration" = NA,
                         "idno" = NA,
                         "Date" = NA)
  
  bouts_person_df <- tibble("idno" = NA,
                            "N_Bouts" = NA,
                            "Rows" = NA,
                            "Wear_Time_Seconds" = NA,
                            "Wear_Time_HMS" = as.period(NA),
                            "Run_Time" = NA,
                            "Valid_Days" = NA,
                            "flag" = NA)
  
  # for(i in 1:length(csvfnames)){
  for(i in 1:length(csvfnames)){
    
    #print the row number for debugging
    print(i)
   
    time1 <- Sys.time()
  
  #read in one file and identify walk bouts
    data1 <- read_csv(csvfnames[i], skip = 10)
    
    #identify epoch length from meta data (line 5 of standard output format)
    data1_epoch <- read_csv(csvfnames[i], skip = 4, n_max = 1, col_names = "epoch") %>%
      pull(epoch) %>%
      str_sub(start = 25, end = 32) %>%
      hms() %>%
      as.numeric()
    
    
    
    
    #pull idno from filename
    id <- str_sub(csvfnames[i], 100, 105)
    
    # bouts_person_df$idno[i] <- id
    
    #flag if the number of columns is different than expected
    if(ncol(data1) != n_col) {warning(paste0("Participant ", id, ", row ", i, " data format unexpected; record dropped"))} 
    
    #only execute code if proper number of columns
    #using two if statements rather than if...else due to the latter being finicky
    if(ncol(data1) == n_col){
    
    #1. Collapse to 30 second epochs
    
    #a. Create lead variable and sum current row and lead cpe to create running 30 second total
    
        if(data1_epoch == 15){
        
          data1 <- data1 %>%
          mutate("lead_Axis1" = lead(Axis1),
                 "Axis1_totl30" = Axis1 + lead_Axis1)
          
        }
          
        if(data1_epoch == 10){
          
          data1 <- data1 %>%
            mutate("lead1_Axis1" = lead(Axis1),
                   "lead2_Axis1" = lead(Axis1, n = 2),
                   "Axis1_totl30" = Axis1 + lead1_Axis1 + lead2_Axis1)
          
        }  
    
    #check
    # min_ind <- min(which(data1$Axis1 > 0))
    
    
    # *Check*
      
    # 
    # data1 %>%
    #   select(Time, Axis1, lead_Axis1, Axis1_totl30) %>%
    #   slice(min_ind, min_ind+1)
    
    
    #b. Subset to keep only rows whose time is a multiple of 30 seconds
    
    #create seconds variable
    data1 <- data1 %>%
      mutate("time_sec" = as.duration(Time)) %>%
      filter(as.numeric(time_sec)%%30 == 0) #keep only rows where time is multiple of 30s
    
    #create threshold indicator for counts/30s epoch > 500
    data1 <- data1 %>%
      mutate("ovr500" = Axis1_totl30 > 500)
    
    
    #2. Remove blocks of zeros >= 20 minutes
    
    #a. categorize as Active/Non_Active based on Axis1  > 0
    data1 <- data1 %>%
      mutate("Active" = case_when(Axis1_totl30 > 0 ~ "Active",
                                  Axis1_totl30 == 0 ~ "Non_Active") %>% factor())
    
    
    #b. Run length encoding on clusters of activity/inactivity
    
    rle_active <- rle(as.numeric(data1$Active))
    
    #c. Convert output of rle to data frame to use to create indices for subsetting data
    
    # *Cumulative sum of run lengths (from run length encoding) gives the index of the end of each run. The index for the beginning of each run can be found by the end of the previous run plus 1*
      
    #convert rle output to data frame
    rle_df <- tibble("values" = rle_active$values,
                     "Active" = factor(values,
                                       labels = c("Active", "Inactive")),
                     "lengths" = rle_active$lengths,
                     "cumul_lgth" = cumsum(lengths),
                     #replace NA with 1 as only NA will be in row 1 since no previous row for lag function
                     "begin" = replace_na(lag(cumul_lgth) + 1, replace = 1),
                     "end" = cumul_lgth,
                     "duration" = end - begin + 1)
    
    #d. Create a vector of rows than meet the criteria to be removed (inactive run length of over 40 rows/20 minutes)
    indices <- rle_df %>%
      filter(values == 2 & duration > 40) %>%
      select(begin, end)
    
      
      
    #e. Loop through rle data frame to fill a vector of values to remove defined by the beginning and end index of each row
    to_remove <- NULL
    
    for(q in 1:length(indices$begin)){
      beg_ <- indices$begin[q]
      end_ <- indices$end[q]
      values_ <- beg_:end_
      # print(values_)
      to_remove <- c(to_remove, values_)
    }
    
    data1$remove_20min <- "Keep"
    data1$remove_20min[to_remove] <- "Remove"
    
    #f. create subset data frame after removing inactive periods
    data2 <-  data1 %>%
      filter(remove_20min == "Keep")
    
    #g. Keep only days with at least 8hr of wear time after removing 20 min runs of inactivity
    
    #create count of (valid) epochs/rows per day
    date_counts <- data2 %>%
      group_by(Date) %>%
      tally()
    
    #further subset by days with >= 960 rows (8 hours)
    valid_dates <- date_counts %>%
      filter(n >= 960)
    
    #subset data frame by valid dates (>= 8hrs of activity)
    data3 <- data2 %>%
      filter(Date %in% valid_dates$Date)
    
    
    
    
    ###Use rle to identify runs of activity >= 500 cpe threshold
    
    #identify bouts based on five minute consecutive epochs >= 500 cpe
    rle_over500 <- rle(data3$ovr500)
    rle_over500$values[rle_over500$values == "TRUE"] <- "Walking"
    rle_over500$values[rle_over500$values == "FALSE"] <- "No_Walk"
    
    #only run rest of code if there is at least 1 run of physical activity (>= 500cpe)
    # rle values > 1 because first entry will be nonactivity ("No_Walk"); > 1 requires at least 1 entry of "Walking"
    if(length(rle_over500$values) > 1){
    
    #Create data frame from output of rle, include cumulative sum (length), and beginning/end row index of each run
    rle_dfbouts <- tibble("Value" = rle_over500$values,
                          "Length" = rle_over500$lengths,
                          "cumul_lgth" = cumsum(Length),
                          "begin" = replace_na(lag(cumul_lgth) + 1, replace = 1),
                          "end" = cumul_lgth)
    
    # Identify bouts by time rather than consecutive rows
    rle_dfbouts$Date <- data3$Date[rle_dfbouts$begin]
    rle_dfbouts$Time_begin <- data3$Time[rle_dfbouts$begin]
    rle_dfbouts$Time_end <- data3$Time[rle_dfbouts$end]
    rle_dfbouts$Duration <- (rle_dfbouts$Time_end - rle_dfbouts$Time_begin + 30)
    rle_dfbouts$lag_end <- lag(rle_dfbouts$Time_end)
    
    
    rle_walkbouts <- rle_dfbouts %>%
      filter(Value == "Walking") %>%
      mutate("lag_end" = lag(Time_end),
             "Downtime_before" = Time_begin - lag_end + 30,
             "run_id" = row_number())
    
    
    #Add cumulative duration for each bout, resetting when cumulative downtime = 0
    #set downtime_before to 0 for first row
    rle_walkbouts$Downtime_before[1] <- 0
    
    rle_walkbouts$cumul_duration <- 0
    
    #set cumul_duration to Duration for first row
    rle_walkbouts$cumul_duration[1] <- rle_walkbouts$Duration[1]
    
    #Create a counter for bouts - each new bout adds 1
    rle_walkbouts$bout_count <- 1
    
    if(nrow(rle_walkbouts) >= 2){
      for(ii in 2:nrow(rle_walkbouts)){
      
        rle_walkbouts$cumul_duration[ii] <- ifelse(rle_walkbouts$Downtime_before[ii] > 120,
                                                rle_walkbouts$Duration[ii],
                                                rle_walkbouts$Duration[ii] + rle_walkbouts$cumul_duration[ii - 1])
      
        rle_walkbouts$bout_count[ii] <- ifelse(rle_walkbouts$Downtime_before[ii] > 120,
                                            rle_walkbouts$bout_count[ii - 1] + 1,
                                            rle_walkbouts$bout_count[ii - 1])
      
            }
          }
          
    
    
    time2 <- Sys.time()
    
    
    potential_bouts <- rle_walkbouts %>%
      group_by(bout_count) %>%
      summarise("Begin" = seconds_to_period(min(Time_begin)),
                "End" = seconds_to_period(max(Time_end)),
                "Sum_Duration" = sum(Duration),
                "Date" = Date)
    
    valid_bouts_df <- potential_bouts %>%
      filter(Sum_Duration >= 300) %>%
      mutate("idno" = id) %>%
      select(Sum_Duration, idno, Begin, End, Date)
    
    # all_bouts_df <- tibble("Sum_Duration" = NA,
    #                        "idno" = NA,
    #                        "Date" = NA,
    #                        "Time_Begin" = NA,
    #                        "Time_End" = NA)
    
    
    time2 <- Sys.time()
    
    elapsed <- time2 - time1
    
    #add to bout level data set
    all_bouts_df <- all_bouts_df %>%
      bind_rows(valid_bouts_df)
    
    #create individual person level data
    valid_bouts_total <- valid_bouts_df %>%
      group_by(idno) %>%
      summarise("N_Bouts" = nrow(.)) %>%
      mutate("Rows" = nrow(data3),
             "Wear_Time_Seconds" = nrow(data3) * 30,
             "Wear_Time_HMS" = seconds_to_period(Wear_Time_Seconds),
             "Run_Time" = elapsed,
             "Valid_Days" = nrow(valid_dates))
    
    #add to person level data set
    bouts_person_df <- bouts_person_df %>%
      bind_rows(valid_bouts_total)
    
        } #closing if statment (length(rle500$values) > 1)
    
    #logic for entries with insufficient walk time to process
    if(length(rle_over500$values) <= 1){ 
      time2 <- Sys.time() 
      
      elapsed <- time2 - time1
      
      bouts_person_temp <- tibble("idno" = id,
                                  "N_Bouts" = 0,
                                  "Rows" = nrow(data3),
                                  "Wear_Time_Seconds" = nrow(data3) * 30,
                                  "Wear_Time_HMS" = seconds_to_period(Wear_Time_Seconds),
                                  "Run_Time" = elapsed,
                                  "Valid_Days" = nrow(valid_dates))
      
      bouts_person_df <- bouts_person_df %>%
        bind_rows(bouts_person_temp)
      
       }
    
      } #end if statement for ncols
    
    } #end for loop - individual csv processing

  # save(bouts_person_df, all_bouts_df, file = here("data", "20210314-prelim_results.rda"))
  
  #create output
  save(bouts_person_df, all_bouts_df, file = out_file)
  
  if(return_df == T){
    assign("bouts_person_df", bouts_person_df, envir = .GlobalEnv)
    
    assign("all_bouts_df", all_bouts_df, envir = .GlobalEnv)
  }

} #end function



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


classify_accelerometry_activity <- function(df, epoch_field, cpe_field, minimum_cpe){
  #
  # categorize as Active, low active, or Non_Active based on Axis1 sums per epoch
  #
  # df (data.table): the data.table containimg the accelerometry time-series
  # cpe_field (str): the name of the cpe field for comparisons
  # case_when_criteria (str): the code string for case_when statements
  # minimum_cpe (int):
  df %>%
    mutate(Activity = case_when(.data[[ cpe_field ]] > { minimum_cpe } ~ 'Active',
                                .data[[ cpe_field ]] <= { minimum_cpe } & .data[[ cpe_field ]] > 0 ~ 'Low active',
                                .data[[ cpe_field ]] == 0 ~ 'Non_active') %>% as.factor()) %>%
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
           mutate(Nonwearing = case_when(df[[activity_field]]=={activity_value} & df[[duration_field]]*epoch_inc>=threshold_lower ~ 1)))
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

