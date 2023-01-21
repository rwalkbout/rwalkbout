## functions that helps the analysis for a city's data
read_result_save_folder_as_dataframe <- function (result_save_folder) {
  list_of_df <- list()
  failed_subjects <- c()
  failed_messages <- c()
  for (result_file in list.files(result_save_folder)) {
    df <- read_csv(file.path(result_save_folder, result_file))
    subject_id <- strsplit(result_file, ".csv")[[1]]
    if (nrow(df) == 0) {
      failed_subjects <- c(failed_subjects, subject_id[[1]])
      failed_messages <- c(failed_messages, tail(colnames(df), 3)[1])
      message(paste('Found result with row 0, skipping subject:', strsplit(result_file, ".csv")[[1]]))
      next
    }
    df['subject_id'] <- subject_id
    list_of_df[[result_file]] <- df
  }
  print(tibble('failed_subject'=failed_subjects, 'message'=failed_messages) %>% arrange(message))
  result_df <- do.call(rbind, list_of_df)
  return(result_df)
}

get_subject_aggragtes <- function (group_df, group_key) {
  # 1. count of each bout
  aggregate_value_list <- list()
  aggregate_value_list[["num_bouts"]] <- nrow(group_df)
  for (bout_name in c("NonWalk1_ACC", "NonWalk2_GPS", "Walk1_GPS", "dwell_bouts")) {
    aggregate_value_list[[paste0('num_', bout_name)]] <- group_df %>% select(bout_name) %>% sum(na.rm = TRUE)
  }
  # 2. walk bout specific

  walk_bout_df <- group_df %>% filter(Walk1_GPS == 1)

  if (nrow(walk_bout_df) == 0) {
    aggregate_value_list[['walk_bout_mean_n_epochs']] <- NA
    aggregate_value_list[['walk_bout_min_min_accel_count']] <- NA
    aggregate_value_list[['walk_bout_max_max_accel_count']] <- NA
    aggregate_value_list[['mean_median_speed']] <- NA
    aggregate_value_list[['walk_bout_mean_duration_mins']] <- NA
  } else {
    aggregate_value_list[['walk_bout_mean_n_epochs']] <- mean((walk_bout_df %>% select("n_epochs"))$n_epochs) # mean of n_epochs
    aggregate_value_list[['walk_bout_min_min_accel_count']] <- min((walk_bout_df %>% select("min_accel_count"))$min_accel_count) # min of min_accel_count
    aggregate_value_list[['walk_bout_max_max_accel_count']] <- max((walk_bout_df %>% select("max_accel_count"))$max_accel_count) # max of max_accel_count
    aggregate_value_list[['mean_median_speed']] <- mean((walk_bout_df %>% select("median_speed"))$median_speed) # mean of median_speed

    duration_df <- walk_bout_df %>%
      rowwise() %>%
      mutate(walk_bout_start = as_datetime(strsplit(bout_start_time, " PST")[[1]]), walk_bout_end = as_datetime(strsplit(bout_end_time, " PST")[[1]]), walk_bout_duration = difftime(walk_bout_end, walk_bout_start, units = "mins")) %>%
      select(walk_bout_start, walk_bout_end, walk_bout_duration) %>%
      select(walk_bout_duration)
    aggregate_value_list[['walk_bout_mean_duration_mins']] <- mean(as.numeric(duration_df$walk_bout_duration)) # then mean of duration
    aggregate_value_list[['walk_bout_total_mins_per_subj']] <- sum(duration_df$walk_bout_duration)
  }

  subject_aggregated_df <- as_tibble(aggregate_value_list)

  for (stat_var in c("num_bouts", "num_NonWalk1_ACC", "num_NonWalk2_GPS", "num_dwell_bouts", "num_Walk1_GPS", "walk_bout_mean_duration_mins")) {
    print(paste("Mean", stat_var, ":", mean(subject_aggregated_df[[stat_var]], na.rm = TRUE)))
    # in walk_bout_mean_duration_mins, some has no walk bout and then NA for duration
    # note that some dwell bouts are not NonWalk2_GPS
  }

  print(paste("Number of subjects with walk bout:", sum(subject_aggregated_df$num_Walk1_GPS > 0), "of", nrow(subject_aggregated_df)))

  return(subject_aggregated_df)
}

