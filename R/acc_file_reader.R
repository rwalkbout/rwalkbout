## customized acc file reader for differnet acc file format
#' Portland Acceletrometry reader
portland_acc_file_reader <- function (acc_file_path) {
  acc_data <-
    read.csv(acc_file_path, skip = 10, stringsAsFactors = FALSE) %>%
    data.table() %>%
    fix_date_time(., date_field="Date", time_field="Time", date_format="%m/%d/%Y", time_format="%H:%M:%S", time_zone=time_zone, data_source="ACC") %>% # generates a field called DateTime
    select(DateTIME, Axis1) %>%
    rename(date_time=DateTIME, count=Axis1) %>%
    as.tibble()
  return(acc_data)
}

#' Seattle Acceletrometry reader
seattle_baseline_1_acc_file_reader <- function (acc_file_path) {
  ## read count column
  count_df <- read_csv(file=acc_file_path, skip = 10, show_col_types=FALSE, col_names = c("count", "other1", "other2")) %>% select(count)
  time_zone <- "America/Los_Angeles"
  browser()

  ## make date_time column
  file_header <- read_csv(file=acc_file_path, n_max = 9, show_col_types=FALSE, col_names = 'header')
  #epoch_period <- as.integer(gsub(".* .* \\(.*\\) 00:00:(.*)", "\\1", file_header$header[5]))
  if (file_header$header[5] == "Epoch Period (hh:mm:ss) 00:00:30") {
    epoch_period <- 30
  } else {
    stop(paste("Unsupported epoch period:", file_header$header[5]))
  }
  start_date <- gsub(".* .* (.*)", "\\1", file_header$header[4])
  start_time <- gsub(".* .* (.*)", "\\1", file_header$header[3])
  start_date_time <- as.POSIXct(paste(start_date, start_time), format="%m/%d/%Y %H:%M:%OS", tz=time_zone)
  num_time_stamp <- nrow(count_df)
  date_time_column <- seq(from=start_date_time, length.out = num_time_stamp, by = epoch_period, tz=time_zone)

  acc_data <- tibble(
    epoch_time = format(date_time_column, format='%Y-%m-%d %H:%M:%S', tz=time_zone, usetz=TRUE),
    Axis1_epochSum = count_df$count,
    Activity = 'Low active'
  )
  acc_data <- as.data.table(acc_data)
  acc_data <- acc_data[Axis1_epochSum==0, Activity := 'Non_active']
  acc_data <- acc_data[Axis1_epochSum > refvalues$min_pa_cpe, Activity := 'Active']
  acc_data$Activity <- as.factor(acc_data$Activity)

  return(acc_data)
}
