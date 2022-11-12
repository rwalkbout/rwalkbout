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

  ## put together date_time and count
  acc_data <- count_df %>% add_column(date_time = date_time_column, .before = 1)

  ## interpolate 30 epoch period to 15 epoch period
  interpolated_count <- numeric(2*length(count_df$count))
  interpolated_count[c(TRUE, FALSE)] = floor(count_df$count/2)
  interpolated_count[c(FALSE, TRUE)] = ceiling(count_df$count/2)

  interpolated_date_time_column <- seq(from=start_date_time, length.out = num_time_stamp*2, by = epoch_period/2, tz=time_zone)
  acc_data <- tibble(date_time = interpolated_date_time_column, count = interpolated_count)

  return(acc_data)
}
