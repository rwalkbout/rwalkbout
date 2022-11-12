## customized gps file reader for differnet acc file format
#' Portland GPS reader
portland_gps_file_reader <- function (gps_file_path) {
  gps_data <-
    read.csv(gps_file_path, stringsAsFactors = FALSE) %>%
    fix_date_time(., date_field="LOCAL.DATE", time_field="LOCAL.TIME", date_format="%Y/%m/%d", time_format="%H:%M:%S", time_zone=time_zone, data_source="GPS") %>% # generates a field called DateTime
    correction_for_cardinality(gps_data=., coordinate_field='LATITUDE', cardinality_field='N.S') %>%
    correction_for_cardinality(gps_data=., coordinate_field='LONGITUDE', cardinality_field='E.W') %>%
    select(LOCAL.DATETIME, LATITUDE_coord, LONGITUDE_coord, SPEED) %>%
    rename(date_time=LOCAL.DATETIME, latitude=LATITUDE_coord, longitude=LONGITUDE_coord, speed=SPEED) %>%
    as.tibble()
  return(gps_data)
}

#' Seattle GPS reader
seattle_baseline_1_gps_file_reader <- function (gps_file_path) {
  input_df <- read_csv(file=gps_file_path, skip = 1, col_names = c("record_index", "date", "time", "latitude", "longitude", "speed", "altitude"))

  ## make latitude/longitude
  latitude <- input_df$latitude/100
  longitude <- input_df$longitude/100

  ## make speed
  speed_type <- str_split(read_lines(file=gps_file_path, n_max=1), pattern = ",")[[1]][6]
  if (speed_type == "Speed(km/hour)") {
    speed <- input_df$speed
  } else if (speed_type == "Speed(mile/hour)") {
    speed <- input_df$speed * 1.609
  } else {
    stop("Unknown speed unit")
  }

  ## make date time
  date_sample <- input_df$date[1]
  if (str_detect(date_sample, "-")) {
    date_time_format <- "%Y-%m-%d %H:%M:%OS"
  } else if (str_detect(date_sample, "/")) {
    date_time_format <- "%m/%d/%Y %H:%M:%OS"
  } else {
    stop("Unknown date format")
  }
  date_time_string_vector <- paste(input_df$date, input_df$time)
  source_time_zone <- "America/Los_Angeles"
  target_time_zone <- "America/Los_Angeles"
  date_time <- with_tz(as.POSIXct(date_time_string_vector, format=date_time_format, tz=source_time_zone), tzone = target_time_zone)

  ## make gps data
  gps_data <- tibble(date_time=date_time, latitude=latitude, longitude=longitude, speed=speed)

  ## drop na rows (e.g. 3703 row of ~/Desktop/retrieved_backup/epiProject/walk_bout/data/trac/gps/gps_12015817_baseline_1.csv)
  gps_data <- drop_na(gps_data)

  # moving this from main script to happen within this function [LBW 11/12: not sure why this is needed, to do: investigate]
  gps_data <- gps_data[!(gps_data$subject_id %in% c("13421835_baseline_1", "12915257_baseline_1")), ]

  return(gps_data)
}
