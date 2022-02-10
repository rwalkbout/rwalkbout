
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
