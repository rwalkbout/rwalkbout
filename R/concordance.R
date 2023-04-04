## Concordance Table

library(stringr)
library(RPostgreSQL)
library("tidyverse")
library(lubridate)
library(reshape2)

get_merge_data <- function(w_results_path = "~/walkbout_csvs/results/w_results.csv", p_results_path = "~/walkbout_csvs/results/p_results.csv"){
##### Format Weipeng's results #####
w_results <- read.csv(w_results_path) %>% mutate(walk_type = NA)
w_data<-w_results %>%
  mutate(id=str_match(acc_file_path, "acc_(.*?)_baseline")[,2],
         bout_start_time=as.POSIXct(bout_start_time, tz='America/Los_Angeles'),
         bout_end_time=as.POSIXct(bout_end_time, tz='America/Los_Angeles'),
         bout_duration=difftime(bout_end_time+seconds(30), bout_start_time), units='mins',
         walk_type = ifelse(NonWalk1_ACC == 1, 'nonwalk1_acc', walk_type),
         walk_type = ifelse(NonWalk2_GPS == 1 & dwell_bouts == 1, "nonwalk2_gps_dwell", walk_type),
         walk_type = ifelse((NonWalk2_GPS == 1 & (sufficient_GPS_coverage==FALSE | median_speed<2 | median_speed>6)),
                            'nonwalk2_gps_speed', walk_type), # change gps stuff
         walk_type = ifelse(Walk1_GPS==1 & is.na(walk_type), "walk1_gps", walk_type),
         walk_type = ifelse(walk_type == "", 'nonwalk_other', walk_type)) %>%
  select(id, walk_type, cp_mean=mean_accel_count, complete_days, bout_label, sufficient_GPS_coverage,
         dwell_bouts, median_speed, bout_start_time, bout_end_time, bout_duration, mean_speed)

#### Process Phil's results ####
p_results <- read.csv(p_results_path)
# colnames(p_results)[23]<-c("id.2")
p_data<-p_results %>%
  rowwise()%>%
  select('id', walk_type, cpm_mean, complete_days=valid_10h_wearing_day, bout_label=boutnum__1000_cpm__5_mingap__2_minlow,
         sufficient_gps=has_complete_gps_20pct_5pts, mean_speed_kmh, start_time, end_time=time_pa_1000cpm_bout_end) %>%
  mutate(bout_start_time=as.POSIXct(start_time, usetz=TRUE, tz='America/Los_Angeles'),
         bout_end_time=as.POSIXct(end_time, usetz=TRUE, tz='America/Los_Angeles')) %>% select(-start_time,-end_time) %>%
  mutate(bout_duration=difftime((bout_end_time+seconds(30)), bout_start_time, units='mins'))

#### Generate merged data for concordance table ####
#coerce time zone:
  attr(p_data$bout_start_time, 'tzone')<-'America/Los_Angeles'
  attr(p_data$bout_end_time, 'tzone')<-'America/Los_Angeles'
#check time zones # tz(p_data$bout_start_time)

#round down to the nearest 5 minutes for merging
  p_data$start_merge<-floor_date(p_data$bout_start_time, '5 mins')
  w_data$start_merge<-floor_date(w_data$bout_start_time, '5 mins')

#merge all the data
merge_data<-merge(w_data, p_data, by=c('id', 'start_merge'), suffixes=c('.w', '.p'), all=TRUE) %>%
  select(id, start_merge, bout_start_time.w, bout_start_time.p, bout_end_time.w,bout_duration.p,
         bout_start_time.w, bout_end_time.p, bout_duration.w, walk_type.w, walk_type.p, mean_speed_kmh.p=mean_speed_kmh, median_speed.w=median_speed, mean_speed.w = mean_speed)%>%
  mutate(walkbout_w=ifelse(walk_type.w=='walk1_gps', 1, NA),
         walkbout_p=ifelse(walk_type.p=='walk1_gps', 1, NA),
         flag_start=ifelse(bout_start_time.w!=bout_start_time.p, 1, NA),
         flag_end=ifelse(bout_end_time.w!=bout_end_time.p, 1, NA))
return(merge_data)
}

get_concordance_table <- function(merge_data){
#### Concordance Summary ####
concordance <- merge_data %>%
  mutate(walkbout_for_all = ifelse(walkbout_p == 1 & walkbout_w == 1, 1, NA),
         walkbout_p = ifelse(walkbout_p == 1, 1, NA),
         walkbout_p_nonw_w = ifelse(walkbout_p == 1 & walkbout_w != 1, 1, NA),
         walkbout_p_nonw_speed_w = ifelse(walkbout_p == 1 & walk_type.w == "nonwalk2_gps_speed", 1, NA),
         walkbout_p_dwell_w = ifelse(walkbout_p == 1 & walk_type.w == "nonwalk2_gps_dwell", 1, NA),
         walkbout_p_other_w = ifelse(walkbout_p == 1 & walk_type.w == "nonwalk_other", 1, NA),

         walkbout_w = ifelse(walkbout_w == 1, 1, NA),
         walkbout_w_nonw_p = ifelse(walkbout_w == 1 & walkbout_p != 1, 1, NA),
         walkbout_w_nonw_speed_p = ifelse(walkbout_w == 1 & walk_type.p == "nonwalk2_gps_speed", 1, NA),
         walkbout_w_dwell_p = ifelse(walkbout_w == 1 & walk_type.p == "nonwalk2_gps_dwell", 1, NA),
         walkbout_w_other_p = ifelse(walkbout_w == 1 & walk_type.p == "nonwalk_other", 1, NA),
         walkbout_for_none = ifelse(walkbout_p != 1 & walkbout_w != 1, 1, NA)
         ) %>%
  summarise(total_walkbout_for_all = sum(walkbout_for_all, na.rm=TRUE),
            total_walkbout_p = sum(walkbout_p, na.rm=TRUE),
            total_walkbout_p_nonw_w = sum(walkbout_p_nonw_w, na.rm=TRUE),
            total_walkbout_p_nonw_speed_w = sum(walkbout_p_nonw_speed_w, na.rm=TRUE),
            total_walkbout_p_dwell_w = sum(walkbout_p_dwell_w, na.rm=TRUE),
            total_walkbout_p_other_w = sum(walkbout_p_other_w, na.rm=TRUE),
            total_walkbout_w = sum(walkbout_w, na.rm=TRUE),
            total_walkbout_w_nonw_p = sum(walkbout_w_nonw_p, na.rm=TRUE),
            total_walkbout_w_nonw_speed_p = sum(walkbout_w_nonw_speed_p, na.rm=TRUE),
            total_walkbout_w_dwell_p = sum(walkbout_w_dwell_p, na.rm=TRUE),
            total_walkbout_w_other_p = sum(walkbout_w_other_p, na.rm=TRUE)) %>%
  reshape2::melt(.)
total_walkbout_w <- (concordance %>% filter(variable == "total_walkbout_w"))$value
total_walkbout_p <- (concordance %>% filter(variable == "total_walkbout_p"))$value
concordance <- concordance %>%
  mutate(percent = ifelse(str_detect(variable, "total_walkbout_w"), ((value/total_walkbout_w)*100), ((value/total_walkbout_p)*100)),
         percent = percent %>% round(2),
         percent = ifelse(variable == "total_walkbout_for_all", NA, percent),
         percent = ifelse(percent == 100.00, NA, percent)) %>%
  rename(Count = value, Percent = percent)
labels <- c("Walk bout identified by all",

            "Walk bout identified by Phil/Bumjoon (no comparable walkbout identified by WalkboutR)", "Walk bout identified by Phil/Bumjoon, Non-walk bout identified by the WalkboutR package","Walk bout identified by Phil/Bumjoon, Non-walk bout identified by the WalkboutR package due to speed",
            "Walk bout identified by Phil/Bumjoon, Non-walk bout identified by the WalkboutR package due to dwell","Walk bout identified by Phil/Bumjoon, Non-walk bout identified by the WalkboutR package (other)",

            "Walk bout identified by the WalkboutR package (no comparable walkbout identified by Phil/Bumjoon)", "Walk bout identified by the WalkboutR package, Non-walk bout identified by Phil/Bumjoon","Walk bout identified by the WalkboutR package, Non-walk bout identified by Phil/Bumjoon due to speed",
            "Walk bout identified by the WalkboutR package, Non-walk bout identified by Phil/Bumjoon due to dwell","Walk bout identified by the WalkboutR package, Non-walk bout identified by Phil/Bumjoon code (other)")
concordance <- cbind(concordance, "Identified Bouts" = labels) %>%
  relocate("Identified Bouts", .before = "Count") %>%
  select("Identified Bouts", "Count", "Percent")

return(concordance)
}
