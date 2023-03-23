get_speed_table <- function(merge_data){
  gps_speed<-merge_data %>% filter(!str_detect(walk_type.p, "diary")) %>% 
    mutate(median_speed.w = round(median_speed.w, 1),
           mean_speed.w = round(mean_speed.w, 1),
           speed_flag = ifelse(mean_speed_kmh.p == median_speed.w & is.na(flag_start) & is.na(flag_end), "mean.p = median.w",""),
           speed_flag = ifelse(mean_speed_kmh.p == mean_speed.w & is.na(flag_start) & is.na(flag_end) & speed_flag!= "mean.p = median.w", "mean.p = mean.w", speed_flag),
           speed_flag = ifelse(mean_speed_kmh.p == mean_speed.w & mean_speed_kmh.p == median_speed.w & is.na(flag_start) & is.na(flag_end), "mean.p = median.w = mean.w", speed_flag),
           speed_flag = ifelse(speed_flag == "", "no speed metrics are equivalent", speed_flag)) %>% 
    filter(walk_type.w != walk_type.p) %>% 
    filter(!(str_detect(walk_type.p, "unknown"))) 
  gps_speed_table <- gps_speed %>% 
    select(walk_type.w, walk_type.p, speed_flag) %>% 
    group_by(walk_type.w, walk_type.p, speed_flag) %>% 
    summarise(count = n()) %>% 
    mutate(percent=(count/nrow(gps_speed))*100, 
           percent = round(percent, 2)) %>% 
    arrange(speed_flag)
  return(gps_speed_table)
} 
