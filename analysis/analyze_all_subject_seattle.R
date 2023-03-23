setwd("~/repos/rwalkbout/R/")
source("setup.R")
source("util.R")
library("measurements")

## configs
time_zone <- 'America/Los_Angeles'
acc_folder_path <- '/projects/trac/data/acc'
gps_folder_path <- '/projects/trac/data/gps'
result_save_folder <- "~/walkbout_results/test/trac"
if(!(dir.exists(result_save_folder))){
  dir.create(file.path(results_save_folder))}

## 1. process all subjects and store the result
# pull file paths:
file_mapper_result <- seattle_baseline_1_file_mapper(acc_folder_path=acc_folder_path, gps_folder_path=gps_folder_path)
# for testing:
  subject_subset <- c()
  # subject_subset <- c("10902186_baseline_1",  "10705609_baseline_1")
  # "10100725_baseline_1", "14835176_baseline_1", "14830788_baseline_1", "13322435_baseline_1","10101700_baseline_1", "11516771_baseline_1",
  # "10902186_baseline_1", "10704259_baseline_1", "14427353_baseline_1", "10300768_baseline_1","11613715_baseline_1", "11516901_baseline_1",
  # "10102248_baseline_1", "10800490_baseline_1","10101329_baseline_1",
if(length(subject_subset)>0){
  file_mapper_result <- file_mapper_result[file_mapper_result$subject_id %in% subject_subset,]
}

# run walkbout code:
path_result <- process_many_subject(vector_of_acc_file_path = file_mapper_result$acc_file_path,
                                    vector_of_gps_file_path = file_mapper_result$gps_file_path,
                                    vector_of_subject_id = file_mapper_result$subject_id,
                                    acc_file_reader = seattle_baseline_1_acc_file_reader,
                                    gps_file_reader = seattle_baseline_1_gps_file_reader,
                                    result_save_folder=result_save_folder,
                                    time_zone=time_zone)

## 2. read the processed results, do analysis and output summary
result_df <- read_result_save_folder_as_dataframe(result_save_folder)
subject_aggregated_df <- result_df %>%
  group_by(subject_id) %>%
  group_modify(get_subject_aggragtes)




