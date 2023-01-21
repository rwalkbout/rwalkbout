source("setup.R")
library("measurements")

## configs
time_zone <- 'America/Los_Angeles'
acc_folder_path <- '/projects/trac/data/acc'
gps_folder_path <- '/projects/trac/data/gps'
result_save_folder <- "~/walkbout_results/test/trac" # TO DO: Make results directory if it doesn't exist


## 1. process all subjects and store the result
# pull file paths:
file_mapper_result <- seattle_baseline_1_file_mapper(acc_folder_path=acc_folder_path, gps_folder_path=gps_folder_path)
# for testing:
subject_subset <- c()
# subject_subset <- c("10100052_baseline_1", "14832802_baseline_1")
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









