## create a subject's mapping between accelerometry and GPS's file path

portland_file_mapper <- function (acc_folder_path='/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/test/Actigraphy/Phase2', gps_folder_path='/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/test/GPS/Phase2') {
  print(10)

  # read acc files, extract the subject id
  acc_subject_ids <- gsub("() \\(.*\\).*", "\\1", list.files(acc_folder_path))
  acc_file_paths <- list.files(acc_folder_path, full.names = TRUE)
  acc_df <- tibble(subject_id=acc_subject_ids, acc_file_path=acc_file_paths)

  # read gps files, extract the subject id
  gps_subject_ids <- gsub("(.*)_GPS.csv", "\\1", list.files(gps_folder_path))
  gps_file_paths <- list.files(gps_folder_path, full.names = TRUE)
  gps_df <- tibble(subject_id=gps_subject_ids, gps_file_path=gps_file_paths)

  # ensure no duplicates of subject id in acc and gps
  expect_true(length(unique(acc_subject_ids)) == length(acc_subject_ids))
  expect_true(length(unique(gps_subject_ids)) == length(gps_subject_ids))

  # do outter join
  outer_subject_file_mapping_df <- full_join(acc_df, gps_df, by="subject_id")
  acc_unmatched_df <- outer_subject_file_mapping_df[is.na(outer_subject_file_mapping_df$gps_file_path), ]
  gps_unmatched_df <- outer_subject_file_mapping_df[is.na(outer_subject_file_mapping_df$acc_file_path), ]
  valid_subject_file_mapping_df <- na.omit(outer_subject_file_mapping_df)

  # print unmatched ACC and GPS subjects
  print(acc_unmatched_df)
  print(gps_unmatched_df)
  print(valid_subject_file_mapping_df)
  print(sprintf("Number of ACC subjects with no GPS mapping: %d", nrow(acc_unmatched_df)))
  print(sprintf("Number of GPS subjects with no ACC mapping: %d", nrow(gps_unmatched_df)))
  print(sprintf("Number of matched subjects: %d", nrow(valid_subject_file_mapping_df)))

  return(valid_subject_file_mapping_df)
}

seattle_baseline_1_file_mapper <- function (acc_folder_path='/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/test/trac/acc/', gps_folder_path='/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/test/trac/gps/') {

  phase <- "baseline_1"
  #phase <- "baseline_2"
  #phase <- "post1_1"
  #phase <- "post2_1"

  # read acc files, substract the subject id
  acc_file_paths <- list.files(acc_folder_path, full.names = TRUE)
  acc_file_paths <- acc_file_paths[str_detect(acc_file_paths, phase)] # include baseline_1 only
  acc_subject_ids <- gsub("(acc|gps)_(.*)\\.(.*)", "\\2", basename(acc_file_paths))
  acc_df <- tibble(subject_id=acc_subject_ids, acc_file_path=acc_file_paths)

  # read gps files, substract the subject id
  gps_file_paths <- list.files(gps_folder_path, full.names = TRUE)
  gps_file_paths <- gps_file_paths[str_detect(gps_file_paths, phase)] # include baseline_1 only
  gps_subject_ids <- gsub("(acc|gps)_(.*)\\.csv", "\\2", basename(gps_file_paths))
  gps_df <- tibble(subject_id=gps_subject_ids, gps_file_path=gps_file_paths)

  # ensure no duplicates of subject id in acc and gps
  expect_true(length(unique(acc_subject_ids)) == length(acc_subject_ids))
  expect_true(length(unique(gps_subject_ids)) == length(gps_subject_ids))

  # do outter join
  outer_subject_file_mapping_df <- full_join(acc_df, gps_df, by="subject_id")
  acc_unmatched_df <- outer_subject_file_mapping_df[is.na(outer_subject_file_mapping_df$gps_file_path), ]
  gps_unmatched_df <- outer_subject_file_mapping_df[is.na(outer_subject_file_mapping_df$acc_file_path), ]
  valid_subject_file_mapping_df <- na.omit(outer_subject_file_mapping_df)

  # print unmatched ACC and GPS subjects
  print(acc_unmatched_df)
  print(gps_unmatched_df)
  print(valid_subject_file_mapping_df)
  print(sprintf("Number of ACC subjects with no GPS mapping: %d", nrow(acc_unmatched_df)))
  print(sprintf("Number of GPS subjects with no ACC mapping: %d", nrow(gps_unmatched_df)))
  print(sprintf("Number of matched subjects: %d", nrow(valid_subject_file_mapping_df)))
  # moving this from main script to happen within this function [LBW 11/12: not sure why this is needed, to do: investigate]
  valid_subject_file_mapping_df <- valid_subject_file_mapping_df[!(valid_subject_file_mapping_df$subject_id %in% c("13421835_baseline_1", "12915257_baseline_1")), ]

  return(valid_subject_file_mapping_df)
}
