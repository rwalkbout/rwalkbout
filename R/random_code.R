source("setup.R")
time_zone <- 'America/Los_Angeles'

#' process one file
#acc_file_path <- '/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/test/trac/acc/acc_10100052_baseline_1.csv'
#gps_file_path <- '/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/test/trac/gps/gps_10100052_baseline_1.csv'
#acc_file_path <- "/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/test/trac/acc//acc_10100341_baseline_1.csv"
#gps_file_path <- "/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/test/trac/gps//gps_10100341_baseline_1.csv"
#acc_file_path <- "/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/trac/acc//acc_12015817_baseline_1.csv"
#gps_file_path <- "/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/trac/gps//gps_12015817_baseline_1.csv"
acc_file_path <- "/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/trac/acc//acc_12915257_baseline_1.csv"
gps_file_path <- "/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/trac/gps//gps_12915257_baseline_1.csv"

process_one_subject(time_zone = time_zone, acc_file_path = acc_file_path, gps_file_path = gps_file_path, acc_file_reader = seattle_baseline_1_acc_file_reader, gps_file_reader = seattle_baseline_1_gps_file_reader)

#' create acc and gps file reader
acc_file_path <- '/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/test/trac/acc/acc_10100052_baseline_1.csv'
gps_file_path <- '/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/test/trac/gps/gps_10100052_baseline_1.csv'
#seattle_baseline_1_acc_file_reader(acc_file_path)
#seattle_baseline_1_gps_file_reader(gps_file_path)

#' create file mapper
#acc_folder_path <- '/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/test/Actigraphy/Phase2'
#gps_folder_path <- '/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/test/GPS/Phase2'
#acc_folder_path <- '/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/test/trac/acc/'
#gps_folder_path <- '/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/test/trac/gps/'
acc_folder_path <- '/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/trac/acc/'
gps_folder_path <- '/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/trac/gps/'

#file_mapper_result <- seattle_baseline_1_file_mapper(acc_folder_path=acc_folder_path, gps_folder_path=gps_folder_path)
#process_many_subject(vector_of_acc_file_path = file_mapper_result$acc_file_path, vector_of_gps_file_path = file_mapper_result$gps_file_path, vector_of_subject_id = file_mapper_result$subject_id, acc_file_reader = seattle_baseline_1_acc_file_reader, gps_file_reader = seattle_baseline_1_gps_file_reader, result_save_folder="/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/result/test/trac", time_zone="America/Los_Angeles")

#' baseline_1: 11 acc, 1 gps, 720 acc-gps
#' baseline_2: 1 acc, 1 gps, 48 acc-gps
#' post1_1: 10 acc, 4 gps, 587 acc-gps
#' post2_1: 22 acc, 2 gps, 545 acc-gps
