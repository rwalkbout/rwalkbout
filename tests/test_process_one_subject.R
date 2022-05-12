
# if called by tester, the working directory should be ../R
setwd("../R")
source("setup.R")

#' Testing process_file.R
test_that("Subject 100021 processing correct", {
  p1 <- "/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/test/Actigraphy/Phase2/100021 (2016-12-10)15sec.csv"
  q1 <- "/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout/data/test/GPS/Phase2/100021_GPS.csv"
  summary_with_start_end_date <- process_one_subject(acc_file_path = p1, gps_file_path = q1)

  expect_equal(sum(summary_with_start_end_date$NonWalk2_GPS, na.rm = TRUE), 12)
  expect_equal(summary_with_start_end_date$mean_accel_count[7], 614.6)
  expect_equal(summary_with_start_end_date$max_accel_count[2], 1433)
})

