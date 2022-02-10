identify_bouts <- function(datadir, out_file, n_col = 12, return_df = F){
  
  #install packages if not installed
  #load packages
  if (!require(tidyverse)) install.packages('tidyverse')
  library(tidyverse)
  
  if(!require(lubridate)) intall.packages('lubridate')
  library(lubridate)

#get list of csv file names
  csvfnames <- list.files(path = datadir, pattern = "*.csv", recursive = TRUE, full.names = TRUE)
  
  all_bouts_df <- tibble("Sum_Duration" = NA,
                         "idno" = NA,
                         "Date" = NA)
  
  bouts_person_df <- tibble("idno" = NA,
                            "N_Bouts" = NA,
                            "Rows" = NA,
                            "Wear_Time_Seconds" = NA,
                            "Wear_Time_HMS" = as.period(NA),
                            "Run_Time" = NA,
                            "Valid_Days" = NA,
                            "flag" = NA)
  
  # for(i in 1:length(csvfnames)){
  for(i in 1:length(csvfnames)){
    
    #print the row number for debugging
    print(i)
   
    time1 <- Sys.time()
  
  #read in one file and identify walk bouts
    data1 <- read_csv(csvfnames[i], skip = 10)
    
    #identify epoch length from meta data (line 5 of standard output format)
    data1_epoch <- read_csv(csvfnames[i], skip = 4, n_max = 1, col_names = "epoch") %>%
      pull(epoch) %>%
      str_sub(start = 25, end = 32) %>%
      hms() %>%
      as.numeric()
    
    
    
    
    #pull idno from filename
    id <- str_sub(csvfnames[i], 100, 105)
    
    # bouts_person_df$idno[i] <- id
    
    #flag if the number of columns is different than expected
    if(ncol(data1) != n_col) {warning(paste0("Participant ", id, ", row ", i, " data format unexpected; record dropped"))} 
    
    #only execute code if proper number of columns
    #using two if statements rather than if...else due to the latter being finicky
    if(ncol(data1) == n_col){
    
    #1. Collapse to 30 second epochs
    
    #a. Create lead variable and sum current row and lead cpe to create running 30 second total
    
        if(data1_epoch == 15){
        
          data1 <- data1 %>%
          mutate("lead_Axis1" = lead(Axis1),
                 "Axis1_totl30" = Axis1 + lead_Axis1)
          
        }
          
        if(data1_epoch == 10){
          
          data1 <- data1 %>%
            mutate("lead1_Axis1" = lead(Axis1),
                   "lead2_Axis1" = lead(Axis1, n = 2),
                   "Axis1_totl30" = Axis1 + lead1_Axis1 + lead2_Axis1)
          
        }  
    
    #check
    # min_ind <- min(which(data1$Axis1 > 0))
    
    
    # *Check*
      
    # 
    # data1 %>%
    #   select(Time, Axis1, lead_Axis1, Axis1_totl30) %>%
    #   slice(min_ind, min_ind+1)
    
    
    #b. Subset to keep only rows whose time is a multiple of 30 seconds
    
    #create seconds variable
    data1 <- data1 %>%
      mutate("time_sec" = as.duration(Time)) %>%
      filter(as.numeric(time_sec)%%30 == 0) #keep only rows where time is multiple of 30s
    
    #create threshold indicator for counts/30s epoch > 500
    data1 <- data1 %>%
      mutate("ovr500" = Axis1_totl30 > 500)
    
    
    #2. Remove blocks of zeros >= 20 minutes
    
    #a. categorize as Active/Non_Active based on Axis1  > 0
    data1 <- data1 %>%
      mutate("Active" = case_when(Axis1_totl30 > 0 ~ "Active",
                                  Axis1_totl30 == 0 ~ "Non_Active") %>% factor())
    
    
    #b. Run length encoding on clusters of activity/inactivity
    
    rle_active <- rle(as.numeric(data1$Active))
    
    #c. Convert output of rle to data frame to use to create indices for subsetting data
    
    # *Cumulative sum of run lengths (from run length encoding) gives the index of the end of each run. The index for the beginning of each run can be found by the end of the previous run plus 1*
      
    #convert rle output to data frame
    rle_df <- tibble("values" = rle_active$values,
                     "Active" = factor(values,
                                       labels = c("Active", "Inactive")),
                     "lengths" = rle_active$lengths,
                     "cumul_lgth" = cumsum(lengths),
                     #replace NA with 1 as only NA will be in row 1 since no previous row for lag function
                     "begin" = replace_na(lag(cumul_lgth) + 1, replace = 1),
                     "end" = cumul_lgth,
                     "duration" = end - begin + 1)
    
    #d. Create a vector of rows than meet the criteria to be removed (inactive run length of over 40 rows/20 minutes)
    indices <- rle_df %>%
      filter(values == 2 & duration > 40) %>%
      select(begin, end)
    
      
      
    #e. Loop through rle data frame to fill a vector of values to remove defined by the beginning and end index of each row
    to_remove <- NULL
    
    for(q in 1:length(indices$begin)){
      beg_ <- indices$begin[q]
      end_ <- indices$end[q]
      values_ <- beg_:end_
      # print(values_)
      to_remove <- c(to_remove, values_)
    }
    
    data1$remove_20min <- "Keep"
    data1$remove_20min[to_remove] <- "Remove"
    
    #f. create subset data frame after removing inactive periods
    data2 <-  data1 %>%
      filter(remove_20min == "Keep")
    
    #g. Keep only days with at least 8hr of wear time after removing 20 min runs of inactivity
    
    #create count of (valid) epochs/rows per day
    date_counts <- data2 %>%
      group_by(Date) %>%
      tally()
    
    #further subset by days with >= 960 rows (8 hours)
    valid_dates <- date_counts %>%
      filter(n >= 960)
    
    #subset data frame by valid dates (>= 8hrs of activity)
    data3 <- data2 %>%
      filter(Date %in% valid_dates$Date)
    
    
    
    
    ###Use rle to identify runs of activity >= 500 cpe threshold
    
    #identify bouts based on five minute consecutive epochs >= 500 cpe
    rle_over500 <- rle(data3$ovr500)
    rle_over500$values[rle_over500$values == "TRUE"] <- "Walking"
    rle_over500$values[rle_over500$values == "FALSE"] <- "No_Walk"
    
    #only run rest of code if there is at least 1 run of physical activity (>= 500cpe)
    # rle values > 1 because first entry will be nonactivity ("No_Walk"); > 1 requires at least 1 entry of "Walking"
    if(length(rle_over500$values) > 1){
    
    #Create data frame from output of rle, include cumulative sum (length), and beginning/end row index of each run
    rle_dfbouts <- tibble("Value" = rle_over500$values,
                          "Length" = rle_over500$lengths,
                          "cumul_lgth" = cumsum(Length),
                          "begin" = replace_na(lag(cumul_lgth) + 1, replace = 1),
                          "end" = cumul_lgth)
    
    # Identify bouts by time rather than consecutive rows
    rle_dfbouts$Date <- data3$Date[rle_dfbouts$begin]
    rle_dfbouts$Time_begin <- data3$Time[rle_dfbouts$begin]
    rle_dfbouts$Time_end <- data3$Time[rle_dfbouts$end]
    rle_dfbouts$Duration <- (rle_dfbouts$Time_end - rle_dfbouts$Time_begin + 30)
    rle_dfbouts$lag_end <- lag(rle_dfbouts$Time_end)
    
    
    rle_walkbouts <- rle_dfbouts %>%
      filter(Value == "Walking") %>%
      mutate("lag_end" = lag(Time_end),
             "Downtime_before" = Time_begin - lag_end + 30,
             "run_id" = row_number())
    
    
    #Add cumulative duration for each bout, resetting when cumulative downtime = 0
    #set downtime_before to 0 for first row
    rle_walkbouts$Downtime_before[1] <- 0
    
    rle_walkbouts$cumul_duration <- 0
    
    #set cumul_duration to Duration for first row
    rle_walkbouts$cumul_duration[1] <- rle_walkbouts$Duration[1]
    
    #Create a counter for bouts - each new bout adds 1
    rle_walkbouts$bout_count <- 1
    
    if(nrow(rle_walkbouts) >= 2){
      for(ii in 2:nrow(rle_walkbouts)){
      
        rle_walkbouts$cumul_duration[ii] <- ifelse(rle_walkbouts$Downtime_before[ii] > 120,
                                                rle_walkbouts$Duration[ii],
                                                rle_walkbouts$Duration[ii] + rle_walkbouts$cumul_duration[ii - 1])
      
        rle_walkbouts$bout_count[ii] <- ifelse(rle_walkbouts$Downtime_before[ii] > 120,
                                            rle_walkbouts$bout_count[ii - 1] + 1,
                                            rle_walkbouts$bout_count[ii - 1])
      
            }
          }
          
    
    
    time2 <- Sys.time()
    
    
    potential_bouts <- rle_walkbouts %>%
      group_by(bout_count) %>%
      summarise("Begin" = seconds_to_period(min(Time_begin)),
                "End" = seconds_to_period(max(Time_end)),
                "Sum_Duration" = sum(Duration),
                "Date" = Date)
    
    valid_bouts_df <- potential_bouts %>%
      filter(Sum_Duration >= 300) %>%
      mutate("idno" = id) %>%
      select(Sum_Duration, idno, Begin, End, Date)
    
    # all_bouts_df <- tibble("Sum_Duration" = NA,
    #                        "idno" = NA,
    #                        "Date" = NA,
    #                        "Time_Begin" = NA,
    #                        "Time_End" = NA)
    
    
    time2 <- Sys.time()
    
    elapsed <- time2 - time1
    
    #add to bout level data set
    all_bouts_df <- all_bouts_df %>%
      bind_rows(valid_bouts_df)
    
    #create individual person level data
    valid_bouts_total <- valid_bouts_df %>%
      group_by(idno) %>%
      summarise("N_Bouts" = nrow(.)) %>%
      mutate("Rows" = nrow(data3),
             "Wear_Time_Seconds" = nrow(data3) * 30,
             "Wear_Time_HMS" = seconds_to_period(Wear_Time_Seconds),
             "Run_Time" = elapsed,
             "Valid_Days" = nrow(valid_dates))
    
    #add to person level data set
    bouts_person_df <- bouts_person_df %>%
      bind_rows(valid_bouts_total)
    
        } #closing if statment (length(rle500$values) > 1)
    
    #logic for entries with insufficient walk time to process
    if(length(rle_over500$values) <= 1){ 
      time2 <- Sys.time() 
      
      elapsed <- time2 - time1
      
      bouts_person_temp <- tibble("idno" = id,
                                  "N_Bouts" = 0,
                                  "Rows" = nrow(data3),
                                  "Wear_Time_Seconds" = nrow(data3) * 30,
                                  "Wear_Time_HMS" = seconds_to_period(Wear_Time_Seconds),
                                  "Run_Time" = elapsed,
                                  "Valid_Days" = nrow(valid_dates))
      
      bouts_person_df <- bouts_person_df %>%
        bind_rows(bouts_person_temp)
      
       }
    
      } #end if statement for ncols
    
    } #end for loop - individual csv processing

  # save(bouts_person_df, all_bouts_df, file = here("data", "20210314-prelim_results.rda"))
  
  #create output
  save(bouts_person_df, all_bouts_df, file = out_file)
  
  if(return_df == T){
    assign("bouts_person_df", bouts_person_df, envir = .GlobalEnv)
    
    assign("all_bouts_df", all_bouts_df, envir = .GlobalEnv)
  }

} #end function

