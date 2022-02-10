process_gps <- function(datadir_gps){
  
  csvfnames_gps <- list.files(path = datadir_gps, pattern = "*.csv", recursive = TRUE, full.names = TRUE)
  
  #identify id number as beginning at the first position in each file name after the end of the provided file path
  ids_gps <- str_sub(csvfnames_gps, start = nchar(datadir_gps) + 2, end = nchar(csvfnames_gps[1])) %>%
    toupper() %>% #coerce to uppercase for string matching/replacement
    str_replace_all("_GPS.CSV", "") %>% #drop suffix after idno
    str_replace_all("_CRS.CSV", "") #alternative suffix (n = 1 in Rails and Health data)
  
  #create copy of bout data frame
  
  bout_data <- phase_2_bouts
  
  #loop through each gps file
  # for(i in 1:length(csvfnames_gps)){
  for(i in 1:length(csvfnames_gps)){  
    
    #pull the idno for the record in question
    id <- ids_gps[i]
    
    #create empty vector of ids with no bouts
    id_no_bout <- NULL
    
    #only attempt to join gps to bout data if individuals had valid bouts
    if(id %in% phase_2_bouts$idno){
      
      gps_data_temp <- read_csv(csvfnames_gps[i]) %>%
        mutate("datetime" = ymd_hms(paste(`LOCAL DATE`, `LOCAL TIME`)))
      
      
      gps_data_temp$idno <- id
      
      bout_data_temp <- bout_data %>%
        filter(idno == id) 
      
      #loop through bouts to id all gps points within a bout
      for(ii in 1:nrow(bout_data_temp)){
        
        period_start <- bout_data_temp$begin_datetime[ii]
        
        period_end <- bout_data_temp$end_datetime[ii]
        
        gps_points <- gps_data_temp %>%
          filter(datetime >= period_start & datetime <= period_end)
        
        gps_bout_join_temp <- gps_points %>%
          left_join(bout_data_temp[ii,], by = "idno")
        
        #if the end gps bout level data set has not been created yet, create it
        if(!exists("gps_bout_join")){
          
          gps_bout_join <- gps_bout_join_temp
          
        } 
        #end if(!exists("gps_bout_join"))
        
        #if the end gps bout level data set has been created, bind rows
        if(exists("gps_bout_join")){
          
          gps_bout_join <- gps_bout_join %>%
            bind_rows(gps_bout_join_temp)
          
        } 
        #end if(exists("gps_bout_join"))
        
      } 
      #end for loop through individual's bouts
        
        
    } 
    #end if idno exists in bout data
     
    
    #CONTINGENCY NEEDED FOR THOSE WITH NO VALID BOUTS?
    if(!(id %in% phase_2_bouts$idno)){
      
      id_no_bout <- c(id_no_bout, id)
      
    } 
    #end if(!(id %in% phase_2_bouts$idno))
    
  } 
  #end for loop through each csv
  
  assign("gps_bout_join", gps_bout_join, envir = .GlobalEnv)
  
} 
#end function