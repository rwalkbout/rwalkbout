save_result <- function (processed_result, result_save_folder) {
  splitted_by_slash <- tail(strsplit(processed_result$gps_file_path[1], "/")[[1]], n=1)
  subject_id <- strsplit(splitted_by_slash, "_")[[1]][1]
  file_path <- file.path(result_save_folder, paste0(subject_id, ".csv"))
  processed_result %>% write_csv(file_path)
}
process_many_subject <- function (vector_of_acc_file_path=NULL, vector_of_gps_file_path=NULL, auto_search=FALSE, acc_folder_path=NULL, gps_folder_path=NULL, result_save_folder=NULL) {
  # find pairs of acc and gps files
  if (auto_search) {
    expect_false(is.null(acc_folder_path), "Must specify accelerametry data folder path")
    expect_false(is.null(gps_folder_path), "Must specify GPS data folder path")
    path_all <- get_paths(accelerometry_phase2=acc_folder_path, gps_phase2=gps_folder_path)
    all_acc_file_paths <- path_all %>% filter(ftype=="Accelerometry") %>% pull(path)
    vector_of_acc_file_path <- c()
    vector_of_gps_file_path <- c()
    for (acc_file_path in all_acc_file_paths) {
      gps_file_path <- get_mapped_file(meta_df=path_all, fname=acc_file_path, ftype='GPS')
      if (!is.null(gps_file_path)) {
        vector_of_acc_file_path <- c(vector_of_acc_file_path, acc_file_path)
        vector_of_gps_file_path <- c(vector_of_gps_file_path, gps_file_path)
      }
    }
    expect_equal(length(vector_of_acc_file_path), length(vector_of_gps_file_path))
    print(paste("Number of acc/gps file pairs found:", length(vector_of_acc_file_path)))
  } else {
    expect_true(length(vector_of_acc_file_path) == length(vector_of_gps_file_path))
    expect_false(is.null(vector_of_acc_file_path))
    expect_false(is.null(vector_of_gps_file_path))
    expect_false(auto_search)
  }

  # loop each and run process_one
  list_of_processed_result <- list()
  for (i in seq_along(vector_of_acc_file_path)) {
    acc_file_path <- vector_of_acc_file_path[i]
    gps_file_path <- vector_of_gps_file_path[i]
    print(paste("Processing", vector_of_acc_file_path[i], vector_of_gps_file_path[i]))
    print(paste("Progress:", i, "of", length(vector_of_acc_file_path), "acc/gps pairs"))
    processed_result <- try(process_one_subject(acc_file_path = acc_file_path, gps_file_path = gps_file_path))
    if (class(processed_result) == "try-error")  # time zone problem
      next
    processed_result$acc_file_path <- acc_file_path
    processed_result$gps_file_path <- gps_file_path
    save_result(processed_result = processed_result, result_save_folder = result_save_folder)
    list_of_processed_result[[i]] <- processed_result
  }

  # rbind and return
  summary_table <- do.call(rbind, list_of_processed_result)
  return(summary_table)
}

get_paths <- function (accelerometry_phase2, gps_phase2) {
  # 1. extract paths
  paths <- rbindlist(list(extract_files(folderpath=accelerometry_phase2, pattern="*.csv", ftype='Accelerometry', phase='Phase 2')))  # weipeng edit

  # 2. parse the filenames
  output <- parse_fname(fpaths = paths$path, name_delim=c('\\s+\\(', '\\)', 'DataTable.','\\.'), name_list=c('idno','date_start','inc', 'format'))
  paths <- left_join(paths, output, by="path") %>%
    mutate(inc=gsub('sec','',inc))

  # 3. extract tuples
  output_object <- data.table()
  for (each in 1:length(paths$path)){
    output = parse_meta_tuples(fpath=paths$path[each], skip_nrows=0, parse_nrows=7)
    output_object <- rbindlist(list(output_object, output), use.names=TRUE, fill=TRUE)
  }
  paths <- left_join(paths, output_object, by="path", all=TRUE)

  # 4. extract collapsed rows metadata
  output_object <- data.table()
  for (each in 1:length(paths$path)){
    output = parse_collapsed_rows(fpath=paths$path[each], skip_nrows=7, parse_nrows=1, condensed_delims = '\\: | = ')
    output_object <- rbindlist(list(output_object, output), use.names=TRUE, fill=TRUE)
  }
  paths <- left_join(paths, output_object, by='path', all=TRUE)


  ####### ####### ####### #######
  #
  # extract GPS file metadata
  #
  ####### ####### ####### #######

  # 1. extract paths
  #path1 = data.table(extract_files(folderpath=gps_phase1, pattern="*.csv", ftype='GPS', phase='Phase 1'))
  path2 = data.table(extract_files(folderpath=gps_phase2, pattern="*.csv", ftype='GPS', phase='Phase 2'))

  # 2. parse the filenames
  #path1 (GPS phase 1) doesn't have the same data structure; looks like clones of Accelerometry Phase 1
  #output = parse_fname(fpaths = path1$path, name_delim=c('\\s+\\(', '\\)', '\\DataTable.','\\.'), name_list=c('idno','date_start','inc', 'format'))
  #path1 <- left_join(path1, output, by="path")

  path2 <- parse_fname(fpaths = path2$path,
                       name_delim=c('\\s+\\_', '_CRS.', '_GPS.','\\.'),
                       name_list=c('idno','format')) %>%
    left_join(path2, ., by="path")

  # 3. Extract metadata from sample rows
  output_object <- data.table()
  for (each in 1:length(path2$path)){
    object = extract_metadata_from_file(fpath=path2$path[each], nrows=10)
    output_object <- rbindlist(list(output_object, object), use.names=TRUE, fill=TRUE)
  }

  # if clause to prevent recurrent left_joins
  if (!'TRACK.ID' %in% colnames(path2)){
    path2 <- left_join(path2, #>#
                       output_object %>%
                         mutate(date_start = as.character(date_start),
                                inc = as.character(inc)),  # drop date datatype
                       by='path', all=TRUE)
  }

  # compare files and idno mappings
  path_all = rbindlist(list(paths,
                            #path1 %>% select(path, ftype,phase,fname,idno, format), # GPS Phase 1 are clone files
                            path2), use.names=TRUE, fill=TRUE) %>%
    mutate(ftype_phase = paste0(ftype, " ", phase))

  return(path_all)
}