# unit-test 1: detect files
detect_files <- function(folder){
  expect_true(file.exists(folder))
  expect_equal(length(folder),1)
}

#detect_files(accelerometry_phase2)
#detect_files(gps_phase2)

# unit-test 2: detect directory
detect_dir <- function(folder){
  expect_true(length(strsplit(folder, split = '/')[[1]])>1)
}

#detect_dir(accelerometry_phase2)
#detect_dir(gps_phase2)

# unit test 3: detect csv with metadata leading rows
detect_leading_metadata <- function(directory){
  p = list.files(directory)[1]
  expect_equal(ncol(read.csv(file.path(directory,p), nrows=10)),1)
}

detect_no_leading_metadata <- function(directory){
  p = list.files(directory)[1]
  expect_false(ncol(read.csv(file.path(directory,p), nrows=10))==1)
}

#detect_leading_metadata(accelerometry_phase2)
#detect_no_leading_metadata(gps_phase2)


####### ####### #######
#
# metadata functions
#
####### ####### #######

# applied on folders of files
extract_files <- function(folderpath, pattern="*.csv", ftype='Accelerometry', phase='Phase 1'){
  #
  # generate list of files from folder path
  #
  # folderpath (dir): the path to a folder of files
  # pattern (str): pattern to recognize valid files within the folder, e.g., file formats
  # ftype (str): keyword for the folder
  # phase (str): keyword for the phase

  meta_tab <- list.files(path = folderpath, pattern = pattern, recursive = TRUE, full.names = TRUE) %>%
    data.table(path = ., ftype = ftype, phase = phase)
  return(meta_tab)
}


# applied on all csv files
parse_fname <- function(fpaths, name_delim=c('\\s+\\(', '\\)', '\\DataTable.','\\.'), name_list=c('idno','date_start','inc', 'format')){
  #
  # extract metadata 1: metadata from filename
  #
  # fpaths (list of dir): the file path to be wrangled
  # name_delim (list of str): list of character patterns for splitting the filename
  # name_list (list of str): the naming convention for each of the filename components

  # init
  output_object <- data.table()
  name_delim <- paste0(name_delim, collapse='|')

  # iterate through all files
  for (ind in 1:length(fpaths)){
    # extract filename to list
    fname = tail(unlist(strsplit(fpaths[ind],'/')),1)
    fpath <- c(fpaths[ind], fname)
    names(fpath) <- c('path','fname')

    # parse metadata from fname to list
    fname_parsed <- unlist(strsplit(fname, name_delim))
    tryCatch(names(fname_parsed) <- name_list)

    # generate dictionary
    output_object <- rbindlist(list(output_object, rbind(c(fpath, fname_parsed)) %>% data.table()))
  }
  return(output_object)
}


# applied on Accelerometry files
parse_meta_tuples <- function(fpath, skip_nrows=0, parse_nrows=7, param_delim=c("\\:\\s+","\\ ")) {
  #
  # extract metadata 2: row-wise tuples
  #
  # fpath (dir): the file path to be wrangled
  # skip_nrows (integer): the number of rows to skip until metadata content, assuming headers are present
  # parse_nrows (integer): the number of rows containing metadata in row-wise tuples
  # param_delim (list of str): the delimiters usable for separating metadata name and metadata value

  # init
  object <- data.table(path=fpath)
  ac_meta <- read.csv(file=fpath, skip=skip_nrows, nrows = parse_nrows, col.names = "params")

  # loop through tuples
  for (each in ac_meta$params){
    fragment <- unlist(strsplit(each, split=paste0(param_delim, collapse="|")))
    object[1, paste0(head(fragment,-1), collapse='_'):=tail(fragment,1)]
  }
  return(object)
}


# applied on Accelerometry files
parse_collapsed_rows <- function(fpath, skip_nrows, parse_nrows, param_delim=c(' \\s+'), condensed_delims = c('\\: ',' = ')) {
  #
  # extract metadata 3: collapsed row
  #
  # fpath (dir): the file path to be wrangled
  # skip_nrows (integer): the number of rows to skip until metadata content starts, assuming headers are present
  # parse_nrows (integer): the number of rows containing metadata condensed into a collapsed rows of information
  # param_delim (list of str): the delimiters usable for separating metadata name and metadata value

  #init
  object = data.table(path=fpath)
  ac_meta <- read.csv(file=fpath, skip=7, nrows = 1, col.names = "params")

  # decompress the collapsed rows
  fragment <- strsplit(ac_meta$params, split=paste0(param_delim, collapse='|')) %>%
    unlist() %>%
    strsplit(split=paste0(condensed_delims, collapse='|'))

  # loop through each collapsed metadata tuple
  for (eachf in fragment){
    object[1, eachf[1] := eachf[2]]
  }
  return(object)
}


# applied on GPS files
extract_metadata_from_file <- function (fpath, nrows, date_format="%Y/%m/%d", time_format="%H:%M:%S"){
  #
  # extract metadata features from a sample read
  #
  # fpath (dir): the file path to be wrangled
  # nrows (integer): the number of rows to use as metadata samples

  # init read
  output = read.csv(file=fpath, nrows=nrows, stringsAsFactors = F) %>%
    mutate(
      path=fpath,
      Date = as.POSIXct(LOCAL.DATE, format = date_format), # date
      Time = format(as.POSIXct(LOCAL.TIME, format = time_format), '%H:%M:%S'), # time
      DateTime = as.POSIXct(paste(Date, Time), format = '%Y-%m-%d %H:%M:%S'), # datetime, standardized
      time.difference = DateTime - lag(DateTime)) %>%
    # group_by(TRACK.ID) %>% # not sure why TRACK.ID seems to be important for separate time-series episodes
    summarize(path=min(path),
              date_start=unique(Date) %>% head(1),
              inc = median(time.difference, na.rm=T), # min or median may not work on singleton NA values if grouping by TRACK.ID
              Start_Time=format(min(DateTime), '%H:%M:%S'),
              'Epoch_Period_hms'=as_hms(inc)) %>%
    data.table()
  return(output)
}


get_mapped_file <- function(meta_df, fname, ftype){
  #
  # find the target file mapped by phase and idno
  # meta_df (data.table): the data.table of available files. No clones allowed.
  # fname (dir): the path to the target file
  # ftype (str): the target file type mapped by idno and phase

  file_meta <- meta_df %>% filter(path == {{fname}})
  out_file <- meta_df %>%  filter(idno %in% (file_meta$idno),  phase %in% (file_meta$phase),  ftype=={{ftype}})

  #if (length(out_file) == 0){
  if (nrow(out_file) == 0){  # weipeng edit
    print('No matching file')
    return(NULL)
  } else {
    return(out_file$path)
  }
}
