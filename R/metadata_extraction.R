library(testthat)

# source file directories
ROOT_ <- "/Users/wzhou87/Desktop/retrieved_backup/epiProject/walk_bout"
accelerometry_phase1 <- file.path(ROOT_,"data/test/Actigraphy/Phase1")
accelerometry_phase2 <- file.path(ROOT_,"data/test/Actigraphy/Phase2")
gps_phase1 <- file.path(ROOT_,"data/test/GPS/Phase2")
gps_phase2 <- file.path(ROOT_,"data/test/GPS/Phase2")

####### ####### #######
#
# file directory unit tests
#
####### ####### #######

# unit-test 1: detect files
detect_files <- function(folder){
  expect_true(file.exists(folder))
  expect_equal(length(folder),1)
}

  #detect_files(accelerometry_phase1)
  detect_files(accelerometry_phase2)
  #detect_files(gps_phase1)
  detect_files(gps_phase2)

# unit-test 2: detect directory
detect_dir <- function(folder){
  expect_true(length(strsplit(folder, split = '/')[[1]])>1)
}

  #detect_dir(accelerometry_phase1)
  detect_dir(accelerometry_phase2)
  #detect_dir(gps_phase1)
  detect_dir(gps_phase2)

# unit test 3: detect csv with metadata leading rows
detect_leading_metadata <- function(directory){
  p = list.files(directory)[1]
  expect_equal(ncol(read.csv(file.path(directory,p), nrows=10)),1)
}

detect_no_leading_metadata <- function(directory){
  p = list.files(directory)[1]
  expect_false(ncol(read.csv(file.path(directory,p), nrows=10))==1)
}

  #detect_leading_metadata(accelerometry_phase1)
  detect_leading_metadata(accelerometry_phase2)
  #detect_no_leading_metadata(gps_phase1) # flagged for format issue
  detect_no_leading_metadata(gps_phase2)


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


####### ####### ####### #######
# Extract accelerometry files
#
# function: extract_files
# function: parse_fname
# function: parse_meta_tuples
# function: parse_collapsed_rows
#
####### ####### ####### #######

# 1. extract paths
#paths <- rbindlist(list(extract_files(folderpath=accelerometry_phase1, pattern="*.csv", ftype='Accelerometry', phase='Phase 1'),
#                        extract_files(folderpath=accelerometry_phase2, pattern="*.csv", ftype='Accelerometry', phase='Phase 2')))
#' comment everything below
#paths <- rbindlist(list(extract_files(folderpath=accelerometry_phase2, pattern="*.csv", ftype='Accelerometry', phase='Phase 2')))  # weipeng edit
#
#
## 2. parse the filenames
#output = parse_fname(fpaths = paths$path, name_delim=c('\\s+\\(', '\\)', 'DataTable.','\\.'), name_list=c('idno','date_start','inc', 'format'))
#paths <- left_join(paths, output, by="path") %>%
#  mutate(inc=gsub('sec','',inc))
#
##%>% mutate(date_start = as.POSIXct(date_start, format = '%Y-%m-%d'))
## 3. extract tuples
#output_object <- data.table()
#for (each in 1:length(paths$path)){
#  output = parse_meta_tuples(fpath=paths$path[each], skip_nrows=0, parse_nrows=7)
#  output_object <- rbindlist(list(output_object, output), use.names=TRUE, fill=TRUE)
#}
#paths <- left_join(paths, output_object, by="path", all=TRUE)
#
## 4. extract collapsed rows metadata
#output_object <- data.table()
#for (each in 1:length(paths$path)){
#  output = parse_collapsed_rows(fpath=paths$path[each], skip_nrows=7, parse_nrows=1, condensed_delims = '\\: | = ')
#  output_object <- rbindlist(list(output_object, output), use.names=TRUE, fill=TRUE)
#}
#paths <- left_join(paths, output_object, by='path', all=TRUE)
#
#
######## ####### ####### #######
##
## extract GPS file metadata
##
######## ####### ####### #######
#
## 1. extract paths
##path1 = data.table(extract_files(folderpath=gps_phase1, pattern="*.csv", ftype='GPS', phase='Phase 1'))
#path2 = data.table(extract_files(folderpath=gps_phase2, pattern="*.csv", ftype='GPS', phase='Phase 2'))
#
## 2. parse the filenames
##path1 (GPS phase 1) doesn't have the same data structure; looks like clones of Accelerometry Phase 1
##output = parse_fname(fpaths = path1$path, name_delim=c('\\s+\\(', '\\)', '\\DataTable.','\\.'), name_list=c('idno','date_start','inc', 'format'))
##path1 <- left_join(path1, output, by="path")
#
#path2 <- parse_fname(fpaths = path2$path,
#                     name_delim=c('\\s+\\_', '_CRS.', '_GPS.','\\.'),
#                     name_list=c('idno','format')) %>%
#  left_join(path2, ., by="path")
#
######## ####### ####### ####### #######
## extract metadata from GPS files
##
## function = extract_metadata_from_file
##
######## ####### ####### ####### #######
#
## 3. Extract metadata from sample rows
#output_object <- data.table()
#for (each in 1:length(path2$path)){
#  object = extract_metadata_from_file(fpath=path2$path[each], nrows=10)
#  output_object <- rbindlist(list(output_object, object), use.names=TRUE, fill=TRUE)
#}
#
#
## if clause to prevent recurrent left_joins
#if (!'TRACK.ID' %in% colnames(path2)){
#  path2 <- left_join(path2, #>#
#                     output_object %>%
#                       mutate(date_start = as.character(date_start),
#                              inc = as.character(inc)),  # drop date datatype
#                     by='path', all=TRUE)
#}
#
## compare files and idno mappings
#path_all = rbindlist(list(paths,
#                          #path1 %>% select(path, ftype,phase,fname,idno, format), # GPS Phase 1 are clone files
#                          path2), use.names=TRUE, fill=TRUE) %>%
#  mutate(ftype_phase = paste0(ftype, " ", phase))
#
## idno dimensions
#count_fname = path_all %>% dcast(formula = ftype ~ phase, value.var = "fname", fun.aggregate=length)
#
## idno catalog
#count_idno = path_all %>% dcast(formula = idno ~ ftype_phase, value.var = "fname", fun.aggregate=length)
#
## fname catalog
##type_phase = c('Accelerometry Phase 1',  'GPS Phase 1', 'Accelerometry Phase 2', 'GPS Phase 2')
#type_phase = c('Accelerometry Phase 2', 'GPS Phase 2')  # weipeng edit
#fname_usage = path_all %>%
#  dcast(formula = fname ~ ftype_phase, value.var = "idno") %>%
#  mutate(set_linkage = path_all %>%
#           dcast(formula = fname ~ ftype_phase, value.var = "idno") %>%
#           select(all_of(type_phase)) %>%
#           is.na %>%
#           `!` %>%
#           rowSums()) %>%
#  arrange(-set_linkage)
#
## filter for non-clones
#nonclones_ = path_all %>% filter(fname %in% (fname_usage %>% filter(set_linkage==1) %>% .$fname)) # filter for non-clone files
#
## filter for clones
## identifies the shared filenames (clones)
##clones_ = path_all %>% filter(fname %in% (fname_usage %>% filter(set_linkage>1) %>% .$fname )) # filter for fname
#
## how many have multiple filetypes
#count_idno_nonclone = count_idno %>%
#  filter(idno %in% (nonclones_ %>% # non-clones
#                      select(ftype, idno, phase) %>%
#                      group_by(idno, phase) %>%
#                      count() %>%
#                      filter(n>1) %>% # has both accelerometry and GPS
#                      .$idno)) # 473 have multiple filetypes in Phase 2
#
#count_fname_nonclone = path_all %>%
#  filter(idno %in% count_idno_nonclone$idno) %>% # 473 non-clones with both accelerometry and GPS
#  filter(fname %in% nonclones_$fname) %>%
#  dcast(formula = ftype ~ phase, value.var = "fname", fun.aggregate=length)
#
#
######## ####### ####### ####### #######
##
## metadata search functions
##
## function = get_mapped_file
##
######## ####### ####### ####### #######
#
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
#
## find the target file mapped by phase and idno
##get_mapped_file(meta_df=path_all, fname=path2$path[1], ftype='Accelerometry')
##get_mapped_file(meta_df=path_all, fname=paths$path[1], ftype='GPS')
##get_mapped_file(meta_df=path_all, fname=paths$path[1000], ftype='GPS')
#
#  ## test: find the target file mapped by phase and idno
#  #length(get_mapped_file(meta_df=path_all, fname=path2$path[1], ftype='Accelerometry'))==1
#  #length(get_mapped_file(meta_df=path_all, fname=paths$path[1], ftype='GPS'))==0
#  #length(get_mapped_file(meta_df=path_all, fname=paths$path[1000], ftype='GPS'))==1
#
