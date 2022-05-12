options(digits=15, warn = -1)
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(here)) install.packages('here')
if (!require(lubridate)) install.packages('lubridate')
if (!require(data.table)) install.packages('data.table')
if (!require(hms)) install.packages('hms')
if (!require(sp)) install.packages('sp')
if (!require(sf)) install.packages('sf')
if (!require(lwgeom)) install.packages(c('lwgeom','Rcpp'))
if (!require(geosphere)) install.packages('geosphere')
if (!require(measurements)) install.packages('measurements')
if (!require(latticeExtra)) install.packages('latticeExtra')
if (!require(testthat)) install.packages('testthat')

source("walkbout_config.R")
source("metadata_file_mapping.R") # metadata extraction
source("subject_file_mapper.R")
source("acc_file_reader.R")
source("gps_file_reader.R")
source("process_acc.R")
source("process_acc.R")
source("process_gps.R")
source("process_one_subject.R") # get bout summary info from one subject's accelerometry and GPS data
source("process_many_subject.R") # a loop version of process_one_subject.R


