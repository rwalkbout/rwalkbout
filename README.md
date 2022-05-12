## rwalkabout
`rwalkbout` aims to streamline the identification of walk bouts from accelerometry and gps data.

### Set up
Set working directory to be `rwalkabout/R`.

Edit and run `R/analysis/analyze_all_subject_seattle.R`.

### Customize rwalkbout to your dataset
`rwalkbout` can be configured to any gps and accelerometry (acc) data, as long as you write the respective acc/gps file reader. 

The reader should be a function that reads the data and return to `rwalkbout` with a predefined format.

Below outlines what inputs `rwalkbout` will give to the function and what returned format `rwalkbout` expects.

Note: ensure that date_time returned by both readers are in the same time zone

#### File reader for acc data
Inputs:
accepts a parameter `acc_file_path`

Outputs:
returns a dataframe with columns `date_time,count`
cast to tibble with `as.tibble()`
collapse/interpolate to make epoch period=15s

Example outputs and data types:

``` output data types
> acc_data$date_time[1]
[1] "2016-12-10 00:00:15 PST"
> class(acc_data$date_time[1])
[1] "POSIXct" "POSIXt" 

> acc_data$count[1]
[1] 251
> class(acc_data$count)
[1] "numeric"
```

#### File reader for gps data
Inputs:
accepts a parameter `gps_file_path`

Outputs:
returns a dataframe with columns `date_time,latitude,longitude,speed`, latitude and longitude should be positive/negative based its N/S, W/E
cast to tibble with `as.tibble()`

Example outputs and data types:
```data type
> class(gps_data$date_time[1])
[1] "POSIXct" "POSIXt" 
> gps_data$date_time[1]
[1] "2016-12-10 03:37:05 PST"

> class(gps_data$latitude[1])
[1] "numeric"
> gps_data$latitude[1]
[1] 45.437084

> class(gps_data$longitude[1])
[1] "numeric"
> gps_data$longitude[1]
[1] -122.62609

> class(gps_data$speed[1])
[1] "numeric"
> gps_data$speed[1]
[1] 0.590584
```

#### acc and gps file mapper
Optionally users can create a function that returns the mapping between acc and gps files. Examples are shown in `rwalkbout/R/subject_file_mapper.R`.

`rwalkbout` does not require it for running. It is just a nice thing to do when dealing with folders of files and subjects.