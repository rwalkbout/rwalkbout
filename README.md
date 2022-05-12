## walk about
identify walk bouts using GPS and accelerometry data

### set up
set working directory to be `walkabout/R`
run `R/example.R`

#### file reader (ACC, GPS)
Ensure that date_time returned by both readers are in the same time zone

##### acc reader
accepts a parameter `acc_file_path`
returns a dataframe with columns `date_time,count`
cast to tibble with `as.tibble()`
collapse/interpolate to make epoch period=30s

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

##### gps reader
accepts a parameter `gps_file_path`
returns a dataframe with columns `date_time,latitude,longitude,speed`, latitude and longitude should be positive/negative based its N/S, W/E
cast to tibble with `as.tibble()`

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

#### subject file mapper
This tells the program which ACC and which GPS files are from the same subject.

runs and returns a dataframe with columns `subject_id,acc_file_path,gps_file_path,`

### known issue
Failure with subject 100006. This subject's duration contains the daylight saving day. 
All accelerometry data contains 24 hours. And this becomes a problem when converting the time to UTC on the daylight saving day. NA was created and error occurs.
