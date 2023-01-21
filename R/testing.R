library(RPostgreSQL)

test <- source('/projects/code_base/dbconnect.R')
disconnectall()

# db connection -----------------------------------------------------------
connect <- function(dbname, host="localhost", port=5432){
  if(!exists('password')){
    password <- readline("PG password: ")
    assign("password", password, envir = .GlobalEnv)
  }
  if(!exists(dbname)){
    message(dbname, " not connected")
    conn <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname, host=host, password = password, port=port)
    message(dbname, " now connected")
    return(conn)
  }
  if(exists(dbname)){
    cmd <- sprintf("isPostgresqlIdCurrent(%s)", dbname)
    connectionIsCurrent <- eval(parse(text=cmd))
    if(!connectionIsCurrent){
      message("establising connection to ", dbname)
      conn <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname, host=host, password = password, port=port)
    } else {
      cmd <- sprintf("conn <- %s", dbname)
      eval(parse(text=cmd))
    }
    return(conn)
  }
}

password <- "Bu!Bd03w7LK8SW2c9cA9"
trac <- connect("trac")

test_query <- "select count(*) from baseline.bout_all;"
test_result <- dbGetQuery(conn = trac, statement=test_query)

# Get all bouts at baseline near home to match Ruizhu's paper
bouts_people <- "select b.*,
                          d.*,
                          b.time_pa_1000cpm_bout_start AT TIME ZONE 'America/Los_Angeles' as start_time
                        from
                          baseline.bout_all as b,
                          baseline.survey as d
                        where b.id = d.id::text
                        order by b.id, start_time"
baseline_bouts <- dbGetQuery(conn = trac, statement=bouts_people)
colnames(baseline_bouts)

table(baseline_bouts$id)

all_bouts_for_id <- function(trac_id) {
  return(baseline_bouts[baseline_bouts$id == trac_id,
                        # c("start_time", "duration_m", "walk_type", "mean_speed_kmh")
                        ])
}


gps_bouts_for_id <- function(trac_id) {
  return(baseline_bouts[baseline_bouts$id == trac_id & baseline_bouts$walk_type == 'walk1_gps' | baseline_bouts$walk_type == 'nonwalk2_gps',
                        # c("start_time", "duration_m", "walk_type", "mean_speed_kmh")
                        ])
}
gps_bouts_for_id('10100052') # person ID is 10100052. this is looking at what bouts are identified for this person.
all_bouts_for_id('10100052')
test <- all_bouts_for_id('10100052')
# should be able to run github code on raw data and get the same # of bouts for each person as we get when we run this here.






#### USING ACCELEROMETRY PACKAGE ###
library(accelerometry)
library(data.table)
pkg_dat <- as.data.table(copy(unidata))
setnames(pkg_dat, c("seqn", "paxday", "paxinten"), c("participant", "epoch_day", "count"))
epoch_series <- read_csv("/home/wilnerl/epoch_series.csv")
dat <- as.data.table(copy(epoch_series))
dat <- dat[,Activity:=NULL]
setnames(dat, "Axis1_epochSum", "count")
dat <- dat[,participant:=10100052]
bouts_21007 <- bouts(counts = pkg_dat[participant==21007]$count,
                   bout_length = 10,
                   thresh_lower = 2020,
                   tol = 2,
                   tol_lower = 2)
sum(bouts_21007) # 10075
bouts_10100052 <- bouts(counts = dat[participant==10100052]$count,
                   bout_length = 10*2,
                   thresh_lower = 2020*2,
                   tol = 2*2,
                   tol_lower = 2*2)
sum(bouts_10100052)
# multiplying by 2 because i believe their rows are minutes and our rows are 30 second epochs
# how do they handle days??
  # it seems like they just sort of ignore days and
  # calculate bouts consecutively for the entire dataset?
# what is the sum they are calculating here?

# the bout output is minutes of bouted activity ?????
# add summary statistic of total # of min for each participant

## TO DO
# 1. Email dr van domelen
# 2. Add epoch length as an arg
# 3. add bouted minutes as a summary statistic to our output
