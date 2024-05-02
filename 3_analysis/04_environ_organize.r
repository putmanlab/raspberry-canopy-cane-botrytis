###########################
# RASPBERRY Cane Botrytis #
# Environmental Data	  #
# Organize				  #
# Upload 				  #
###########################

## built on Docker putmanlab/exploratory-analysis:420.0

library(conflicted)

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)

library(photobiology) # for sun rise/set times: sunrise_time, sunset_time

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/raspb_botrytis")

source("./3_analysis/rcb_functions.r")

#install.packages("tictoc")
#library(tictoc)

# NOTE: R hanged on a few commands on large datasets (see below)
	# CPU was high, then declined to about idle, but command did not finish
	# R did not crash, freeze, or give memory limit error
	# resolved by increasing memory limit in Docker Desktop preferences
	# Environment window in Rstudio says 3.41 GB of memory is used by session to run this script; memory report says total of 3.936 GB


################################
# A. Import/Organize - Ranch 1 #
################################
#tic("A")

### import
	## get data
	in.r1.env.1 = read_csv(file="./2_data/environmental/run-1/mx_loggers_2018_06_06_12_44_40_PDT_1.csv", col_names=T, na="", guess_max=11000)
	in.r1.env.2 = read_csv(file="./2_data/environmental/run-1/mx_loggers_2018_06_06_12_44_40_PDT_2.csv", col_names=T, na="", guess_max=11000)
	in.r1.env.3 = read_csv(file="./2_data/environmental/run-1/mx_loggers_2018_06_06_12_44_40_PDT_3.csv", col_names=T, na="", guess_max=11000)
	in.r1.env.4 = read_csv(file="./2_data/environmental/run-1/mx_loggers_2018_06_06_12_44_40_PDT_4.csv", col_names=T, na="", guess_max=11000)
	
	## bind dfs
	in.r1.env.w.1 = bind_rows(in.r1.env.1, in.r1.env.2, in.r1.env.3, in.r1.env.4)

### organize
	## column names
		# get names
		names.col = tibble(names.old = colnames(in.r1.env.w.1))
		
		# add column of new names
		names.col = names.col %>% mutate(names.new=as.character(names.old))
		
		# remove parenthesis and text inside
		names.col = names.col %>% mutate(names.new=str_replace(names.new, "\\(.*\\)", ""))
		
		# remove units and ","
		names.col = names.col %>% mutate(names.new=str_replace(names.new, " , \\*C, ", "_"))
		names.col = names.col %>% mutate(names.new=str_replace(names.new, " , %, ", "_"))
		
		# shorten names
		names.col = names.col %>% mutate(names.new=str_replace(names.new, "Temperature - Avg", "temp_avg"))
		names.col = names.col %>% mutate(names.new=str_replace(names.new, "Temperature", "temp"))
		names.col = names.col %>% mutate(names.new=str_replace(names.new, "RH - Avg", "rh_avg"))
		names.col = names.col %>% mutate(names.new=str_replace(names.new, "RH", "rh"))
		names.col = names.col %>% mutate(names.new=str_replace(names.new, "Dew Point", "dew_point"))
		
		# rename line numb.
		names.col = names.col %>% mutate(names.new=replace(names.new, names.old == "Line#", "line_num"))
		
		# change columns in original dataframe
		in.r1.env.w.1 = in.r1.env.w.1 %>% rename_at(vars(names.col$names.old), ~ (names.col$names.new))

	## change to datetime
		# create new column in proper format
		in.r1.env.w.1 = in.r1.env.w.1 %>% mutate(datetime=mdy_hms(Date))
	
		# change column order and remove old column
		in.r1.env.w.1 = in.r1.env.w.1 %>% select(line_num, datetime, temp_1:dew_point_9)
		
	## adjust timestamps of logger #9 to align with rest; could not figure out how to mutate in place with conditions
		# split up df
		in.r1.env.w.t1 = in.r1.env.w.1 %>% filter(!is.na(temp_1) & is.na(temp_9)) %>% select(datetime, temp_1:dew_point_8)
		in.r1.env.w.t2 = in.r1.env.w.1 %>% filter(is.na(temp_1) & !is.na(temp_9)) %>% select(datetime, temp_9:dew_point_9)
		
		# adjust time of logger 9
		in.r1.env.w.t2 = in.r1.env.w.t2 %>% mutate(datetime=update(datetime, minute=minute(datetime) + 4)) %>% mutate(datetime=update(datetime, second=second(datetime) + 32))
		
		# merge back together
		in.r1.env.w.2 = in.r1.env.w.t1 %>% full_join(in.r1.env.w.t2, by=c("datetime" = "datetime"))
	
	## convert to extra-long format to get logger as variable instead of in columns
	in.r1.env.elg = in.r1.env.w.2 %>% gather(key="measure", value="value", -datetime)
	
	## separate measurement from logger number
		# replace last "_" in string (the one the separates measurement from number) to facilitate splitting
		in.r1.env.elg = in.r1.env.elg %>% mutate(measure=str_replace(measure, "_(?!.*_)", "-"))

		# after switching to R 4.2.0, the "split measurement and logger number..." command led to a hang
		# max CPU usage for a few minutes, then declined to idle leading to freeze/hang up without command finishing
		# command worked on whole dataset in R 3.5.0
		# can be resolved by increasing memory (from 2 to 4 GB) and swap file (from 1 to 2 GB) limits in docker settings
			# from activity monitor does not appear swap file is used that much
			# can also be resolved by splitting dataset into smaller tibbles

		# split measurement and logger number
		in.r1.env.elg = in.r1.env.elg %>% separate(measure, into=c("measure","logger_id"), sep="-")
		
		# change column to integer
		in.r1.env.elg = in.r1.env.elg %>% mutate(logger_id=as.integer(logger_id))
				
	## logger 7 did not record avg values; in previous version, temp/rh was substituted for temp_avg/rh_avg, but this was removed due to possible bias and left as NA

### merge plot/treatment data with logger
	## import 
	in.r1.env.logid = read_csv(file="./2_data/environmental/run-1/HOBO_MX2300_Series - Sheet1.csv", col_names=T, na="")
	
	## select and rename columns
	r1.env.logid = in.r1.env.logid %>% select(logger_id=`Sensor Name`, treatment=Treatment, tunnel=rows, block)
	
	## change tunnel column
	r1.env.logid = r1.env.logid %>% mutate(tunnel=replace(tunnel, tunnel == 3, "3-row")) %>% mutate(tunnel=replace(tunnel, tunnel == 2, "2-row"))
	
	## merge
	r1.env.elg = in.r1.env.elg %>% left_join(r1.env.logid, by=c("logger_id" = "logger_id"))
	
	## reorder columns
	r1.env.elg = r1.env.elg %>% select(datetime, logger_id, tunnel, treatment, block, measure, value)
	
	## quality control
		# remove mis-treated control plots
		r1.env.elg = r1.env.elg %>% mutate(value=replace(value, tunnel == "2-row" & treatment == "control" & block == 2, NA))
		r1.env.elg = r1.env.elg %>% mutate(value=replace(value, tunnel == "3-row" & treatment == "control" & block == 1, NA))
		r1.env.elg = r1.env.elg %>% mutate(value=replace(value, tunnel == "3-row" & treatment == "control" & block == 4, NA))	
	
		# change abnormal RH readings to NA
			# 3-row, block 3, blade, 2018-04-27; much higher than all other treatments for that date
			r1.env.elg = r1.env.elg %>% mutate(value=replace(value, ( measure %in% c("rh","rh_avg") & tunnel == "3-row" & treatment == "blade" & date(datetime) == as_date("2018-04-27") ), NA))
			
			# 2-row, block 5, manual, 2018-05-06; much higher than all other treatments for that date
			r1.env.elg = r1.env.elg %>% mutate(value=replace(value, ( measure %in% c("rh","rh_avg") & tunnel == "2-row" & treatment == "manual" & date(datetime) == as_date("2018-05-06") ), NA))

	## convert to regular long
	r1.env = r1.env.elg %>% spread(key=measure, value=value)

		# check data 
		r1.env %>% group_by(logger_id, tunnel, treatment, block) %>% summarize(ct=n(), ct_temp_avg_na=sum(is.na(temp_avg)), ct_rh_avg_na=sum(is.na(rh_avg))) %>% print(n=Inf)

###	final organization
	## remove unneeded, large objects
#	rm(in.r1.env.elg, r1.env.elg)
	
	## remove final timestamp to prevent irregularities in analysis when rounding by hour and day
	r1.env = r1.env %>% filter(datetime < as_datetime("2018-05-28 23:00:00"))

	## add timezone info
	r1.env = r1.env %>% mutate(datetime=force_tz(datetime, tzone="Etc/GMT+8"))

#toc(log=TRUE, quiet=TRUE)
	
	
################################
# B. Import/Organize - Ranch 2 #
################################
#tic("B")	

	# NOTE: original export of data did not account for daylight savings time
	# some loggers currently in use, therefore data cannot be redownloaded for all; instead, manually adjust times using row ids

### import and assign logger number
	in.r2.env.3 = read_csv(file="./2_data/environmental/run-2/3_B1_manual.csv", col_names=T, na="") %>% mutate(logger_id=3)
	in.r2.env.4 = read_csv(file="./2_data/environmental/run-2/4_B1_control.csv", col_names=T, na="") %>% mutate(logger_id=4)
	in.r2.env.5 = read_csv(file="./2_data/environmental/run-2/5_B1_twine.csv", col_names=T, na="") %>% mutate(logger_id=5)
	in.r2.env.6 = read_csv(file="./2_data/environmental/run-2/6_B1_blade.csv", col_names=T, na="") %>% mutate(logger_id=6)
	in.r2.env.7 = read_csv(file="./2_data/environmental/run-2/7_B2_twine.csv", col_names=T, na="") %>% mutate(logger_id=7)
	in.r2.env.9 = read_csv(file="./2_data/environmental/run-2/9_B2_blade.csv", col_names=T, na="") %>% mutate(logger_id=9)
	in.r2.env.10 = read_csv(file="./2_data/environmental/run-2/10_B2_control.csv", col_names=T, na="") %>% mutate(logger_id=10)
	in.r2.env.11 = read_csv(file="./2_data/environmental/run-2/11_B2_manual.csv", col_names=T, na="") %>% mutate(logger_id=11)
	in.r2.env.12 = read_csv(file="./2_data/environmental/run-2/12_B3_control.csv", col_names=T, na="") %>% mutate(logger_id=12)
	in.r2.env.13 = read_csv(file="./2_data/environmental/run-2/13_B3_manual.csv", col_names=T, na="") %>% mutate(logger_id=13)
	in.r2.env.14 = read_csv(file="./2_data/environmental/run-2/14_B3_blade.csv", col_names=T, na="") %>% mutate(logger_id=14)
	in.r2.env.15 = read_csv(file="./2_data/environmental/run-2/15_B3_twine.csv", col_names=T, na="") %>% mutate(logger_id=15)
	in.r2.env.16 = read_csv(file="./2_data/environmental/run-2/16_B4_control.csv", col_names=T, na="") %>% mutate(logger_id=16)
	in.r2.env.17 = read_csv(file="./2_data/environmental/run-2/17_B4_manual.csv", col_names=T, na="") %>% mutate(logger_id=17)
	in.r2.env.18 = read_csv(file="./2_data/environmental/run-2/18_B4_twine.csv", col_names=T, na="") %>% mutate(logger_id=18)
	in.r2.env.19 = read_csv(file="./2_data/environmental/run-2/19_B4_blade.csv", col_names=T, na="") %>% mutate(logger_id=19)
	in.r2.env.20 = read_csv(file="./2_data/environmental/run-2/20_B5_twine.csv", col_names=T, na="") %>% mutate(logger_id=20)
	in.r2.env.26 = read_csv(file="./2_data/environmental/run-2/26_B5_control.csv", col_names=T, na="") %>% mutate(logger_id=26)
	in.r2.env.27 = read_csv(file="./2_data/environmental/run-2/27_B5_blade.csv", col_names=T, na="") %>% mutate(logger_id=27)
	in.r2.env.32 = read_csv(file="./2_data/environmental/run-2/32_B5_manual.csv", col_names=T, na="") %>% mutate(logger_id=32)

### organize
	## bind together
	in.r2.env = bind_rows(in.r2.env.3, in.r2.env.4, in.r2.env.5, in.r2.env.6, in.r2.env.7, in.r2.env.9, in.r2.env.10, in.r2.env.11, in.r2.env.12, in.r2.env.13, in.r2.env.14, in.r2.env.15, in.r2.env.16, in.r2.env.17, in.r2.env.18, in.r2.env.19, in.r2.env.20, in.r2.env.26, in.r2.env.27, in.r2.env.32)

	## change column names
	in.r2.env = in.r2.env %>% rename(datetime=`Date Time, GMT -0700`, temp=`Temp, °F`, temp_avg=`Temp - Avg, °F`, rh=`RH, %`, rh_avg=`RH - Avg, %`, dew_point=`DewPt, °F`, record_id=`#`)

	## convert format of datetime, logger_id columns
		# datetime
		in.r2.env = in.r2.env %>% mutate(datetime=ymd_hms(datetime), logger_id=as.integer(logger_id))

	## adjust time of loggers 9 and 17; could not figure out how to mutate in place with conditions
		# split dataframes
		in.r2.env.t1 = in.r2.env %>% filter(!logger_id %in% c(9,17))
		in.r2.env.t.l9 = in.r2.env %>% filter(logger_id == 9)
		in.r2.env.t.l17 = in.r2.env %>% filter(logger_id == 17)
		
		# update loggers	
		in.r2.env.t.l9 = in.r2.env.t.l9 %>% mutate(datetime=update(datetime, minute=minute(datetime) + 1)) %>% mutate(datetime=update(datetime, second=second(datetime) + 39))
		in.r2.env.t.l17 = in.r2.env.t.l17 %>% mutate(datetime=update(datetime, minute=minute(datetime) + 1))

		# bind together
		in.r2.env.t = bind_rows(in.r2.env.t1, in.r2.env.t.l9, in.r2.env.t.l17)

	## fix daylight savings time overlap; data was exported as local time, therefore there are duplicate records between 0100 and 0200 when clocks were turned back on 2018-11-04 at 0200
		# split dataframes; 3,4,5,6 logging ends on 11-01
		in.r2.env.s0 = in.r2.env.t %>% filter(logger_id %in% c(3,4,5,6))
		
		in.r2.env.s1.a = in.r2.env.t %>% filter(record_id <= 5706 & logger_id %in% c(7,10,11,12,13,14,15,16,18,19,20,26,27,32))
		in.r2.env.s1.b = in.r2.env.t %>% filter(record_id > 5706 & logger_id %in% c(7,10,11,12,13,14,15,16,18,19,20,26,27,32))

		in.r2.env.s2.a = in.r2.env.t %>% filter(record_id <= 5708 & logger_id == 9)
		in.r2.env.s2.b = in.r2.env.t %>% filter(record_id > 5708 & logger_id == 9)

		in.r2.env.s3.a = in.r2.env.t %>% filter(record_id <= 5710 & logger_id == 17)
		in.r2.env.s3.b = in.r2.env.t %>% filter(record_id > 5710 & logger_id == 17)

		# bind dfs before/after DST
		r2.env.dst.bef = bind_rows(in.r2.env.s0, in.r2.env.s1.a, in.r2.env.s2.a, in.r2.env.s3.a)
		r2.env.dst.aft = bind_rows(in.r2.env.s1.b, in.r2.env.s2.b, in.r2.env.s3.b)
		
		# arrange, for checking
		r2.env.dst.bef = r2.env.dst.bef %>% arrange(datetime, logger_id)
		r2.env.dst.aft = r2.env.dst.aft %>% arrange(datetime, logger_id)
		
		# change timezone
			# before DST change 
				# first force tz to UTC-7 because df thinks it is UTC but it is actually UTC-7
				r2.env.dst.bef = r2.env.dst.bef %>% mutate(datetime=force_tz(datetime, tzone="Etc/GMT+7"))
				
				# then convert to/express in GMT-8
				r2.env.dst.bef = r2.env.dst.bef %>% mutate(datetime=with_tz(datetime, tzone="Etc/GMT+8"))
			
			# after DST change - force timezone metadata to be UTC-8, to align with r1 (most disease rating dates are in UTC-8)
			r2.env.dst.aft = r2.env.dst.aft %>% mutate(datetime=force_tz(datetime, tzone="Etc/GMT+8"))
	
		# bind together
		in.r2.env.s = bind_rows(r2.env.dst.bef, r2.env.dst.aft)
		
		# checks
		in.r2.env.t %>% group_by(logger_id) %>% summarize(ct=n(), ct_na=sum(is.na(temp_avg)))
		in.r2.env.s %>% group_by(logger_id) %>% summarize(ct=n(), ct_na=sum(is.na(temp_avg)))
	
	## assign treatment info
		# import worksheet
		in.r2.logid = read_csv(file="./2_data/environmental/run-2/run2_logger_trt-assignment.csv", col_names=T)
		
		# join
		in.r2.env.s = in.r2.env.s %>% left_join(in.r2.logid, by=c("logger_id" = "logger_id"))
	
	## clean up
		# remove record_id
		in.r2.env.s = in.r2.env.s %>% select(-record_id)

		# rename
		r2.env = in.r2.env.s
	
	## convert to C
	r2.env = r2.env %>% mutate(temp= ((temp-32)*(5/9)), temp_avg= ((temp_avg-32)*(5/9)), dew_point= ((dew_point-32)*(5/9)) )		

### final organization
	## exclude data after plants were mown down; call was received on 2/1/19
	r2.env = r2.env %>% filter(datetime < as_datetime("2019-02-01 00:00:00"))

### check data 
	r2.env %>% group_by(logger_id) %>% summarize(ct=n(), ct_temp_avg_na=sum(is.na(temp_avg)), ct_rh_avg_na=sum(is.na(rh_avg))) %>% print(n=Inf)

#toc(log=TRUE, quiet=TRUE)


#########################
# C. Combine Dataframes #
#########################
#tic("C")

### add experiment column
	## ranch 1
		# create column
		r1.env = r1.env %>% mutate(experiment=as.character(NA))
		
		# fill
		r1.env = r1.env %>% mutate(experiment=replace(experiment, tunnel == "2-row", "A-2-row"))
		r1.env = r1.env %>% mutate(experiment=replace(experiment, tunnel == "3-row", "B-3-row"))
		
		# remove tunnel column
		r1.env = r1.env %>% select(-tunnel)
	
	## ranch 2
	r2.env = r2.env %>% mutate(experiment=as.character("C-3-row"))
	
### bind
	## bind
	data.env = bind_rows(r1.env, r2.env)	
	
	## change column order; remove tunnel column
	data.env = data.env %>% select(experiment, treatment, block, logger_id, datetime, temp, temp_avg, rh, rh_avg, dew_point)

		# check data 
		data.env %>% group_by(experiment, treatment, block) %>% summarize(ct=n(), ct_temp_avg_na=sum(is.na(temp_avg)), ct_rh_avg_na=sum(is.na(rh_avg))) %>% print(n=Inf)


###############
# D. Organize #
###############

### calculate dew point from averaged rh and temp values
	# logger provides a dewpoint measurement, but unclear if it is of instantaneous temp/rh or average temp/rh, also unclear what method used to calculate it
	# source: https://ag.arizona.edu/azmet/dewpoint.html
		# other source used c=243.04, b=17.625 (https://bmcnoldy.rsmas.miami.edu/Humidity.html)
	
	data.env = data.env %>% mutate(
		x = ( (log(rh_avg/100) + ( (17.27 * temp_avg) / (237.3  + temp_avg))) / 17.27  ) )
		
	data.env = data.env %>% mutate(
		dew_point_avg = ( (237.3  * x) / (1 - x) ) )
		
	data.env = data.env %>% mutate(dew_point_avg=round(dew_point_avg, digits=2))

### organize
	## discard instantaneous measurements (columns temp, rh, dew_point) and dew point intermediate calculations
	data.env = data.env %>% select(-temp, -rh, -dew_point, -x)
	
	## rename
	data.env = data.env %>% rename(dpt_avg=dew_point_avg)

### assign day/night
	# set location
	geo_code = data.frame(lon = -119.094, lat = 34.212)

	## get sun rise/set times
		# extract dates
		data.dates = data.env %>% group_by(date=as_date(floor_date(datetime, "day"))) %>% summarize(ct=n())
	
		## get sunrise, sunset times
		data.sun = data.dates %>% mutate(
									time_sunrise=sunrise_time(date=date, tz="Etc/GMT+8", geocode=geo_code, twilight="none", unit.out="datetime"),
									time_sunset=sunset_time(date=date, tz="Etc/GMT+8", geocode=geo_code, twilight="none", unit.out="datetime") )	

		# add date column to facilitate joining
		data.env = data.env %>% mutate(date=date(datetime))
		
	## join sun data
	data.env = data.env %>% left_join(data.sun, by=c("date" = "date"))
	
	## add blank column
	data.env = data.env %>% mutate(period=as.character(NA))
	
	## fill
	data.env = data.env %>% mutate(period=replace(period, ( (datetime >= ceiling_date(time_sunrise, "10 mins")) & (datetime <= ceiling_date(time_sunset, "10 mins")) ), "day") )
	data.env = data.env %>% mutate(period=replace(period, ( (datetime <  ceiling_date(time_sunrise, "10 mins")) | (datetime >  ceiling_date(time_sunset, "10 mins")) ), "night") )

### add date_rating grouping variable; based on sunset on rating date
	## get sunset times on rating dates
		# make tibble
		rating_interval = tibble(
			date_rating = as_date(c("2018-02-13","2018-03-13","2018-04-11","2018-10-17","2018-11-07","2018-11-28")),
			days_since_last = c(28,28,29,22,21,21) )

		# get sunrise, sunset times
		rating_interval %>% mutate(time_sunset=sunset_time(date=date_rating, tz="Etc/GMT+8", geocode=geo_code, twilight="none", unit.out="datetime") )	
		
		# get sunset time on treatment application date for C-3-row
		sunset_time(date=as_date("2018-09-25"), tz="Etc/GMT+8", geocode=geo_code, twilight="none", unit.out="datetime")

	## assign rating date; begin date of range not needed for first date because logging did not start until after treatments were applied
	data.env = data.env %>% mutate(date_rating=case_when(
		(															   		datetime <= as_datetime("2018-02-13 17:33:39", tz="Etc/GMT+8")) ~ as_date("2018-02-13"),
		(datetime > as_datetime("2018-02-13 17:33:39", tz="Etc/GMT+8") &	datetime <= as_datetime("2018-03-13 17:58:07", tz="Etc/GMT+8")) ~ as_date("2018-03-13"),
		(datetime > as_datetime("2018-03-13 17:58:07", tz="Etc/GMT+8") &	datetime <= as_datetime("2018-04-11 18:20:28", tz="Etc/GMT+8")) ~ as_date("2018-04-11"),
		(datetime > as_datetime("2018-09-25 17:45:31", tz="Etc/GMT+8") &	datetime <= as_datetime("2018-10-17 17:16:03", tz="Etc/GMT+8")) ~ as_date("2018-10-17"),
		(datetime > as_datetime("2018-10-17 17:16:03", tz="Etc/GMT+8") &	datetime <= as_datetime("2018-11-07 16:54:01", tz="Etc/GMT+8")) ~ as_date("2018-11-07"),
		(datetime > as_datetime("2018-11-07 16:54:01", tz="Etc/GMT+8") &	datetime <= as_datetime("2018-11-28 16:42:42", tz="Etc/GMT+8")) ~ as_date("2018-11-28") ) )

	## join
	data.env = data.env %>% left_join(rating_interval, by=c("date_rating" = "date_rating"))

### finalize
	## remove unneeded columns
	data.env = data.env %>% select(-logger_id, -date, -ct)
	
	## order columns
	data.env = data.env %>% select(experiment, treatment, block, datetime, period, date_rating, days_since_last, temp_avg, rh_avg, dpt_avg, time_sunrise, time_sunset)
	
	## remove outlier (C-3-row manual block 2)
	data.env = data.env %>% mutate(
		rh_avg=replace(
			rh_avg,
			experiment == "C-3-row" & treatment == "manual" & block == 2 & ( date(datetime) >= as_date("2018-10-04") & date(datetime) <= as_date("2018-10-20") ),
			NA),
		dpt_avg=replace(
			dpt_avg,
			experiment == "C-3-row" & treatment == "manual" & block == 2 & ( date(datetime) >= as_date("2018-10-04") & date(datetime) <= as_date("2018-10-20") ),
			NA) )

	## check data
		# check NAs
		data.env %>% group_by(experiment, treatment, block) %>% summarize(ct=n(), ct_na_temp=sum(is.na(temp_avg)), ct_na_rh=sum(is.na(rh_avg)) ) %>% print(n=Inf)

		# check for extra rows from grouping; commented out after confirming no double rows; takes a long time to run
#		data.env.chk = data.env %>% 
#			group_by(experiment, treatment, block, datetime=ceiling_date(datetime, "10 min", change_on_boundary=F), period, date_rating, days_since_last) %>%
#			summarize(ct=n()) %>%
#			ungroup()
#			
#			data.env.chk %>% 
#				group_by(experiment, treatment, block, datetime=ceiling_date(datetime, "10 min", change_on_boundary=F), date_rating, days_since_last) %>% 
#				summarize(ct=n(), ct_distinct=n_distinct(period) ) %>% filter(ct_distinct > 1) %>% print(n=5)
#	
#			data.env.chk %>% 
#				group_by(experiment, treatment, block, datetime=ceiling_date(datetime, "10 min", change_on_boundary=F), period, date_rating, days_since_last) %>% 
#				summarize(ct=n(), ct_distinct=n_distinct(date(datetime)) ) %>% filter(ct_distinct > 1) %>% print(n=5)
#	
#			data.env.chk %>% 
#				group_by(experiment, treatment, block, datetime=ceiling_date(datetime, "10 min", change_on_boundary=F), period, days_since_last) %>% 
#				summarize(ct=n(), ct_distinct=n_distinct(date_rating) ) %>% filter(ct_distinct > 1) %>% print(n=5)
#	
#			data.env.chk %>% 
#				group_by(experiment, treatment, block, datetime=ceiling_date(datetime, "10 min", change_on_boundary=F), period, date_rating) %>% 
#				summarize(ct=n(), ct_distinct=n_distinct(days_since_last) ) %>% filter(ct_distinct > 1) %>% print(n=5)

### export
	## remove unneeded columns
	data.env.exp = data.env %>% select(-time_sunrise, -time_sunset)
	
	## export
	data.env.exp %>% filter(experiment == "A-2-row") %>% write_csv(file="./2_data_curated/rasp-caneb_04d_environ_exp-A-2-row_final-10min.csv", na="", col_names=T, append=F)
	data.env.exp %>% filter(experiment == "B-3-row") %>% write_csv(file="./2_data_curated/rasp-caneb_04d_environ_exp-B-3-row_final-10min.csv", na="", col_names=T, append=F)
	data.env.exp %>% filter(experiment == "C-3-row") %>% write_csv(file="./2_data_curated/rasp-caneb_04d_environ_exp-C-3-row_final-10min.csv", na="", col_names=T, append=F)


#######################
# E. Aggregate - Hour #
#######################

### assign periods by hour
	# using existing 10-min period data
		# could not figure out how to reconcile rounding at midnight
		# datetime is rounded up (e.g., right-edge data, 23:10 is rounded to midnight) but sunrise/set is rounded to nearest hour
		# this splits one midnight timestamp into two rows with sun rise/set times on different dates
	# create date column for joining
		
	## split off period data for aggregating to hourly; also drop date_rating, days_since_last
	data.env.hr = data.env %>% select(-date_rating, -days_since_last, -time_sunrise, -time_sunset, -period) 
	data.env.hr.sun = data.env %>% 
		select(-date_rating, -days_since_last) %>% 
		select(datetime, time_sunrise, time_sunset) 

	## get solar times for each date
		# collapse to day
		data.env.hr.sun = data.env.hr.sun %>% 
			group_by(date=as_date(floor_date(datetime, "day")), time_sunrise, time_sunset) %>%
			summarize(ct=n()) %>%
			ungroup()
		
		# round to nearest hour
		data.env.hr.sun = data.env.hr.sun %>% mutate(time_sunrise=round_date(time_sunrise, "hour"), time_sunset=round_date(time_sunset, "hour"))

	## aggregate to hourly
		# summarize
		data.env.hr = data.env.hr %>% 
			group_by(experiment, treatment, block, datetime=ceiling_date(datetime, "hour", change_on_boundary=F), date=date(datetime) ) %>%
			summarize(
				temp_avg=mean(temp_avg, na.rm=T), 
				rh_avg=mean(rh_avg, na.rm=T),
				dpt_avg=mean(dpt_avg, na.rm=T) ) %>%
			ungroup()
			
	## join solar, date data
	data.env.hr = data.env.hr %>% left_join(data.env.hr.sun, by=c("date" = "date") )

	## assign solar periods
		# add blank column
		data.env.hr = data.env.hr %>% mutate(period=as.character(NA))

		# fill
		data.env.hr = data.env.hr %>% mutate(period=replace(period, ( (datetime >= ceiling_date(time_sunrise, "hour")) & (datetime <= ceiling_date(time_sunset, "hour")) ), "day") )
		data.env.hr = data.env.hr %>% mutate(period=replace(period, ( (datetime <  ceiling_date(time_sunrise, "hour")) | (datetime >  ceiling_date(time_sunset, "hour")) ), "night") )

		# remove columns
		data.env.hr = data.env.hr %>% select(-date)

	## assign rating date; begin date of range not needed for first date because logging did not start until after treatments were applied
	data.env.hr = data.env.hr %>% mutate(date_rating=case_when(
		(											date(datetime) <= as_date("2018-02-13")) ~ as_date("2018-02-13"),
		(date(datetime) > as_date("2018-02-13") &	date(datetime) <= as_date("2018-03-13")) ~ as_date("2018-03-13"),
		(date(datetime) > as_date("2018-03-13") &	date(datetime) <= as_date("2018-04-11")) ~ as_date("2018-04-11"),
		(date(datetime) > as_date("2018-09-25") &	date(datetime) <= as_date("2018-10-17")) ~ as_date("2018-10-17"),
		(date(datetime) > as_date("2018-10-17") &	date(datetime) <= as_date("2018-11-07")) ~ as_date("2018-11-07"),
		(date(datetime) > as_date("2018-11-07") &	date(datetime) <= as_date("2018-11-28")) ~ as_date("2018-11-28") ) )

	## join
	data.env.hr = data.env.hr %>% left_join(rating_interval, by=c("date_rating" = "date_rating"))

### calculate variables
	# dew point depression
	data.env.hr = data.env.hr %>% mutate(dpt_depress=temp_avg-dpt_avg)
	
	# vapor pressure deficit
	data.env.hr = data.env.hr %>% mutate(vpd_avg=vapor.pres.def(temp=temp_avg, rh=rh_avg) )
	
### finalize
	## add columns for statistical analysis
		# datetime components: week, day of week, hour
		data.env.hr = data.env.hr %>% mutate(week=isoweek(datetime), day_of_week=wday(datetime), hour=hour(datetime) )

	## round data
	data.env.hr = data.env.hr %>% mutate(
		temp_avg	=round(temp_avg	  , digits=2),
		rh_avg		=round(rh_avg	  , digits=1),
		dpt_avg		=round(dpt_avg	  , digits=2),
		dpt_depress =round(dpt_depress, digits=2),
		vpd_avg		=round(vpd_avg	  , digits=3) )

	## filter out times outside rating period
	data.env.hr = data.env.hr %>% filter(!is.na(date_rating))

	## remove first timestamp because it is NA (logging started midway through hour?) and messes with date_rating summary statistics calculations
	data.env.hr = data.env.hr %>% filter(!(datetime == as_datetime("2018-01-16 13:00:00", tz="Etc/GMT+8")) )
	
	## remove unneeded columns
	data.env.hr = data.env.hr %>% select(-time_sunrise, -time_sunset, -ct)
	
	## reorder columns
	data.env.hr = data.env.hr %>% select(experiment, treatment, block, datetime, week, day_of_week, hour, period, date_rating, days_since_last, temp_avg, rh_avg, dpt_avg, dpt_depress, vpd_avg)

	## check data
		# check NAs
		data.env.hr %>% group_by(experiment, treatment, block) %>% summarize(ct=n(), ct_na_temp=sum(is.na(temp_avg)), ct_na_rh=sum(is.na(rh_avg)) ) %>% print(n=Inf)

		# check for >1 row/>1 date_rating per hour
		data.env.hr %>% group_by(experiment, treatment, block, week, day_of_week, datetime=ceiling_date(datetime, "hour", change_on_boundary=F), period) %>% summarize(ct=n(), ct_ratingdate=n_distinct(date_rating)) %>% filter(ct > 1 | ct_ratingdate > 1)

#		# check for extra rows from grouping
#		data.env.hr.chk = data.env.hr %>% 
#			group_by(experiment, treatment, block, datetime=ceiling_date(datetime, "hour", change_on_boundary=F), period, date_rating, days_since_last) %>%
#			summarize(ct=n()) %>%
#			ungroup()
#			
#			data.env.hr.chk %>% 
#				group_by(experiment, treatment, block, datetime=ceiling_date(datetime, "hour", change_on_boundary=F), date_rating, days_since_last) %>% 
#				summarize(ct=n(), ct_distinct=n_distinct(period) ) %>% filter(ct_distinct > 1) %>% print(n=5)
#	
#			data.env.hr.chk %>% 
#				group_by(experiment, treatment, block, datetime=ceiling_date(datetime, "hour", change_on_boundary=F), period, date_rating, days_since_last) %>% 
#				summarize(ct=n(), ct_distinct=n_distinct(date(datetime)) ) %>% filter(ct_distinct > 1) %>% print(n=5)
#	
#			data.env.hr.chk %>% 
#				group_by(experiment, treatment, block, datetime=ceiling_date(datetime, "hour", change_on_boundary=F), period, days_since_last) %>% 
#				summarize(ct=n(), ct_distinct=n_distinct(date_rating) ) %>% filter(ct_distinct > 1) %>% print(n=5)
#	
#			data.env.hr.chk %>% 
#				group_by(experiment, treatment, block, datetime=ceiling_date(datetime, "hour", change_on_boundary=F), period, date_rating) %>% 
#				summarize(ct=n(), ct_distinct=n_distinct(days_since_last) ) %>% filter(ct_distinct > 1) %>% print(n=5)
	
### export
	write_csv(data.env.hr, file="./2_data_curated/rasp-caneb_04e_environ_final-hour.csv", na=".", col_names=T, append=F)


######################
# F. Aggregate - Day #
######################

### remove unneeded columns
#	data.env.day = data.env.hr %>% select(-days_since_last, -dpt_avg, -dpt_depress)
		
## summarize by day; instead of converting to long first, variables are summarized separately because are rounded differently
	data.env.day = data.env.hr %>% 
		group_by(experiment, treatment, block, week, day_of_week, date=date(datetime), period, date_rating, days_since_last) %>%
		summarize(temp_avg=mean(temp_avg, na.rm=T), rh_avg=mean(rh_avg, na.rm=T), dpt_avg=mean(dpt_avg, na.rm=T), dpt_depress=mean(dpt_depress, na.rm=T), vpd_avg=mean(vpd_avg, na.rm=T) ) %>%
		ungroup()
		
### round
	data.env.day = data.env.day %>% mutate(
		temp_avg	=round(temp_avg	  , digits=2),
		rh_avg		=round(rh_avg	  , digits=1),
		dpt_avg		=round(dpt_avg	  , digits=2),
		dpt_depress =round(dpt_depress, digits=2),
		vpd_avg		=round(vpd_avg	  , digits=3) )

## check data
	# check NAs
	data.env.day %>% group_by(experiment, treatment, block) %>% summarize(ct=n(), ct_na_temp=sum(is.na(temp_avg)), ct_na_rh=sum(is.na(rh_avg)) ) %>% print(n=Inf)

	# check for >1 row/>1 date_rating per date-period
	data.env.day %>% group_by(experiment, treatment, block, week, day_of_week, date, period) %>% summarize(ct=n(), ct_ratingdate=n_distinct(date_rating)) %>% filter(ct > 1 | ct_ratingdate > 1)

### export
	write_csv(data.env.day, file="./2_data_curated/rasp-caneb_04f_environ_final-day.csv", na=".", col_names=T, append=F)


##############################
# G. Aggregate - Rating Date #
##############################
		
## summarize by day; instead of converting to long first, variables are summarized separately because are rounded differently
	data.env.rat = data.env.hr %>% 
		group_by(experiment, treatment, block, date_rating, days_since_last, period) %>%
		summarize(temp_avg=mean(temp_avg, na.rm=T), rh_avg=mean(rh_avg, na.rm=T), dpt_avg=mean(dpt_avg, na.rm=T), dpt_depress=mean(dpt_depress, na.rm=T), vpd_avg=mean(vpd_avg, na.rm=T) ) %>%
		ungroup()
		
### round
	data.env.rat = data.env.rat %>% mutate(
		temp_avg	=round(temp_avg	  , digits=2),
		rh_avg		=round(rh_avg	  , digits=1),
		dpt_avg		=round(dpt_avg	  , digits=2),
		dpt_depress =round(dpt_depress, digits=2),
		vpd_avg		=round(vpd_avg	  , digits=3) )

## check data
	# check NAs
	data.env.rat %>% group_by(experiment, treatment, block) %>% summarize(ct=n(), ct_na_temp=sum(is.na(temp_avg)), ct_na_rh=sum(is.na(rh_avg)) ) %>% print(n=Inf)

### export
	write_csv(data.env.rat, file="./2_data_curated/rasp-caneb_04g_environ_final-rating.csv", na=".", col_names=T, append=F)

