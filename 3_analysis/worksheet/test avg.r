###########################
# RASPBERRY Cane Botrytis #
# Testing summarize()	  #
###########################

# test summarize() across different time intervals and across dates

## built on Docker putmanlab/exploratory-analysis:420.0

library(conflicted)

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)

#library(egg) # for graphing: ggarrange
#library(rstatix) # for vectorized t-test functions: t_test
library(photobiology) # for sun rise/set times: sunrise_time, sunset_time

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/raspb_botrytis")

options(pillar.sigfig=3)

### 1. hourly (10 min to hour)
	## create data
	test.1 = tibble(
		datetime = seq(as_datetime("2018-02-01 09:00:00", tz="Etc/GMT+8"), as_datetime("2018-02-01 15:00:00", tz="Etc/GMT+8"), by="10 mins"),
		temp = c(0, rep(c(10^(0:5)), each=6) ) )
		
	## aggregate to hourly
	summ.1a = test.1 %>% 
		group_by(datetime=ceiling_date(datetime, "hour", change_on_boundary=F) ) %>%
		summarize(s1a_boundF=mean(temp, na.rm=T) ) %>%
		ungroup()

	summ.1b = test.1 %>% 
		group_by(datetime=ceiling_date(datetime, "hour", change_on_boundary=T) ) %>%
		summarize(s1b_boundT=mean(temp, na.rm=T) ) %>%
		ungroup()
	
	# compare
	summ.1a %>% left_join(summ.1b)
	
	# conclusion:
		# change_on_boundary = F: 10:00:00 (10 min) is included in 10:00:00 (hour) average
		# change_on_boundary = T: 10:00:00 (10 min) is included in 11:00:00 (hour) average NOT 10:00:00 (hour)
	
### 2. hourly across midnight (10 min to hour)
	## create data
	test.2 = tibble(
		datetime = seq(as_datetime("2018-02-01 21:00:00", tz="Etc/GMT+8"), as_datetime("2018-02-02 03:00:00", tz="Etc/GMT+8"), by="10 mins"),
		temp = c(0, rep(c(10^(0:5)), each=6) ) )
		
	## aggregate to hourly
	summ.2a = test.2 %>% 
		group_by(datetime=ceiling_date(datetime, "hour", change_on_boundary=F) ) %>%
		summarize(s2a_bF=mean(temp, na.rm=T) ) %>%
		ungroup()

	summ.2b = test.2 %>% 
		group_by(datetime=ceiling_date(datetime, "hour", change_on_boundary=F), date=date(datetime) ) %>%
		summarize(s2b_bF_byD=mean(temp, na.rm=T) ) %>%
		ungroup()

	summ.2c = test.2 %>% 
		group_by(datetime=ceiling_date(datetime, "hour", change_on_boundary=T) ) %>%
		summarize(s2c_bT=mean(temp, na.rm=T) ) %>%
		ungroup()

	summ.2d = test.2 %>% 
		group_by(datetime=ceiling_date(datetime, "hour", change_on_boundary=T), date=date(datetime) ) %>%
		summarize(s2d_bT_byD=mean(temp, na.rm=T) ) %>%
		ungroup()

	# compare	
	summ.2a %>% left_join(summ.2b) %>% left_join(summ.2c) %>% left_join(summ.2d)
	
	# conclusions
		# change_on_boundary = F behaves as expected across midnight
		# including date(datetime) does not affect hourly averages (i.e., no double midnight timestamps with two different dates)
	
### 3. hourly across midnight (10 min to hour)
	## create data
	test.3 = tibble(
		datetime = seq(as_datetime("2018-02-01 21:00:00", tz="Etc/GMT+8"), as_datetime("2018-02-02 03:00:00", tz="Etc/GMT+8"), by="10 mins"),
		temp = c(0, rep(c(10^(0:5)), each=6) ) )
		
	## aggregate to hourly
	summ.3a = test.3 %>% 
		group_by(datetime=ceiling_date(datetime, "hour", change_on_boundary=F), date=date(datetime) ) %>%
		summarize(s3a_F_bydate=mean(temp, na.rm=T) ) %>%
		ungroup()

	summ.3b = test.3 %>% 
		group_by(datetime=ceiling_date(datetime, "hour", change_on_boundary=F), date=as_date( floor_date(datetime, "day") ) ) %>%
		summarize(s3b_F_floordate=mean(temp, na.rm=T) ) %>%
		ungroup()

	summ.3c = test.3 %>% 
		group_by(datetime=ceiling_date(datetime, "hour", change_on_boundary=F), date=as_date( ceiling_date(datetime, "day", change_on_boundary=F) ) ) %>%
		summarize(s3c_F_ceildateF=mean(temp, na.rm=T) ) %>%
		ungroup()

	summ.3d = test.3 %>% 
		group_by(datetime=ceiling_date(datetime, "hour", change_on_boundary=T), date=date(datetime) ) %>%
		summarize(s3d_T_bydate=mean(temp, na.rm=T) ) %>%
		ungroup()

	summ.3e = test.3 %>% 
		group_by(datetime=ceiling_date(datetime, "hour", change_on_boundary=T), date=as_date( floor_date(datetime, "day") ) ) %>%
		summarize(s3e_T_floordateF=mean(temp, na.rm=T) ) %>%
		ungroup()

	summ.3f = test.3 %>% 
		group_by(datetime=ceiling_date(datetime, "hour", change_on_boundary=T), date=as_date( ceiling_date(datetime, "day", change_on_boundary=F) ) ) %>%
		summarize(s3f_T_ceildateF=mean(temp, na.rm=T) ) %>%
		ungroup()
	
	# compare
	summ.3a %>% left_join(summ.3b) %>% left_join(summ.3c) %>% left_join(summ.3d) %>% left_join(summ.3e) %>% left_join(summ.3f)
	
	# conclusions
		# results from date=date(datetime) and as_date(floor_date(datetime, "day")) are same
	