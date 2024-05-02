###########################
# RASPBERRY Cane Botrytis #
# Local Weather Data	  #
###########################

## built on Docker putmanlab/exploratory-analysis:420.0

library(conflicted)

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)

library(patchwork) # for graphing: wrap_plots
library(photobiology) # for sun rise/set times: sunrise_time, sunset_time

setwd("/home/raspb_botrytis")

source("./3_analysis/rcb_functions.r")


######################
# A. Import - Hourly #
######################

### cimis
	## import
	wx.cimis = read_csv(file="./2_data/weather/weather_hourly_CIMIS_152_2017-11-04_to_2018-11-28_raw.csv", col_names=T, na=c(""," "), guess_max=10000)

	## select and rename columns
	wx.cimis = wx.cimis %>% select(station_id=`Stn Id`, date=Date, hour=`Hour (PST)`, temp_avg=`Air Temp (C)`, rh_avg=`Rel Hum (%)`, dpt_avg=`Dew Point (C)`, wind_speed=`Wind Speed (m/s)`, precip=`Precip (mm)`)
	
	## change column type
	wx.cimis = wx.cimis %>% mutate(station_id=as.character(station_id))
	
	## convert to datetime
	wx.cimis = wx.cimis %>% mutate(datetime=mdy_hm(paste(date, hour, sep=" ")))
	
	## update timezone; is exported in PST
	wx.cimis = wx.cimis %>% mutate(datetime=force_tz(datetime, tzone="Etc/GMT+8"))
	
	## remove columns
	wx.cimis = wx.cimis %>% select(-date, -hour)

### noaa - integrated surface; downloaded manually for convenience
	## download programmatically 
		# download
#		in.wx.noaa = importNOAA(code="723926-23136", year=c(2017,2018))
	
		# export raw
#		write_csv(in.wx.noaa, file="./2_data/weather/weather_hourly_NOAA-ISH_72392623136_2017-2018.csv", col_names=T, append=F)

	## import
	wx.noaa = read_csv(file="./2_data/weather/weather_hourly_NOAA-ISH_72392623136_2017-2018.csv", col_names=T, na="NA", guess_max=20000)
	
	## select and rename columns
	wx.noaa = wx.noaa %>% select(datetime=date, station_id=code, temp_avg=air_temp, rh_avg=RH, dpt_avg=dew_point, wind_speed=ws)

	## add empty precip column
	wx.noaa = wx.noaa %>% mutate(precip=as.numeric(NA))

	## update timezone; is exported in UTC
	wx.noaa = wx.noaa %>% mutate(datetime=with_tz(datetime, tzone="Etc/GMT+8"))

	## filter
	wx.noaa = wx.noaa %>% filter(date(datetime) >= as_date("2017-11-04") & date(datetime) <= as_date("2018-11-28"))

### organize
	## bind
	data.wx.hr = bind_rows(wx.cimis, wx.noaa)
	
	## add station name
	data.wx.hr = data.wx.hr %>% mutate(station=case_when(
		(station_id == "152") ~ "cimis_152",
		(station_id == "723926-23136") ~ "noaa_723926") )

	## calculate vapor pressure deficit
	data.wx.hr = data.wx.hr %>% mutate(vpd_avg=round( vapor.pres.def(temp=temp_avg, rh=rh_avg), digits=3) )

	## order columns
	data.wx.hr = data.wx.hr %>% select(station_id, station, datetime, temp_avg, rh_avg, vpd_avg, dpt_avg, wind_speed, precip)

### assign day/night
	# set location
	geo_code = data.frame(lon = -119.094, lat = 34.212)

	## get sun rise/set times
		# extract dates
		data.dates = data.wx.hr %>% group_by(date=as_date(floor_date(datetime, "day"))) %>% summarize(ct=n())
	
		## get sunrise, sunset times
		data.sun.wx = data.dates %>% mutate(
									time_sunrise=sunrise_time(date=date, tz="Etc/GMT+8", geocode=geo_code, twilight="none", unit.out="datetime"),
									time_sunset=sunset_time(date=date, tz="Etc/GMT+8", geocode=geo_code, twilight="none", unit.out="datetime") )
		
		## round to hourly
		data.sun.wx = data.sun.wx %>% mutate(time_sunrise=round_date(time_sunrise, "hour"), time_sunset=round_date(time_sunset, "hour"))

	## add date column to facilitate joining
	data.wx.hr = data.wx.hr %>% mutate(date=date(datetime))

	## join sun data
	data.wx.hr = data.wx.hr %>% left_join(data.sun.wx, by=c("date" = "date"))

	## add blank column
	data.wx.hr = data.wx.hr %>% mutate(period=as.character(NA))

	## fill
	data.wx.hr = data.wx.hr %>% mutate(period=replace(period, ( (datetime >= ceiling_date(time_sunrise, "hour")) & (datetime <= ceiling_date(time_sunset, "hour")) ), "day") )
	data.wx.hr = data.wx.hr %>% mutate(period=replace(period, ( (datetime <  ceiling_date(time_sunrise, "hour")) | (datetime >  ceiling_date(time_sunset, "hour")) ), "night") )

## assign date groups; two weeks before treatments, between treatment date and first rating date, then since last rating date
	data.wx.hr = data.wx.hr %>% mutate(date_group=case_when(
		(date(datetime) >= as_date("2017-12-21") &	date(datetime) <= as_date("2018-01-05")) ~ as_date("2018-01-05"),
		(date(datetime) >  as_date("2018-01-05") &	date(datetime) <= as_date("2018-02-13")) ~ as_date("2018-02-13"),
		(date(datetime) >  as_date("2018-02-13") &	date(datetime) <= as_date("2018-03-13")) ~ as_date("2018-03-13"),
		(date(datetime) >  as_date("2018-03-13") &	date(datetime) <= as_date("2018-04-11")) ~ as_date("2018-04-11"),
		(date(datetime) >= as_date("2018-09-10") &	date(datetime) <= as_date("2018-09-25")) ~ as_date("2018-09-25"),
		(date(datetime) >  as_date("2018-09-25") &	date(datetime) <= as_date("2018-10-17")) ~ as_date("2018-10-17"),
		(date(datetime) >  as_date("2018-10-17") &	date(datetime) <= as_date("2018-11-07")) ~ as_date("2018-11-07"),
		(date(datetime) >  as_date("2018-11-07") &	date(datetime) <= as_date("2018-11-28")) ~ as_date("2018-11-28") ) )

	# check
	data.wx.hr %>% 
		group_by(station, date_group) %>% summarize(ct=n(), ct_na=sum(is.na(temp_avg)) ) %>% 
		mutate(n_days=as.integer( date_group-lag(date_group, n=1) ) ) %>%
		mutate(ct_per_day=round( ct/n_days, digits=1) ) %>%
		print(n=Inf)

### export
	write_csv(data.wx.hr, file="./2_data_curated/rasp-caneb_11a_weather_hour.csv", na="", append=F, col_names=T)


#####################
# B. Import - Daily #
#####################

### import
	in.wx.day = read_csv(file="./2_data/weather/weather_daily_ventura_2016-07-01_to_2019-06-30.csv", col_names=T)

	## filter stations
	in.wx.day = in.wx.day %>% filter(station_id %in% c("723926-23136", "152"))

	# filter dates
	in.wx.day = in.wx.day %>% filter(date >= as_date("2017-11-04") & date <= as_date("2018-11-28"))

	## convert units to metric
	in.wx.day = in.wx.day %>% mutate( across(c(temp_avg, temp_min, temp_max), ~ round( ((. - 32)*(5/9)), digits=1) ) )
	in.wx.day = in.wx.day %>% mutate(precip=round( (precip*25.4), digits=2))

	## rename column
	in.wx.day = in.wx.day %>% rename(wind_speed=wind)

### adjust GSOD date; observations appear to come one day later than CIMIS for unknown reason, not in documentation
	## rename column
	data.wx.day = in.wx.day %>% rename(precip_old=precip)

	## get value from next row
	data.wx.day = data.wx.day %>% mutate(precip_lead=lead(precip_old))

	## collect values into one column
	data.wx.day = data.wx.day %>% mutate(precip=case_when(
		(station_id == "152") ~ precip_old,
		(station_id == "723926-23136") ~ precip_lead) )

	## remove unneeded column
	data.wx.day = data.wx.day %>% select(-precip_old, -precip_lead)		

### assign date groups; two weeks before treatments, between treatment date and first rating date, then since last rating date
	data.wx.day = data.wx.day %>% mutate(date_group=case_when(
		(date >= as_date("2017-12-21") &	date <= as_date("2018-01-05")) ~ as_date("2018-01-05"),
		(date >  as_date("2018-01-05") &	date <= as_date("2018-02-13")) ~ as_date("2018-02-13"),
		(date >  as_date("2018-02-13") &	date <= as_date("2018-03-13")) ~ as_date("2018-03-13"),
		(date >  as_date("2018-03-13") &	date <= as_date("2018-04-11")) ~ as_date("2018-04-11"),
		(date >= as_date("2018-09-10") &	date <= as_date("2018-09-25")) ~ as_date("2018-09-25"),
		(date >  as_date("2018-09-25") &	date <= as_date("2018-10-17")) ~ as_date("2018-10-17"),
		(date >  as_date("2018-10-17") &	date <= as_date("2018-11-07")) ~ as_date("2018-11-07"),
		(date >  as_date("2018-11-07") &	date <= as_date("2018-11-28")) ~ as_date("2018-11-28") ) )

	# check
	data.wx.day %>% 
		group_by(station_id, date_group) %>% summarize(ct=n(), ct_na=sum(is.na(temp_avg)) ) %>% 
		mutate(n_days=as.integer( date_group-lag(date_group, n=1) ) ) %>%
		mutate(ct_per_day=round( ct/n_days, digits=1) ) %>%
		print(n=Inf)

### final organization	
	## add station name
	data.wx.day = data.wx.day %>% mutate(station=case_when(
		(station_id == "152") ~ "cimis_152",
		(station_id == "723926-23136") ~ "noaa_723926") )

	## remove columns
	data.wx.day = data.wx.day %>% select(-region, -data_source)

	## reorder columns
	data.wx.day = data.wx.day %>% select(station_id, station, date, date_group, temp_avg, rh_avg, wind_speed, precip)
	
### check 
	## compare precip between stations for possible errors from cimis station
	data.wx.day %>% select(station, date, date_group, precip) %>% spread(key=station, value=precip) %>% filter(!is.na(date_group)) %>% print(n=Inf)

	## change <= 1.02 at cimis to 0
	data.wx.day = data.wx.day %>% mutate(precip=replace(precip, station == "cimis_152" & precip <= 1.02, 0) )

	## show stations side by side
	data.wx.day %>%
		gather(key="var_wx", value="value_wx", -station_id, -station, -date, -date_group) %>%
		mutate(var_wx_new=case_when(
			(station == "noaa_723926") ~ paste("noaa_", var_wx, sep=""),
			(station == "cimis_152") ~ paste("cimis_", var_wx, sep="") ) ) %>%
		select(-station_id, -station, -var_wx) %>%
		spread(key=var_wx_new, value=value_wx) %>%
		select(date, date_group, noaa_temp_avg, cimis_temp_avg, noaa_rh_avg, cimis_rh_avg, noaa_wind_speed, cimis_wind_speed, noaa_precip, cimis_precip) %>%
		filter(
			(date >= as_date("2017-12-15") & date <= as_date("2018-04-11") ) |
			(date >= as_date("2018-09-10") & date <= as_date("2018-11-28") ) ) %>%
		print(n=Inf)

### export
	write_csv(data.wx.day, file="./2_data_curated/rasp-caneb_11b_weather_day.csv", na="", append=F, col_names=T)
			

##############
# C. Prepare #
##############

### hourly - aggregate by date, across stations
	## by period
		# temp, rh
		summ.wx.day.1 = data.wx.hr %>% 
			group_by(date=date(datetime), date_group, period) %>%
			summarize(
				temp_avg=round( mean(temp_avg, na.rm=TRUE), digits=1),
				rh_avg  =round( mean(rh_avg,   na.rm=TRUE), digits=0),
				vpd_avg =round( mean(vpd_avg,  na.rm=TRUE), digits=3) ) %>%
			ungroup()
		
	## precip (removed because hourly data not available for noaa station; just get precip from daily data from both stations)
	
	## wind - by date, no period
	summ.wx.day.3 = data.wx.hr %>% 
		group_by(date=date(datetime), date_group ) %>%
		summarize(wind_speed=round( mean(wind_speed, na.rm=TRUE), digits=1) ) %>%
		ungroup()
		
### daily - aggregate by date, across stations
	## precip
	summ.wx.day.2 = data.wx.day %>%
		group_by(date, date_group) %>%
		summarize(precip=round( mean(precip, na.rm=TRUE), digits=1) ) %>%
		ungroup()

		# replace 0 with NA so a tiny bar is not plotted for every date
		summ.wx.day.2 = summ.wx.day.2 %>% mutate(precip=replace(precip, precip == 0, NA) )

### prepare data
	## add ranch number
	summ.wx.day.1 = summ.wx.day.1 %>% mutate(ranch=case_when(
		(date_group %in% as_date(c("2018-01-05","2018-02-13","2018-03-13","2018-04-11"))) ~ as.character("1"),
		(date_group %in% as_date(c("2018-09-25","2018-10-17","2018-11-07","2018-11-28"))) ~ as.character("2") ) )

	summ.wx.day.2 = summ.wx.day.2 %>% mutate(ranch=case_when(
		(date_group %in% as_date(c("2018-01-05","2018-02-13","2018-03-13","2018-04-11"))) ~ as.character("1"),
		(date_group %in% as_date(c("2018-09-25","2018-10-17","2018-11-07","2018-11-28"))) ~ as.character("2") ) )

	summ.wx.day.3 = summ.wx.day.3 %>% mutate(ranch=case_when(
		(date_group %in% as_date(c("2018-01-05","2018-02-13","2018-03-13","2018-04-11"))) ~ as.character("1"),
		(date_group %in% as_date(c("2018-09-25","2018-10-17","2018-11-07","2018-11-28"))) ~ as.character("2") ) )
		
	## add days after treatment
	summ.wx.day.1 = summ.wx.day.1 %>% mutate(days_after_trt=case_when(
		(ranch == "1") ~ date - as_date("2018-01-05"), 
		(ranch == "2") ~ date - as_date("2018-09-25") ) )

	summ.wx.day.2 = summ.wx.day.2 %>% mutate(days_after_trt=case_when(
		(ranch == "1") ~ date - as_date("2018-01-05"), 
		(ranch == "2") ~ date - as_date("2018-09-25") ) )

	summ.wx.day.3 = summ.wx.day.3 %>% mutate(days_after_trt=case_when(
		(ranch == "1") ~ date - as_date("2018-01-05"), 
		(ranch == "2") ~ date - as_date("2018-09-25") ) )

	## clean up
		# remove data outside period
		summ.wx.day.1 = summ.wx.day.1 %>% filter(!is.na(ranch))
		summ.wx.day.2 = summ.wx.day.2 %>% filter(!is.na(ranch))
		summ.wx.day.3 = summ.wx.day.3 %>% filter(!is.na(ranch))

		# change days_after_trt to integer
		summ.wx.day.1 = summ.wx.day.1 %>% mutate(days_after_trt=as.integer(days_after_trt))
		summ.wx.day.2 = summ.wx.day.2 %>% mutate(days_after_trt=as.integer(days_after_trt))
		summ.wx.day.3 = summ.wx.day.3 %>% mutate(days_after_trt=as.integer(days_after_trt))

	## convert to long
#	data.wx.day.ranch = data.wx.day.ranch %>% gather(key="variable", value="measurement", -station_id, -station, -date, -days_after_trt, -ranch)


################
# D. Visualize #
################

### prepare
	## create df to annotate with rating dates
		## make tibble
		rating.dates = tibble(
			ranch = as.character(c(1,1,1,2,2,2)),
			period= c("day","day","day","day","day","day"),
			date  = as_date(c("2018-02-13","2018-03-13","2018-04-11","2018-10-17","2018-11-07","2018-11-28")),
			label = c("02-13", "03-13", "04-11", "10-17", "11-07", "11-28") )
	
		## calculate days after treatment
		rating.dates = rating.dates %>% mutate(days_after_trt=as.integer(NA))
		rating.dates = rating.dates %>% mutate(days_after_trt=replace(days_after_trt, ranch == "1", date[ranch == "1"] - as_date("2018-01-05")) )
		rating.dates = rating.dates %>% mutate(days_after_trt=replace(days_after_trt, ranch == "2", date[ranch == "2"] - as_date("2018-09-25")) )

	## graph settings
	source("./3_analysis/rcb_settings.r")	

### visualize
	x.breaks=seq(-14,98, by=7)
	## relative humidity
	plot.d1.1 = ggplot(summ.wx.day.1, aes(x=days_after_trt, y=rh_avg, linetype=ranch, color=ranch) ) +
		geom_line(size=0.5) +
		facet_grid(rows=vars(period), labeller=labeller(period=c("day"="During daylight","night"="During night"))  ) +
		scale_x_continuous(breaks=x.breaks ) +
		scale_color_manual(values=vec.exp.color[2:3]) +
		scale_linetype_manual(values=vec.exp.line[2:3]) +
		theme_bw() +
		theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
		theme(legend.margin=margin(t=-2.75) ) +
		guides(linetype=guide_legend(direction="horizontal"), color=guide_legend(direction="horizontal") ) +
		labs(y="Relative humidity (%)")

	## temperature
	plot.d1.2 = ggplot(summ.wx.day.1, aes(x=days_after_trt, y=temp_avg, linetype=ranch, color=ranch) ) +
		geom_line(size=0.5) +
		geom_text(data={rating.dates %>% filter(ranch == 1)}, aes(x=days_after_trt, y=9.5, label=label, color=ranch), size=3, angle=90) +
		geom_text(data={rating.dates %>% filter(ranch == 2)}, aes(x=days_after_trt, y=7.5, label=label, color=ranch), size=3, angle=90) +
		facet_grid(rows=vars(period), labeller=labeller(period=c("day"="During daylight","night"="During night"))  ) +
		scale_x_continuous(breaks=x.breaks ) +
		scale_color_manual(values=vec.exp.color[2:3]) +
		scale_linetype_manual(values=vec.exp.line[2:3]) +
		theme_bw() +
		theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
		theme(legend.margin=margin(t=-2.75) ) +
		guides(linetype="none", color="none" ) +
		labs(y="Temperature (C)")

	## precip
	plot.d1.3 = ggplot(summ.wx.day.2, aes(x=days_after_trt, y=precip, fill=ranch)) +
		geom_bar(aes(group=ranch), stat="identity", position=position_dodge(), size=0, width=0.9) +
		geom_text(data={rating.dates %>% filter(ranch == 1)}, aes(x=days_after_trt, y=35.0, label=label, color=ranch), size=3, angle=90) +
		geom_text(data={rating.dates %>% filter(ranch == 2)}, aes(x=days_after_trt, y=30.0, label=label, color=ranch), size=3, angle=90) +
		scale_x_continuous(breaks=x.breaks ) +
		scale_color_manual(values=vec.exp.color[2:3]) +
		scale_fill_manual(values=vec.exp.color[2:3]) +
		scale_linetype_manual(values=vec.exp.line[2:3]) +
		theme_bw() +
		theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
		theme(legend.margin=margin(t=-2.75) ) +
		guides(fill=guide_legend(direction="horizontal"), color="none" ) +
		labs(y="Rain (mm)")

	## wind
	plot.d1.4 = ggplot(summ.wx.day.3, aes(x=days_after_trt, y=wind_speed, linetype=ranch, color=ranch) ) +
		geom_line(size=0.5) +
		scale_x_continuous(breaks=x.breaks ) +
		scale_color_manual(values=vec.exp.color[2:3]) +
		scale_linetype_manual(values=vec.exp.line[2:3]) +
		theme_bw() +
		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
		theme(legend.margin=margin(t=-2.75) ) +
		guides(linetype=guide_legend(direction="horizontal"), color=guide_legend(direction="horizontal") ) +
		labs(x="Days after treatment", y="Wind speed (m/s)")
	
	plot.d1 = wrap_plots(plot.d1.1, plot.d1.2, plot.d1.3, plot.d1.4, ncol=1, heights=c(2,2,1,1), guides="collect") +
		plot_annotation(theme=theme(legend.position="bottom") )	

	ggplot2::ggsave(file="./4_results/11d1_weather_compare-ranch.png", device="png", plot=plot.d1, width=6.5, height=7, units="in", dpi=600)

#### visualize - with vapor pressure deficit
#	## vapor pressure deficit
#	plot.d2.5 = ggplot(summ.wx.day.1, aes(x=days_after_trt, y=vpd_avg, linetype=ranch, color=ranch) ) +
#		geom_line(size=0.5) +
#		facet_grid(rows=vars(period), labeller=labeller(period=c("day"="During daylight","night"="During night"))  ) +
#		scale_x_continuous(breaks=x.breaks ) +
#		scale_color_manual(values=vec.exp.color[2:3]) +
#		theme_bw() +
#		theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
#		theme(legend.margin=margin(t=-2.75) ) +
#		guides(linetype=guide_legend(direction="horizontal"), color=guide_legend(direction="horizontal") ) +
#		labs(y="Vapor pressure deficit (kPa)")
#
#	plot.d2 = wrap_plots(plot.d1.1, plot.d2.5, plot.d1.2, plot.d1.3, plot.d1.4, ncol=1, heights=c(2,2,2,1,1), guides="collect") +
#		plot_annotation(theme=theme(legend.position="bottom") )	
#
#	ggplot2::ggsave(file="./4_results/z_11d2_weather_compare-ranch_not-shown.png", device="png", plot=plot.d2, width=6.5, height=9.25, units="in", dpi=600)
	
