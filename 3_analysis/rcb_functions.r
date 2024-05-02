#######################################
# RASPBERRY Cane Botrytis 			  #
# Define Functions					  #
#######################################


###
### 04_environ_organize.r
###

### calculate saturation vapor pressure (from Alduchov and Eskridge, called August-Roche-Magnus)
	# https://en.wikipedia.org/wiki/Vapour_pressure_of_water#Accuracy_of_different_formulations

	# temp = temperature in C
	# rh = relative humidity in %
	# e_s (output) = saturation vapor pressure in kPa
	
sat.vapor.pres = function(temp) {
	# saturation vapor pressure
	e_s = 0.61094 * 
		exp( 
			(17.625 * temp) /
			(243.04 + temp)
		)

	# output
	return(e_s)
}


### calculate vapor pressure deficit
	# see sat.vapor.pres() above for references
	
	# temp = temperature in C
	# rh = relative humidity in %
	# vpd (output) = vapor pressure deficit in kPa
	
vapor.pres.def = function(temp, rh) {
	# saturation vapor pressure
	e_s = sat.vapor.pres(temp=temp)
		
	# actual vapor pressure
	e_a= e_s * (rh/100)
		
	# vapor pressure deficit
	vpd = e_a - e_s

	# output
	return(vpd)
}
	

###
### z_05-1_environ_summ-vis_hour_not-shown.r
###

### plot treatment with min/max value
	# df.in = input dataframe with diff_max_2, diff_3_min, diff_min_max joined to original summ.env.hr grouped by week, day of week, hour
	# var.env = environmental variable (rh_avg, temp_avg, dpt_depress)
	# var.exp = experiment (A-2-row, B-3-row, C-3-row)
	# var.period = solar period (day, night)
	# y.limit = upper range of y axis
	# interval = summary interval (hour, day)
	# section.code = number and letter for section of code in file (06b, 06c, ...)
	
plot.explore = function(df.in, var.env, var.exp, var.period, y.limit, interval, section.code, width, height) {
	## filter
	df.in = df.in %>% filter(var_env == var.env & experiment == var.exp & period == var.period)
	
	## set facet
	facets = function(interval) {
			   if (interval == "hour") { facet_grid(rows=vars(hour), cols=vars(block) )
		} else if (interval == "day")  { facet_grid(rows=vars(block) )
		} else { print('Invalid interval (set facet)') }
	}
	
	## plot
	p = ggplot(df.in, aes(x=date) ) +
		geom_point(aes(y=diff_value, shape=treatment, color=treatment), size=0.75) +
		geom_text(data={ . %>% filter(flag_mult_max == 1) }, y=y.limit, label="*", size=2) +
		geom_text(data={ . %>% filter(flag_mult_min == 1) }, y=-y.limit, label="^", size=2) +
		facets(interval=interval) +
		scale_y_continuous(limits=c(-(y.limit*1.1),(y.limit*1.1)) ) +
		scale_shape_manual(values=c(0,1,2,5) ) +
		theme_bw() +
		theme(legend.position="bottom")
	
	## save
		# update variables
		var.exp = var.exp %>% str_replace_all(c("-2-row" = "","-3-row" = "") )
		var.env = var.env %>% str_replace_all(c("dpt_depress" = "dpt-depress", "rh_avg" = "rh", "temp_avg" = "temp") )
		
		# save		
		ggplot2::ggsave(file=paste("./4_results/", section.code, "_env_", interval, "_minmax_block_", var.env, "_exp-", var.exp, "_", var.period, ".png", sep=""), device="png", plot=p, width=width, height=height, units="in", dpi=600)
}


### plot raw daily data by rep
	# df.in = input dataframe by day, period in wide format
	# var.env = environmental variable (rh_avg, temp_avg, dpt_depress)
	# var.exp = experiment (A-2-row, B-3-row, C-3-row)
	# var.period = solar period (day, night)
	# filename.prefix = string to append to beginning of output filenames (NA, "z_not-shown_")
	# section.code = number and letter for section of code in file (06b, 06c, ...)
	
plot.day.raw = function(df.in, var.env, var.exp, var.period, filename.prefix, section.code) {
	## convert to long (to simplify selecting variable in function)
	df.in = df.in %>% gather(key="var_env", value="value_env", -experiment, -treatment, -block, -week, -day_of_week, -date, -period, -date_rating, -days_since_last)

	## filter
	df.in = df.in %>% filter(var_env == var.env & experiment == var.exp & period == var.period)
	
	## plot
	p = ggplot(df.in, aes(x=date, y=value_env, linetype=treatment, color=treatment) ) +
		geom_line(size=0.4) +
		facet_grid(rows=vars(block) ) +
		theme_bw() +
		theme(legend.position="bottom")
	
	## save
		# update variables
		var.exp = var.exp %>% str_replace_all(c("-2-row" = "","-3-row" = "") )
		var.env = var.env %>% str_replace_all(c("dpt_depress" = "dpt-depress", "rh_avg" = "rh", "temp_avg" = "temp") )

		# save
		ggplot2::ggsave(file=paste("./4_results/", filename.prefix, "_", section.code, "_env_day_raw_block_", var.env, "_exp-", var.exp, "_", var.period, ".png", sep=""), device="png", plot=p, width=10, height=9, units="in", dpi=600)
}


### plot epidemiology summary data by rep
	# df.in = input epidemiology summary stat dataframe by day in long format
	# var.env = environmental variable (rh_avg, temp_avg, dpt_depress)
	# var.exp = experiment (A-2-row, B-3-row, C-3-row)
	# filename.prefix = string to append to beginning of output filenames (NA, "z_not-shown_")
	# section.code = number and letter for section of code in file (06b, 06c, ...)
	
plot.day.epi.summ = function(df.in, var.env, var.exp, filename.prefix, section.code) {
	## filter
	df.in = df.in %>% filter(var_env == var.env & experiment == var.exp)
	
	## plot
	p = ggplot(df.in, aes(x=date, y=value_env, linetype=treatment, color=treatment) ) +
		geom_line(size=0.4) +
		facet_grid(rows=vars(block) ) +
		theme_bw() +
		theme(legend.position="bottom")
	
	## save
		# update variables
		var.exp = var.exp %>% str_replace_all(c("-2-row" = "","-3-row" = "") )
		var.env = var.env %>% str_replace_all(c(
			"temp_time10_all" = "temp-10", "temp_time15_all" = "temp-15", "temp_time1525_all" = "temp-1525", 
			"temprh_rh80_all" = "temprh-80", "temprh_rh90_all" = "temprh-90", 
			"lwe_rh75_all" = "lwe-rh75", "lwe_rh80_all" = "lwe-rh80", "lwe_rh85_all" = "lwe-rh85", "lwe_rh90_all" = "lwe-rh90",
			"lwe_dpd18_all" = "lwe-dpd18", "lwe_dpd20_all" = "lwe-dpd20") )

		# save
		ggplot2::ggsave(file=paste("./4_results/", filename.prefix, "_", section.code, "_env_day_epi-summ_block_", var.env, "_exp-", var.exp, ".png", sep=""), device="png", plot=p, width=10, height=9, units="in", dpi=600)
}


###
### 04_weather_org-vis.r, 07_environ_summ_analysis.r
###

### calculate extreme values; used internally in epi.summ()
	# see parameter definitions in epi.summ
	
extreme.value = function(df.in, cols.grp, extreme.period) {
	# calculate extreme values
		   if (extreme.period == "hour") 	  { summ.extreme = df.in %>% 
		   		group_by(across({{cols.grp}}), datetime=	ceiling_date(datetime, "hour",   change_on_boundary=F),		 date_rating, days_since_last ) %>%
				summarize(
	   				rh_max_hr = max(rh_avg),
	   				temp_min_hr = min(temp_avg) ) %>%
		   		ungroup()				   			
	} else if (extreme.period == "date")  	  { summ.extreme = df.in %>% 
				group_by(across({{cols.grp}}), date=as_date(ceiling_date(datetime, "day", change_on_boundary=F)),		 date_rating, days_since_last ) %>%
				summarize(
	   				rh_max_day = max(rh_avg),
			   		temp_min_day = min(temp_avg) ) %>%
			   	ungroup()
	} else if (extreme.period == "date_adj")  { summ.extreme = df.in %>% 
				group_by(across({{cols.grp}}), date_adj, date_rating, days_since_last ) %>%
				summarize(
	   				rh_max_dateadj = max(rh_avg),
			   		temp_min_dateadj = min(temp_avg) ) %>%
			   	ungroup()
	} else { print('Invalid extreme.period') }

	return(summ.extreme)

}

### identify lw periods; used internally in epi.summ()
	# df.in = hourly data
	# in.interval = interval of input data (possible: "hour")
	# cols.grp = treatment columns to separate calculations by: example, experiment, treatment, block; or, weather station
	
lw.period = function(df.in, in.interval, cols.grp, temp_onset, temp_offset) {
	### check settings
	if (in.interval == "hour") { df.in = df.in
	} else if (in.interval == "10 min") {
		df.in = NULL
		print('Requires hourly data')
	} else { print('Invalid in.interval') }

	# arrange (not sure if needed)
	lw.df = df.in %>% arrange(across({{cols.grp}}), datetime )

	# determine if dpt_depress is below onset threshold
	lw.df = lw.df %>% mutate(
		temp_below_onset=case_when(
			 (dpt_depress < temp_onset) ~ 1,
			(dpt_depress >= temp_onset) ~ 0) )

	# start temp; for verification
	# determine periods when dpt_depress consecutively remains below dry threshold
	lw.df = lw.df %>% mutate(
		temp_below_offset = as.integer(if_else((
			dpt_depress < temp_offset &  lag(dpt_depress, n=1) < temp_offset |
			dpt_depress < temp_offset & lead(dpt_depress, n=1) < temp_offset),
			1, 0) ) )
	# end temp

	# identify dry/end of wetness events: first row that dpt_depress exceeded dry treshold
	lw.df = lw.df %>% mutate(start_dry_period=as.integer(
		if_else(dpt_depress > temp_offset & lag(dpt_depress, n=1) <= temp_offset, 1, 0)))	

	# identify wet periods 
		# group to create periods between start of dry events 
		# within each period, find start of wetness by cumulative sum; cumsum will not >0 until start of wet event
		# https://stackoverflow.com/questions/49148190/create-sequential-counter-starting-with-event-and-zeros-before-event-for-groups?noredirect=1&lq=1
		# from testing group_by experiment, treatment, block not necessary
	lw.df = lw.df %>%
		group_by(
			across({{cols.grp}}), date_rating, days_since_last,
			dry_event_period = with(rle(start_dry_period), rep(seq_along(lengths), lengths))) %>%
		mutate(wet_period=cumsum(cummax(temp_below_onset))) %>%
		ungroup()

	# convert flag column to 0/1 integer
	lw.df = lw.df %>%	
		mutate(leaf_wet_period=case_when(
			(wet_period > 0) ~ as.integer(1),
			(wet_period == 0) ~ as.integer(0)) )

	# clean up
	lw.df = lw.df %>% select(
		-dry_event_period, -wet_period,
		-temp_below_onset, -temp_below_offset, -start_dry_period)

	# output
	return(lw.df)
}


### calculate epidemiological summary statistics
	# df.in = 10 min. or hourly data
	# in.interval = interval of input data (possible: "10 min", "hour")
	# out.interval = interval of output data (possible: "10 min","hour","date","date_adj","date_rating")
	# cols.grp = treatment columns to separate calculations by: example, experiment, treatment, block; or, weather station
	# extreme.method = how extreme values are handled (possible: "none","direct","average")
		# none = only possible option for out.interval = "10 min"
		# direct = extreme values are directly carried through and not summarized further
		# average
	# extreme.period = period over which to calculate min/max (possible: "hour","date","date_adj")
		# e.g., day = one min/max value per day for out.interval = "date_rating"; hour = one min/max value per hour for out.interval = "date"/"date_adj"
		# min/max value is then averaged for each out.interval period
	# data.source = type of data (possible: "environment", "weather")
	# lw.x.onset/offset = temperature thresholds for leaf wetness period calculation by dew point depress; x = paired set
	
epi.summ = function(df.in, in.interval, out.interval, cols.grp, extreme.method, extreme.period, data.source, lw.1.onset, lw.1.offset, lw.2.onset, lw.2.offset) {
	### check settings
		## show input settings and df
		paste("Show df and input settings. in.interval = ", in.interval, sep="")
		df.in %>% group_by(across({{cols.grp}}), datetime=ceiling_date(datetime, "hour", change_on_boundary=F) ) %>% summarize(ct=n()) %>% arrange(desc(ct)) %>% print(n=4)
		df.in %>% group_by(across({{cols.grp}}), date=as_date(ceiling_date(datetime, "day", change_on_boundary=F)) ) %>% summarize(ct=n()) %>% arrange(desc(ct)) %>% print(n=4)
	
		## check setting combinations
			# set flag
				   if (extreme.method == "average" & in.interval %in% c("10 min","hour") & extreme.period == "date"		  				  & out.interval == "date_rating")  { flag = 1 	
			} else if (extreme.method == "average" & in.interval %in% c("10 min","hour") & extreme.period == "date_adj"		  			  & out.interval == "date_rating")  { flag = 1 	
			} else if (extreme.method == "direct"  & in.interval == "10 min" 			 & extreme.period == "hour" 		   			  & out.interval == "hour") 	    { flag = 1
			} else if (extreme.method == "direct"  & in.interval %in% c("10 min","hour") & extreme.period == "date"						  & out.interval == "date") 	    { flag = 1 
			} else if (extreme.method == "direct"  & in.interval %in% c("10 min","hour") & extreme.period == "date_adj"					  & out.interval == "date_adj")     { flag = 1 
			} else if (extreme.method == "none"	   & in.interval == "10 min"			 & extreme.period %in% c("hour","date","date_adj") & out.interval == "10 min")	    { flag = 1
			} else if (extreme.method == "none"	   & in.interval == "hour"				 & extreme.period %in% c("hour","date","date_adj") & out.interval == "hour")	    { flag = 1
			} else if (extreme.method == "none"	   & in.interval == "hour"				 & extreme.period %in% c("hour","date","date_adj") & out.interval == "date")	    { flag = 1
			} else if (extreme.method == "none"	   & in.interval == "hour"				 & extreme.period %in% c("hour","date","date_adj") & out.interval == "date_rating") { flag = 1
			} else { flag = 0 }
			
			# evaluate flag
			if (flag == 1) { df.in = df.in 
			} else if (flag == 0) { 
				df.in = NULL 
				print('Error: invalid combination of extreme.method, in.interval, extreme.period, out.interval')
			} else { print('Error in flag formula') }
			
		## check output interval and grouping variables
		print('Ensure proper grouping with appending date_rating, days_since_last.')
			if (out.interval == "10 min") 	  { 
			   	print('Check by ceiling_date(out.interval)')
			   	df.in %>% group_by(across({{cols.grp}}), datetime=	  ceiling_date(datetime, "10 min", change_on_boundary=F)								 ) %>% summarize(ct=n()) %>% arrange(desc(ct)) %>% print(n=4)
				print('Check with adding date_rating, days_since_last')
			   	df.in %>% group_by(across({{cols.grp}}), datetime=	  ceiling_date(datetime, "10 min", change_on_boundary=F), date_rating, days_since_last	 ) %>% summarize(ct=n()) %>% arrange(desc(ct)) %>% print(n=4)
		} else if (out.interval == "hour") { 
			   	print('Check by ceiling_date(out.interval)')						
				df.in %>% group_by(across({{cols.grp}}), datetime=	  ceiling_date(datetime, "hour",   change_on_boundary=F)								 ) %>% summarize(ct=n()) %>% arrange(desc(ct)) %>% print(n=4)
				print('Check with adding date_rating, days_since_last')
				df.in %>% group_by(across({{cols.grp}}), datetime=	  ceiling_date(datetime, "hour",   change_on_boundary=F), date_rating, days_since_last	 ) %>% summarize(ct=n()) %>% arrange(desc(ct)) %>% print(n=4)
		} else if (out.interval == "date") { 
				print('Check by ceiling_date(out.interval)')
				df.in %>% group_by(across({{cols.grp}}), date=as_date(ceiling_date(datetime, "day",    change_on_boundary=F) )						  		 ) %>% summarize(ct=n()) %>% arrange(desc(ct)) %>% print(n=4)
				print('Check with adding date_rating, days_since_last')
				df.in %>% group_by(across({{cols.grp}}), date=as_date(ceiling_date(datetime, "day",    change_on_boundary=F) ), date_rating, days_since_last ) %>% summarize(ct=n()) %>% arrange(desc(ct)) %>% print(n=4)
		} else if (out.interval == "date_adj") { 
				print('Check by ceiling_date(out.interval)')
				df.in %>% group_by(across({{cols.grp}}), date_adj,						  																	 ) %>% summarize(ct=n()) %>% arrange(desc(ct)) %>% print(n=4)
				print('Check with adding date_rating, days_since_last')
				df.in %>% group_by(across({{cols.grp}}), date_adj, 																date_rating, days_since_last ) %>% summarize(ct=n()) %>% arrange(desc(ct)) %>% print(n=4)
  		} else if (out.interval == "date_rating") {
				print('Check by ceiling_date(out.interval)') 						
 				df.in %>% group_by(across({{cols.grp}}), 																	 	date_rating					 ) %>% summarize(ct=n()) %>% arrange(desc(ct)) %>% print(n=4)
 				print('Check with adding date_rating, days_since_last')
 				df.in %>% group_by(across({{cols.grp}}), 																		date_rating, days_since_last ) %>% summarize(ct=n()) %>% arrange(desc(ct)) %>% print(n=4)
		} else { print('Invalid out.interval') }

	### direct from observations - temp/rh
		## set grouping
			   if (out.interval == "10 min") 	  { summ.grp = df.in %>% group_by(across({{cols.grp}}), datetime=	 ceiling_date(datetime, "10 min", change_on_boundary=F), date_adj, date_rating, days_since_last, period )
		} else if (out.interval == "hour") 		  { summ.grp = df.in %>% group_by(across({{cols.grp}}), datetime=	 ceiling_date(datetime, "hour",   change_on_boundary=F), date_adj, date_rating, days_since_last, period )
		} else if (out.interval == "date") 		  { summ.grp = df.in %>% group_by(across({{cols.grp}}), date=as_date(ceiling_date(datetime, "day",    change_on_boundary=F) ), date_rating, days_since_last )
 		} else if (out.interval == "date_adj")	  { summ.grp = df.in %>% group_by(across({{cols.grp}}), date_adj, 															   date_rating, days_since_last )
		} else if (out.interval == "date_rating") { summ.grp = df.in %>% group_by(across({{cols.grp}}), 																	   date_rating, days_since_last )
		} else { print('Invalid out.interval') }
		
		## temp/rh
			# calculate
			if (out.interval %in% c("10 min","hour")) {
				summ.1 = summ.grp %>% summarize(
					rh_avg_all 			= round( mean(rh_avg, na.rm=T), digits=1),
					temp_avg_all		= round( mean(temp_avg, na.rm=T), digits=2),
					temp_time10_all 	= sum(temp_avg > 10),
					temp_time15_all 	= sum(temp_avg > 15),
					temp_time1525_all	= sum(temp_avg > 15 & temp_avg < 25),
					temprh_rh80_all		= sum(temp_avg > 15 & temp_avg < 25 & rh_avg > 80),
					temprh_rh90_all		= sum(temp_avg > 15 & temp_avg < 25 & rh_avg > 90),
					lwe_rh75_all		= sum(rh_avg > 75),
					lwe_rh80_all		= sum(rh_avg > 80),
					lwe_rh85_all		= sum(rh_avg > 85),
					lwe_rh90_all		= sum(rh_avg > 90) )
			
			} else if (out.interval %in% c("date","date_adj","date_rating")) {
				summ.1 = summ.grp %>% summarize(
#					rh_avg_all 			= round( mean(rh_avg, na.rm=T), digits=1),
#					rh_avg_day			= round( mean(rh_avg[period == "day"], na.rm=T), digits=1), 
#					rh_avg_night		= round( mean(rh_avg[period == "night"], na.rm=T), digits=1),
#					temp_avg_all		= round( mean(temp_avg, na.rm=T), digits=2),
#					temp_avg_day		= round( mean(temp_avg[period == "day"], na.rm=T), digits=2),
#					temp_avg_night		= round( mean(temp_avg[period == "night"], na.rm=T), digits=2),
					temp_time10_all 	= sum(temp_avg > 10),
					temp_time15_all 	= sum(temp_avg > 15),
					temp_time1525_all	= sum(temp_avg > 15 & temp_avg < 25),
					temprh_rh80_all		= sum(temp_avg > 15 & temp_avg < 25 & rh_avg > 80),
					temprh_rh90_all		= sum(temp_avg > 15 & temp_avg < 25 & rh_avg > 90),
					lwe_rh75_all		= sum(rh_avg > 75),
					lwe_rh80_all		= sum(rh_avg > 80),
					lwe_rh85_all		= sum(rh_avg > 85),
					lwe_rh90_all		= sum(rh_avg > 90) )
			} else { print('Invalid out.interval') }
			
			# ungroup
			summ.1 = summ.1 %>% ungroup()
		
	### direct from observations - wind
		   if (data.source == "weather")  { 
		   		summ.2 = summ.grp %>% summarize(
		   			wind_avg_all=mean(wind_speed, na.rm=T),
		   			precip_sum_all = sum(precip, na.rm=T) ) %>% ungroup
	} else if (data.source == "environment") { print('Environmental data: precip, wind not included.') 
	} else { print('Invalid data.source') }
				
	### extreme values 
		if (extreme.method == "direct") { 
			## determine extreme value for given period
			summ.3 = extreme.value(df.in=df.in, cols.grp=cols.grp, extreme.period=extreme.period)
	
		} else if (extreme.method == "average") {
			## determine extreme value for given period
			summ.3.t = extreme.value(df.in=df.in, cols.grp=cols.grp, extreme.period=extreme.period)
			
			## calculate mean of extreme values for a given period; extreme.period = day and out.interval = date_rating required for method = "average"
			if (extreme.period == "date") {
				summ.3 = summ.3.t %>% 
					group_by(across({{cols.grp}}), date_rating, days_since_last ) %>%
					summarize(
						rh_avg_maxday   = round( mean(rh_max_day, na.rm=T), digits=1),
						temp_avg_minday = round( mean(temp_min_day, na.rm=T), digits=2) ) %>%
					ungroup()
			} else if (extreme.period == "date_adj") {
				summ.3 = summ.3.t %>% 
					group_by(across({{cols.grp}}), date_rating, days_since_last ) %>%
					summarize(
						rh_avg_maxdateadj   = round( mean(rh_max_dateadj, na.rm=T), digits=1),
						temp_avg_mindateadj = round( mean(temp_min_dateadj, na.rm=T), digits=2) ) %>%
					ungroup()
			} else { print('Invalid extreme.period') }
		} else if (extreme.method == "none") { summ.3 = NULL 
		} else { print('Invalid extreme.method') }
		
	### leaf wetness duration by dew point depression
		## run functions
			# run
			summ.4.18 = lw.period(df.in=df.in, in.interval="hour", cols.grp=cols.grp, temp_onset=lw.1.onset, temp_offset=lw.1.offset)
			summ.4.20 = lw.period(df.in=df.in, in.interval="hour", cols.grp=cols.grp, temp_onset=lw.2.onset, temp_offset=lw.2.offset)

			# rename columns
			summ.4.18 = summ.4.18 %>% rename(leaf_wet_18=leaf_wet_period)
			summ.4.20 = summ.4.20 %>% rename(leaf_wet_20=leaf_wet_period)

			# join
				# determine if columns present
				if ("date_adj" %in% colnames(summ.4.18)) { col.dateadj = "date_adj" } else { col.dateadj = NULL }
				
				# set weather columns
					   if (data.source == "environment") { col.wx = NULL
				} else if (data.source == "weather") { col.wx = c("precip","wind_speed")
				} else { print('Invalid data.source') }
				
				# join columns; use col. variables for columns that are present				
				summ.4 = summ.4.18 %>% left_join(summ.4.20, by=c(c( 
					{{cols.grp}}, "datetime", "period", "date_rating", "days_since_last", 
					{{col.dateadj}}, "temp_avg", "rh_avg", "dpt_avg", "dpt_depress", {{col.wx}} )) )
			
		## calculate summary stats
			# set group_by
				   if (out.interval == "hour") { 
				   		summ.4 = summ.4 %>% group_by(across({{cols.grp}}), datetime=	ceiling_date(datetime, "hour",change_on_boundary=F), date_adj, date_rating, days_since_last, period )
			} else if (out.interval == "date") { 
				   		summ.4 = summ.4 %>% group_by(across({{cols.grp}}), date=as_date(ceiling_date(datetime, "day", change_on_boundary=F) ), date_rating, days_since_last )
 			} else if (out.interval == "date_adj") { 
 						summ.4 = summ.4 %>% group_by(across({{cols.grp}}), date_adj, 														   date_rating, days_since_last )
			} else if (out.interval == "date_rating") { 
						summ.4 = summ.4 %>% group_by(across({{cols.grp}}), 																	   date_rating, days_since_last )
			} else { print('Invalid out.interval') }
			
			# summarize
			summ.4 = summ.4 %>% summarize(
				lwe_dpd18_all = sum(leaf_wet_18 == 1),
				lwe_dpd20_all = sum(leaf_wet_20 == 1) ) %>% ungroup()
	
	### join summary variables
		## prep additional joining columns
			   if (out.interval == "hour")		  { cols.join = c("datetime", "date_adj", "date_rating", "days_since_last", "period")
		} else if (out.interval == "date") 		  { cols.join = c("date", 	  "date_rating", "days_since_last")
		} else if (out.interval == "date_adj") 	  { cols.join = c("date_adj", "date_rating", "days_since_last")
		} else if (out.interval == "date_rating") { cols.join = c(			  "date_rating", "days_since_last")
		} else { print("Invalid out.interval") }

		## join	
		if (data.source == "weather") {
			if (extreme.method %in% c("average","direct")) {
				summ.epi = summ.1 %>%
					left_join(summ.2, by=c(c( {{cols.grp}}, {{cols.join}} )) ) %>%
					left_join(summ.3, by=c(c( {{cols.grp}}, {{cols.join}} )) ) %>%
					left_join(summ.4, by=c(c( {{cols.grp}}, {{cols.join}} )) )
			} else if (extreme.method == "none") {
				summ.epi = summ.1 %>%
					left_join(summ.2, by=c(c( {{cols.grp}}, {{cols.join}} )) ) %>%
					left_join(summ.4, by=c(c( {{cols.grp}}, {{cols.join}} )) )
			} else { print('Invalid extreme.method') }
		} else if (data.source == "environment") {
			if (extreme.method %in% c("average","direct")) {
				summ.epi = summ.1 %>%
					left_join(summ.3, by=c(c( {{cols.grp}}, {{cols.join}} )) ) %>%
					left_join(summ.4, by=c(c( {{cols.grp}}, {{cols.join}} )) )
			} else if (extreme.method == "none") {
				summ.epi = summ.1 %>%
					left_join(summ.4, by=c(c( {{cols.grp}}, {{cols.join}} )) )
			} else { print('Invalid extreme.method') }
		} else { print('Invalid data.source') }
		
	### convert to long
		   if (data.source == "environment") { summ.epi.l = summ.epi %>% gather(key="var_env", value="value_env", -{{cols.grp}}, -{{cols.join}} )
	} else if (data.source == "weather") 	 { summ.epi.l = summ.epi %>% gather(key="var_wx",  value="value_wx",  -{{cols.grp}}, -{{cols.join}} )
	} else { print('Invalid data.source') }
	
	### manually insert NAs where data is missing; summary calc above used na.rm=T; this results in 0s but data is truly missing
		## show summary
		if (data.source == "environment") { 
			summ.epi.l %>% 
				group_by( across({{cols.grp}}) ) %>% 
				summarize(ct=n(), ct_na=sum(is.na(value_env)), ct_0=sum(var_env == "temp_avg_all" & value_env == 0) ) %>% 
				print(n=Inf)
			summ.epi.l %>% 
				filter(experiment == "C-3-row" & treatment == "manual" & block == 2) %>%
				group_by(var_env) %>% 
				summarize(ct=n(), ct_na=sum(is.na(value_env)), ct_0=sum(value_env == 0) )
		}
			
		## replace with NA
		if (data.source == "environment") {
			summ.epi.l = summ.epi.l %>% mutate(value_env=replace(value_env, experiment == "A-2-row" & treatment == "control" & block == 2 & value_env == 0, NA))
			summ.epi.l = summ.epi.l %>% mutate(value_env=replace(value_env, experiment == "B-3-row" & treatment == "control" & block == 1 & value_env == 0, NA))
			summ.epi.l = summ.epi.l %>% mutate(value_env=replace(value_env, experiment == "B-3-row" & treatment == "control" & block == 4 & value_env == 0, NA))
			summ.epi.l = summ.epi.l %>% mutate(value_env=replace(value_env, experiment == "B-3-row" & treatment == "manual" & block == 2 & value_env == 0, NA))
			summ.epi.l = summ.epi.l %>% mutate(value_env=replace(value_env, experiment == "C-3-row" & treatment == "manual" & block == 4 & value_env == 0, NA))
		} else if (data.source == "weather") 	 { summ.epi.l = summ.epi.l
		} else { print('Invalid data.source') }
	
	### output
	return(summ.epi.l)
	
}

