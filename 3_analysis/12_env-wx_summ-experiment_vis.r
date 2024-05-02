##############################
# RASPBERRY Cane Botrytis 	 #
# Environment, Local Weather #
# Summarize by Rating period #
##############################
# compare ranches

## built on Docker putmanlab/exploratory-analysis:420.0

library(conflicted)

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

library(dplyr)
library(forcats)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)

#remotes::install_version("patchwork", version="1.2.0", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never") # tried for collecting axes
library(patchwork) # for graphing: wrap_plots

setwd("/home/raspb_botrytis")

source("./3_analysis/rcb_functions.r")


#############
# A. Import #
#############

### upload
	## environment, epidemiology variables
	data.ee.day = read_csv(file="./2_data_curated/rasp-caneb_10a_environ-epidem_day.csv", col_names=T, na="")
	
	## weather
	data.wx.hr = read_csv(file="./2_data_curated/rasp-caneb_11a_weather_hour.csv", col_names=T, na="")
	data.wx.day = read_csv(file="./2_data_curated/rasp-caneb_11b_weather_day.csv", col_names=T, na="")

### express time in/convert timezone to UTC-8; datetime was converted to UTC on export
	data.wx.hr = data.wx.hr %>% mutate(across(c(datetime, time_sunrise, time_sunset), ~ with_tz(.x, tzone="Etc/GMT+8") ) )
	

######################################
# B. Summarize Ranches - Environ/Epi #
######################################

### aggregate by rating date
	## temp, rh
	summ.ee.rat = data.ee.day %>% 
		group_by(experiment, date_rating, var_env) %>%
		summarize(value_env=round( mean(value_env, na.rm=TRUE), digits=1) ) %>%
		ungroup()
		
### determine number of observation days (when calculated above result differs for lwe_ vs other variables
	## filter for one variable
	summ.ee.rat.nday = data.ee.day %>% filter(var_env == "rh_day")
	
	## calculate
	summ.ee.rat.nday = summ.ee.rat.nday %>% 
		group_by(experiment, date_rating, var_env) %>%
		summarize(n_day=n_distinct(date) ) %>%
		ungroup()
		
	## remove unneeded column
	summ.ee.rat.nday = summ.ee.rat.nday %>% select(-var_env)
	
	## join
	summ.ee.rat = summ.ee.rat %>% left_join(summ.ee.rat.nday, by=c(c("experiment","date_rating")) )	

### organize
	## remove A-2-row
#	summ.ee.rat = summ.ee.rat %>% filter(experiment != "A-2-row")	

	## pick variables of interest
	summ.ee.rat = summ.ee.rat %>% filter(var_env %in% c("rh_day","rh_night","vpd_day","vpd_night","temp_day","temp_night","lwe_rh85_all","lwe_rh90_all","temp_time1525","temprh_rh90") )
	
	## spread
	summ.ee.rat.w = summ.ee.rat %>% spread(key=var_env, value=value_env)
	
### export
	write_csv(summ.ee.rat.w, file="./4_results/12b_environ_compare-ranch_table.csv", na="", append=F, col_names=T)


##################################
# C. Summarize Ranches - Weather #
##################################
# by date_group

### aggregate by date_group
	## temp, rh, vpd
	summ.wx.rat.1 = data.wx.hr %>% 
		group_by(date_group, period) %>%
		summarize(
			n_day=n_distinct(date),
			temp_avg=round( mean(temp_avg, na.rm=TRUE), digits=1),
			rh_avg  =round( mean(rh_avg,   na.rm=TRUE), digits=1),
			vpd_avg =round( mean(vpd_avg,  na.rm=TRUE), digits=3) ) %>%
		ungroup()
		
	## precip - first sum by date_group (no diurnal period), then average between stations
		# sum
		summ.wx.rat.2 = data.wx.day %>% 
			group_by(date_group, station) %>%
			summarize(n_day=n_distinct(date), precip=sum(precip, na.rm=TRUE) ) %>%
			ungroup()
		
		# average between stations
		summ.wx.rat.2 = summ.wx.rat.2 %>% 
			group_by(date_group) %>%
			summarize(n_day=mean(n_day), precip=round( mean(precip, na.rm=TRUE), digits=0) ) %>%
			ungroup()
	
	## wind - by date, no period
	summ.wx.rat.3 = data.wx.hr %>% 
		group_by(date_group ) %>%
		summarize(n_day=n_distinct(date), wind_speed=round( mean(wind_speed, na.rm=TRUE), digits=1) ) %>%
		ungroup()
		
	## join (only .2. and .3 due to lack of period)
	summ.wx.rat.23 = summ.wx.rat.2 %>% left_join(summ.wx.rat.3, by=c(c("date_group","n_day")) )

### organize set 1
	## gather
	summ.wx.rat.1 = summ.wx.rat.1 %>% gather(key="var_z", value="value_wx", -date_group, -period)
	
	## get new variable
	summ.wx.rat.1 = summ.wx.rat.1 %>% mutate(var_wx=case_when(
		(var_z == "n_day" & period == "day") ~ "nday_day",
		(var_z == "n_day" & period == "night") ~ "nday_night",
		(var_z == "rh_avg" & period == "day") ~ "rh_day",
		(var_z == "rh_avg" & period == "night") ~ "rh_night",
		(var_z == "vpd_avg" & period == "day") ~ "vpd_day",
		(var_z == "vpd_avg" & period == "night") ~ "vpd_night",
		(var_z == "temp_avg" & period == "day") ~ "temp_day",
		(var_z == "temp_avg" & period == "night") ~ "temp_night") )
		
	## remove old columns
	summ.wx.rat.1 = summ.wx.rat.1 %>% select(-period, -var_z)
	
	## spread
	summ.wx.rat.1 = summ.wx.rat.1 %>% spread(key=var_wx, value=value_wx)

### finalize
	## join
	summ.wx.rat.w = summ.wx.rat.1 %>% left_join(summ.wx.rat.23, by=c(c("date_group")) )

	## remove NA
	summ.wx.rat.w = summ.wx.rat.w %>% filter(!is.na(date_group) )
	
	## export
	write_csv(summ.wx.rat.w, file="./4_results/12c_weather_compare-ranch_table.csv", na="", append=F, col_names=T)


###################################
# D. Combine Environment, Weather #
###################################

### prepare environment
	## remove unneeded variables
	summ.ee.rat = summ.ee.rat %>% filter(var_env %in% c("rh_day","rh_night","vpd_day","vpd_night","temp_day","temp_night","lwe_rh85_all","lwe_rh90_all") )

	## add data type column
	summ.ee.rat = summ.ee.rat %>% mutate(type="canopy")
	
	## change column names
	summ.ee.rat = summ.ee.rat %>% rename(date_group=date_rating, var_=var_env, value_=value_env)	
	
### prepare weather
	## gather
		## check if n_day all the same
		summ.wx.rat.w %>% filter(nday_day != nday_night | nday_day != n_day | nday_night != n_day)
	
		## remove unneeded columns
		summ.wx.rat = summ.wx.rat.w %>% select(-nday_day, -nday_night) 
	
		## gather
		summ.wx.rat = summ.wx.rat %>% gather(key="var_wx", value="value_wx", -date_group, -n_day)
	
	## add experiment column
	summ.wx.rat = summ.wx.rat %>% mutate(experiment=case_when(
		(date_group %in% as_date(c("2018-01-05","2018-02-13","2018-03-13","2018-04-11")) ) ~ "B-3-row",
		(date_group %in% as_date(c("2018-09-25","2018-10-17","2018-11-07","2018-11-28")) ) ~ "C-3-row") )
		
	## add data type column
	summ.wx.rat = summ.wx.rat %>% mutate(type="ambient")

	## change column names
	summ.wx.rat = summ.wx.rat %>% rename(var_=var_wx, value_=value_wx)
	
### bind
	summ.rat = bind_rows(summ.ee.rat, summ.wx.rat)
	
### add days after treatment for commonality
	## calculate
	summ.rat = summ.rat %>% mutate(days_after_trt=case_when(
		(experiment %in% c("A-2-row","B-3-row") ) ~ date_group - as_date("2018-01-05"), 
		(experiment == "C-3-row") ~ date_group - as_date("2018-09-25") ) )

	## convert to integer
	summ.rat = summ.rat %>% mutate(days_after_trt=as.integer(days_after_trt) )
		
### add column for aesthetics
	summ.rat = summ.rat %>% mutate(graph_legend=case_when(
		(experiment == "A-2-row" & type == "canopy") ~ "ranch 1 2-row canopy",
		(experiment == "B-3-row" & type == "canopy") ~ "ranch 1 3-row canopy",
		(experiment == "B-3-row" & type == "ambient") ~ "ranch 1 ambient",
		(experiment == "C-3-row" & type == "canopy") ~ "ranch 2 3-row canopy",
		(experiment == "C-3-row" & type == "ambient") ~ "ranch 2 ambient") )		

### reorder columns
#	summ.rat = summ.rat %>% select(experiment, type, date_group, n_day, days_after_trt, var_, var_grp, period, value_)
	summ.rat = summ.rat %>% select(experiment, type, graph_legend, date_group, n_day, days_after_trt, var_, value_)


################
# E. Visualize #
################

### prepare
	summ.rat = summ.rat %>% mutate(graph_legend=fct_relevel(graph_legend, 
		c("ranch 1 ambient","ranch 1 2-row canopy","ranch 1 3-row canopy","ranch 2 ambient","ranch 2 3-row canopy") ) )

### graph settings
	base.size=10
	
	vec.12.shape = c(16,0,1,17,2)	
	vec.12.color = scales::hue_pal()(10)[c(6,4,6,9,9)]
	vec.12.line = c("solid","12223242","dotted","solid","dotted")
	
### visualize
	## relative humidity
	plot.e1.1 = summ.rat %>% filter(var_ %in% c("rh_day","rh_night") ) %>% {
	ggplot(., aes(x=days_after_trt, y=value_, shape=graph_legend, color=graph_legend, linetype=graph_legend) ) +
		geom_point() +
		geom_line() +
		facet_grid(cols=vars(var_), labeller=labeller(var_=c("rh_day"="Daylight","rh_night"="Night") ) ) +
		scale_x_continuous(limits=c(0,99) ) +
		scale_shape_manual(values=vec.12.shape) +
		scale_color_manual(values=vec.12.color) +
		scale_linetype_manual(values=vec.12.line) +
		theme_bw() +
		theme(axis.title.x=element_blank(), axis.title.y=element_text(size=base.size+2), axis.text.x=element_blank(), axis.text.y=element_text(size=base.size) ) +
		theme(strip.text=element_text(size=base.size+1) ) +
		theme(legend.title=element_blank(), legend.text=element_text(size=base.size, margin=margin(l=-2.75)), legend.position="bottom", legend.margin=margin(t=-5.5), legend.spacing.x=unit(0.3, "lines") ) +
		guides(shape=guide_legend(nrow=5, byrow=TRUE), color=guide_legend(nrow=5, byrow=TRUE), linetype=guide_legend(nrow=5, byrow=TRUE) ) +
		labs(y="Relative humidity (%)")
	}
	
	## temperature
	plot.e1.2 = summ.rat %>% filter(var_ %in% c("temp_day","temp_night") ) %>% {
	ggplot(., aes(x=days_after_trt, y=value_, shape=graph_legend, color=graph_legend, linetype=graph_legend) ) +
		geom_point() +
		geom_line() +
		facet_grid(cols=vars(var_), labeller=labeller(var_=c("temp_day"="Daylight","temp_night"="Night") ) ) +
		scale_x_continuous(limits=c(0,99) ) +
		scale_shape_manual(values=vec.12.shape) +
		scale_color_manual(values=vec.12.color) +
		scale_linetype_manual(values=vec.12.line) +
		theme_bw() +
		theme(axis.title.x=element_blank(), axis.title.y=element_text(size=base.size+2), axis.text.x=element_blank(), axis.text.y=element_text(size=base.size) ) +
		theme(strip.text=element_text(size=base.size+1) ) +
		theme(legend.title=element_blank(), legend.text=element_text(size=base.size, margin=margin(l=-2.75)), legend.position="bottom", legend.margin=margin(t=-5.5), legend.spacing.x=unit(0.3, "lines") ) +
		guides(shape=guide_legend(nrow=5, byrow=TRUE), color=guide_legend(nrow=5, byrow=TRUE), linetype=guide_legend(nrow=5, byrow=TRUE) ) +
		labs(y="Temperature (C)")
	}

	## surface wetness estimates
	plot.e1.3 = summ.rat %>% filter(var_ %in% c("lwe_rh85_all","lwe_rh90_all") ) %>% {
	ggplot(., aes(x=days_after_trt, y=value_, shape=graph_legend, color=graph_legend, linetype=graph_legend) ) +
		geom_point() +
		geom_line() +
		facet_grid(cols=vars(var_), labeller=labeller(var_=c("lwe_rh85_all"="eSW rh>85%","lwe_rh90_all"="eSW rh>90%") ) ) +
		scale_x_continuous(limits=c(0,99) ) +
		scale_y_continuous(limits=c(0,14) ) +
		scale_shape_manual(values=vec.12.shape[c(2,3,5)]) +
		scale_color_manual(values=vec.12.color[c(2,3,5)]) +
		scale_linetype_manual(values=vec.12.line[c(2,3,5)]) +
		theme_bw() +
		theme(axis.title=element_text(size=base.size+2), axis.text=element_text(size=base.size) ) +
		theme(strip.text=element_text(size=base.size+1) ) +
		theme(legend.position="none" ) +
		guides(shape=guide_legend(nrow=5, byrow=TRUE), color=guide_legend(nrow=5, byrow=TRUE), linetype=guide_legend(nrow=5, byrow=TRUE) ) +
		labs(x="End of event period (days after treatment)", y="Hours/day meeting threshold")
	}

	## precip
	plot.e1.4.1 = summ.rat %>% filter(var_ == "precip" ) %>% {
	ggplot(., aes(x=days_after_trt, y=value_, shape=graph_legend, color=graph_legend, linetype=graph_legend) ) +
		geom_point() +
		geom_line() +
		facet_grid(cols=vars(var_), labeller=labeller(var_=c("precip"="Precipitation")) ) +
		scale_x_continuous(limits=c(0,99) ) +
		scale_shape_manual(values=vec.12.shape[c(1,4)]) +
		scale_color_manual(values=vec.12.color[c(1,4)]) +
		scale_linetype_manual(values=vec.12.line[c(1,4)]) +
		theme_bw() +
		theme(axis.title.x=element_blank(), axis.title.y=element_text(size=base.size+2), axis.text.x=element_blank(), axis.text.y=element_text(size=base.size) ) +
		theme(strip.text=element_text(size=base.size+1) ) +
		theme(legend.position="none" ) +
		labs(y="Precipitation (mm)")
	}

	## wind
	plot.e1.4.2 = summ.rat %>% filter(var_ == "wind_speed" ) %>% {
	ggplot(., aes(x=days_after_trt, y=value_, shape=graph_legend, color=graph_legend, linetype=graph_legend) ) +
		geom_point() +
		geom_line() +
		facet_grid(cols=vars(var_), labeller=labeller(var_=c("wind_speed"="Wind")) ) +
		scale_x_continuous(limits=c(0,99) ) +
		scale_shape_manual(values=vec.12.shape[c(1,4)]) +
		scale_color_manual(values=vec.12.color[c(1,4)]) +
		scale_linetype_manual(values=vec.12.line[c(1,4)]) +
		theme_bw() +
		theme(axis.title.x=element_blank(), axis.title.y=element_text(size=base.size+2), axis.text=element_text(size=base.size) ) +
		theme(strip.text=element_text(size=base.size+1) ) +
		theme(legend.position="none" ) +
		labs(y="Wind speed (m/s)")
	}
	
	plot.e1 = wrap_plots(plot.e1.1, plot.e1.4.1, plot.e1.2, plot.e1.4.2, plot.e1.3, guide_area(), ncol=2, widths=c(2,0.95), guides="collect" )
		# can collect axes/axis titles in patchwork v1.2.0 but could not get to work
		
	ggplot2::ggsave(file="./4_results/12e1_env-wx_compare-ranch.png", device="png", plot=plot.e1, width=6.5, height=7, units="in", dpi=600)

### visualize - with vapor pressure deficit
	## vapor pressure deficit
	plot.e2.5 = summ.rat %>% filter(var_ %in% c("vpd_day","vpd_night") ) %>% {
	ggplot(., aes(x=days_after_trt, y=value_, shape=graph_legend, color=graph_legend, linetype=graph_legend) ) +
		geom_point() +
		geom_line() +
		facet_grid(cols=vars(var_), labeller=labeller(var_=c("vpd_day"="Daylight","vpd_night"="Night") ) ) +
		scale_x_continuous(limits=c(0,99) ) +
		scale_shape_manual(values=vec.12.shape) +
		scale_color_manual(values=vec.12.color) +
		scale_linetype_manual(values=vec.12.line) +
		theme_bw() +
		theme(axis.title.x=element_blank(), axis.title.y=element_text(size=base.size+2), axis.text.x=element_blank(), axis.text.y=element_text(size=base.size) ) +
		theme(strip.text=element_text(size=base.size+1) ) +
		theme(legend.title=element_blank(), legend.text=element_text(size=base.size, margin=margin(l=-2.75)), legend.position="bottom", legend.margin=margin(t=-5.5), legend.spacing.x=unit(0.3, "lines") ) +
		guides(shape=guide_legend(nrow=5, byrow=TRUE), color=guide_legend(nrow=5, byrow=TRUE), linetype=guide_legend(nrow=5, byrow=TRUE) ) +
		labs(y="Vapor pressure deficit (kPa)")
	}
	
	plot.e2 = wrap_plots(plot.e1.1, plot.e1.4.1, plot.e2.5, plot.e1.4.2, plot.e1.2, guide_area(), plot.e1.3, plot_spacer(), ncol=2, widths=c(2,0.98), guides="collect" )

	ggplot2::ggsave(file="./4_results/12e2_env-wx_compare-ranch_vpd.png", device="png", plot=plot.e2, width=6.5, height=8.5, units="in", dpi=600)

