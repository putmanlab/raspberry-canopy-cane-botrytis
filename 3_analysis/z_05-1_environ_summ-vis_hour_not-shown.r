###########################
# RASPBERRY Cane Botrytis #
# Environmental Data	  #
# Summarize and Visualize #
# Hourly				  #
###########################

## built on Docker putmanlab/exploratory-analysis:420.0

library(conflicted)

library(dplyr)
library(forcats)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)

library(patchwork) # for graphing: wrap_plots
library(RColorBrewer) # for brewer.pal()

# install ggdensity for correlation of large n data points
remotes::install_version("ggdensity", version="1.0.0", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
library(ggdensity)

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/raspb_botrytis")

source("./3_analysis/rcb_functions.r")


#############
# A. Import #
#############

### hour
	## import 
	data.env.hr = read_csv(file="./2_data_curated/rasp-caneb_04e_environ_final-hour.csv", col_names=T, na=".")
	
	## express time in/convert timezone to UTC-8; datetime was converted to UTC on export
	data.env.hr = data.env.hr %>% mutate(datetime=with_tz(datetime, tzone="Etc/GMT+8") )

### summary for Results text - overall experiment average
	data.env.hr %>% group_by(experiment) %>% summarize(rh_avg=round( mean(rh_avg, na.rm=TRUE), digits=1), temp_avg=round( mean(temp_avg, na.rm=TRUE), digits=2) )
	
	 
###########################
# B. Hour - Visualize Raw #
###########################

### relative humidity
	## A-2-row
	plot.b.ra = data.env.hr %>% filter(experiment == "A-2-row") %>% {
	ggplot(., aes(x=datetime, y=rh_avg, color=treatment, linetype=treatment) ) +
		geom_line(size=0.25) +
		facet_grid(rows=vars(experiment, block) ) +
		scale_x_datetime(date_breaks="1 week", date_minor_breaks="2 days", date_labels="%b %d" ) +
		theme_bw() +
		theme(legend.position="bottom") +
		labs(y="Relative humidity (%)")
	}
	ggplot2::ggsave(file="./4_results/z_05-1b_env_hour_raw_rh_exp-A_not-shown.png", device="png", plot=plot.b.ra, width=16, height=9, units="in", dpi=600)

	## B-3-row
	plot.b.rb = data.env.hr %>% filter(experiment == "B-3-row") %>% {
	ggplot(., aes(x=datetime, y=rh_avg, color=treatment, linetype=treatment) ) +
		geom_line(size=0.25) +
		facet_grid(rows=vars(experiment, block) ) +
		scale_x_datetime(date_breaks="1 week", date_minor_breaks="2 days", date_labels="%b %d" ) +
		theme_bw() +
		theme(legend.position="bottom") +
		labs(y="Relative humidity (%)")
	}
	ggplot2::ggsave(file="./4_results/z_05-1b_env_hour_raw_rh_exp-B_not-shown.png", device="png", plot=plot.b.rb, width=16, height=9, units="in", dpi=600)

	## C-3-row
	plot.b.rc = data.env.hr %>% filter(experiment == "C-3-row") %>% {
	ggplot(., aes(x=datetime, y=rh_avg, color=treatment, linetype=treatment) ) +
		geom_line(size=0.25) +
		facet_grid(rows=vars(experiment, block) ) +
		scale_x_datetime(date_breaks="1 week", date_minor_breaks="2 days", date_labels="%b %d" ) +
		theme_bw() +
		theme(legend.position="bottom") +
		labs(y="Relative humidity (%)")
	}
	ggplot2::ggsave(file="./4_results/z_05-1b_env_hour_raw_rh_exp-C_not-shown.png", device="png", plot=plot.b.rc, width=16, height=9, units="in", dpi=600)

### temperature
	## A-2-row
	plot.b.ta = data.env.hr %>% filter(experiment == "A-2-row") %>% {
	ggplot(., aes(x=datetime, y=temp_avg, color=treatment, linetype=treatment) ) +
		geom_line(size=0.25) +
		facet_grid(rows=vars(experiment, block) ) +
		scale_x_datetime(date_breaks="1 week", date_minor_breaks="2 days", date_labels="%b %d" ) +
		theme_bw() +
		theme(legend.position="bottom") +
		labs(y="Temperature (C)")
	}
	ggplot2::ggsave(file="./4_results/z_05-1b_env_hour_raw_temp_exp-A_not-shown.png", device="png", plot=plot.b.ta, width=16, height=9, units="in", dpi=600)

	## B-3-row
	plot.b.tb = data.env.hr %>% filter(experiment == "B-3-row") %>% {
	ggplot(., aes(x=datetime, y=temp_avg, color=treatment, linetype=treatment) ) +
		geom_line(size=0.25) +
		facet_grid(rows=vars(experiment, block) ) +
		scale_x_datetime(date_breaks="1 week", date_minor_breaks="2 days", date_labels="%b %d" ) +
		theme_bw() +
		theme(legend.position="bottom") +
		labs(y="Temperature (C)")
	}
	ggplot2::ggsave(file="./4_results/z_05-1b_env_hour_raw_temp_exp-B_not-shown.png", device="png", plot=plot.b.tb, width=16, height=9, units="in", dpi=600)

	## C-3-row
	plot.b.tc = data.env.hr %>% filter(experiment == "C-3-row") %>% {
	ggplot(., aes(x=datetime, y=temp_avg, color=treatment, linetype=treatment) ) +
		geom_line(size=0.25) +
		facet_grid(rows=vars(experiment, block) ) +
		scale_x_datetime(date_breaks="1 week", date_minor_breaks="2 days", date_labels="%b %d" ) +
		theme_bw() +
		theme(legend.position="bottom") +
		labs(y="Temperature (C)")
	}
	ggplot2::ggsave(file="./4_results/z_05-1b_env_hour_raw_temp_exp-C_not-shown.png", device="png", plot=plot.b.tc, width=16, height=9, units="in", dpi=600)
	
	
##################################
# C. Hour - Exploratory Analysis #
##################################
# determine which treatment(s) represents min/max for that hour and block, each day
	# visually determine if a specific treatment (e.g., control) was consistently min/max across dates, hours, and blocks

### calculate min/max and difference between next highest/lowest value
	## add date column
	summ.env.hr = data.env.hr %>% mutate(date=date(datetime) )

	## gather variables
	summ.env.hr = summ.env.hr %>% gather(key="var_env", value="value_env", -experiment, -treatment, -block, -datetime, -date, -week, -day_of_week, -hour, -period, -date_rating, -days_since_last)
	
	## determine min/max (separate object); order values within each group to extract ranked values
	summ.env.hr.mm = summ.env.hr %>% 
		group_by(experiment, block, date, week, day_of_week, hour, period, date_rating, days_since_last, var_env) %>%
		arrange(desc(value_env) ) %>%
		summarize(value_1=max(value_env, na.rm=TRUE), value_2=value_env[2], value_3=value_env[3], value_4=min(value_env, na.rm=TRUE) ) %>%
		ungroup()
		
		# checks
			# 1 (max) = 2; 4 (min) = 3
#			summ.env.hr.mm %>% filter(value_1 == value_2 | value_3 == value_4)
			# 1 = 3; 4 = 2
#			summ.env.hr.mm %>% filter(value_1 == value_3 | value_2 == value_4)
			# 1 = 4
#			summ.env.hr.mm %>% filter(value_1 == value_4)

	## add comparison flag for min/max ties
		# add empty columns
		summ.env.hr.mm = summ.env.hr.mm %>% mutate(flag_1eq2=as.integer(NA), flag_1eq3=as.integer(NA), flag_2eq4=as.integer(NA), flag_3eq4=as.integer(NA) )
		
		# fill
		summ.env.hr.mm = summ.env.hr.mm %>% mutate(
			flag_1eq2=replace(flag_1eq2, value_1 == value_2, 1),
			flag_1eq3=replace(flag_1eq3, value_1 == value_3, 1),
			flag_2eq4=replace(flag_2eq4, value_2 == value_4, 1),
			flag_3eq4=replace(flag_3eq4, value_3 == value_4, 1) )
	
	## calculate difference between max/next highest and 2nd lowest/min
	summ.env.hr.mm = summ.env.hr.mm %>% mutate(
		diff_max_2=case_when(
			(is.na(flag_1eq2) & is.na(flag_1eq3) ) ~ value_1 - value_2,
			(flag_1eq2 == 1   & is.na(flag_1eq3) ) ~ value_1 - value_3,
			(flag_1eq2 == 1   & flag_1eq3 == 1)    ~ value_1 - value_4),
		diff_3_min=case_when(
			(flag_2eq4 == 1   & flag_3eq4 == 1)    ~ value_4 - value_1,
			(is.na(flag_2eq4) & flag_3eq4 == 1)    ~ value_4 - value_2,
			(is.na(flag_2eq4) & is.na(flag_3eq4) ) ~ value_4 - value_3),
		diff_min_max=value_1 - value_4)

		# check
		summ.env.hr.mm %>% filter(is.na(diff_max_2) | is.na(diff_3_min) )
		
	## condense flag columns
	summ.env.hr.mm = summ.env.hr.mm %>% mutate(
		flag_mult_max=case_when(
			(flag_1eq2 == 1 | flag_1eq3 == 1) ~ 1L,
			(is.na(flag_1eq2) & is.na(flag_1eq3) ) ~ 0L),
		flag_mult_min=case_when(
			(flag_2eq4 == 1 | flag_3eq4 == 1) ~ 1L,
			(is.na(flag_2eq4) & is.na(flag_3eq4) ) ~ 0L) )
			
	## remove flag columns
	summ.env.hr.mm = summ.env.hr.mm %>% select(-flag_1eq2, -flag_1eq3, -flag_2eq4, -flag_3eq4)		

### join and filter
	## join
	summ.env.hr = summ.env.hr %>% left_join(summ.env.hr.mm, by=c(c("experiment","block","date","week","day_of_week","hour","period","date_rating","days_since_last","var_env")) )
	
	## filter
	summ.env.hr = summ.env.hr %>% filter(value_env == value_1 | value_env == value_4)
	
	## transfer diff value
	summ.env.hr = summ.env.hr %>% mutate(diff_value=case_when(
		(value_env == value_1) ~ diff_max_2,
		(value_env == value_4) ~ diff_3_min) )
		
		# check
		summ.env.hr %>% filter(is.na(diff_value) )

### visualize - over dates 
	# note: commented out because no consistent trends observed
	
	## relative humidity
#	plot.explore(df.in=summ.env.hr, var.env="rh_avg", var.exp="A-2-row", var.period="day", y.limit=7.5, interval="hour", section.code="05-1c1", width=16, height=9)
#	plot.explore(df.in=summ.env.hr, var.env="rh_avg", var.exp="A-2-row", var.period="night", y.limit=7.5, interval="hour", section.code="05-1c1", width=16, height=9)
#	
#	plot.explore(df.in=summ.env.hr, var.env="rh_avg", var.exp="B-3-row", var.period="day", y.limit=7.5, interval="hour", section.code="05-1c1", width=16, height=9)
#	plot.explore(df.in=summ.env.hr, var.env="rh_avg", var.exp="B-3-row", var.period="night", y.limit=7.5, interval="hour", section.code="05-1c1", width=16, height=9)
#
#	plot.explore(df.in=summ.env.hr, var.env="rh_avg", var.exp="C-3-row", var.period="day", y.limit=7.5, interval="hour", section.code="05-1c1", width=16, height=9)
#	plot.explore(df.in=summ.env.hr, var.env="rh_avg", var.exp="C-3-row", var.period="night", y.limit=7.5, interval="hour", section.code="05-1c1", width=16, height=9)
#
#	## temperature
#	plot.explore(df.in=summ.env.hr, var.env="temp_avg", var.exp="A-2-row", var.period="day", y.limit=1, interval="hour", section.code="05-1c1", width=16, height=9)
#	plot.explore(df.in=summ.env.hr, var.env="temp_avg", var.exp="A-2-row", var.period="night", y.limit=1, interval="hour", section.code="05-1c1", width=16, height=9)
#	
#	plot.explore(df.in=summ.env.hr, var.env="temp_avg", var.exp="B-3-row", var.period="day", y.limit=1, interval="hour", section.code="05-1c1", width=16, height=9)
#	plot.explore(df.in=summ.env.hr, var.env="temp_avg", var.exp="B-3-row", var.period="night", y.limit=1, interval="hour", section.code="05-1c1", width=16, height=9)
#
#	plot.explore(df.in=summ.env.hr, var.env="temp_avg", var.exp="C-3-row", var.period="day", y.limit=1, interval="hour", section.code="05-1c1", width=16, height=9)
#	plot.explore(df.in=summ.env.hr, var.env="temp_avg", var.exp="C-3-row", var.period="night", y.limit=1, interval="hour", section.code="05-1c1", width=16, height=9)
#
		# results: no consistent differences across blocks or over time
	
### visualize - range (treatment max-min) - violin
	# note: commented out, plots unlikely to be useful given that statistical differences have small magnitudes
	# although there are no apparent trends in raw data, if there are statistical differences, is the magnitude large enough to be relevant?

#	## day
#		# relative humidity
#		plot.c2.d.r = summ.env.hr.mm %>% filter(var_env == "rh_avg" & period == "day") %>% {
#		ggplot(., aes(x=experiment, y=diff_min_max) ) +
#			geom_violin(scale="count", draw_quantiles=c(0.25, 0.5, 0.75), size=0.2) +
#			facet_grid(cols=vars(hour) ) +
#			scale_y_continuous(breaks=seq(0,16,by=4), minor_breaks=seq(0,16,by=2) ) +
#			coord_cartesian(ylim=c(0,16) ) +
#			theme_bw() +
#			theme(axis.title.x=element_blank(), axis.text.x=element_blank() ) +
#			theme(axis.title.y=element_text(size=9), axis.text.y=element_text(size=7) ) +
#			labs(y="Relative humidity (% point)\nrange among treatments")
#		}
#
#		# temperature
#		plot.c2.d.t = summ.env.hr.mm %>% filter(var_env == "temp_avg" & period == "day") %>% {
#		ggplot(., aes(x=experiment, y=diff_min_max) ) +
#			geom_violin(scale="count", draw_quantiles=c(0.25, 0.5, 0.75), size=0.2) +
#			facet_grid(cols=vars(hour) ) +
#			scale_y_continuous(breaks=seq(0,4,by=1), minor_breaks=seq(0,4,by=0.25) ) +
#			coord_cartesian(ylim=c(0,4) ) +
#			theme_bw() +
#			theme(axis.title.x=element_blank(), axis.text.x=element_blank() ) +
#			theme(axis.title.y=element_text(size=9), axis.text.y=element_text(size=7) ) +
#			theme(strip.text=element_blank(), strip.background=element_blank() ) +
#			labs(y="Temperature (C)\nrange among treatments")
#		}
#
#		plot.c2.d = wrap_plots(plot.c2.d.r, plot.c2.d.t, ncol=1)
#		
#		ggplot2::ggsave(file="./4_results/z_05-1c2_env_hour_range_day_not-shown.png", device="png", plot=plot.c2.d, width=9, height=5.5, units="in", dpi=600)
#
#	## night
#		# relative humidity
#		plot.c2.n.r = summ.env.hr.mm %>% filter(var_env == "rh_avg" & period == "night") %>% {
#		ggplot(., aes(x=experiment, y=diff_min_max) ) +
#			geom_violin(scale="count", draw_quantiles=c(0.25, 0.5, 0.75), size=0.2) +
#			facet_grid(cols=vars(hour) ) +
#			scale_y_continuous(breaks=seq(0,16,by=4), minor_breaks=seq(0,16,by=2) ) +
#			coord_cartesian(ylim=c(0,16) ) +
#			theme_bw() +
#			theme(axis.title.x=element_blank(), axis.text.x=element_blank() ) +
#			theme(axis.title.y=element_text(size=9), axis.text.y=element_text(size=7) ) +
#			labs(y="Relative humidity (% point)\nrange among treatments")
#		}
#
#		# temperature
#		plot.c2.n.t = summ.env.hr.mm %>% filter(var_env == "temp_avg" & period == "night") %>% {
#		ggplot(., aes(x=experiment, y=diff_min_max) ) +
#			geom_violin(scale="count", draw_quantiles=c(0.25, 0.5, 0.75), size=0.2) +
#			facet_grid(cols=vars(hour) ) +
#			scale_y_continuous(breaks=seq(0,4,by=1), minor_breaks=seq(0,4,by=0.25) ) +
#			coord_cartesian(ylim=c(0,4) ) +
#			theme_bw() +
#			theme(axis.title.x=element_blank(), axis.text.x=element_blank() ) +
#			theme(axis.title.y=element_text(size=9), axis.text.y=element_text(size=7) ) +
#			theme(strip.text=element_blank(), strip.background=element_blank() ) +
#			labs(y="Temperature (C)\nrange among treatments")
#		}
#		
#		plot.c2.n = wrap_plots(plot.c2.n.r, plot.c2.n.t, ncol=1)
#		
#		ggplot2::ggsave(file="./4_results/z_05-1c2_env_hour_range_night_not-shown.png", device="png", plot=plot.c2.n, width=9, height=5.5, units="in", dpi=600)

###########################
# D. Vapor Pressure - Raw #
###########################
# compare vpd with rh

### visualize - overall
	## prepare reference lines
		# set temperatures
		vpd.ref = tibble(
			temp = c(5,10,15,20,25) )
			
		# calculate saturation vapor pressure
		vpd.ref = vpd.ref %>% mutate(e_s=sat.vapor.pres(temp=temp) )

		# determine equation lines
		vpd.ref = vpd.ref %>% mutate(slope=e_s/100, intercept=-e_s)
		
		# factor to order
		vpd.ref = vpd.ref %>% mutate(temp=fct_relevel(as.character(temp), c("5","10","15","20","25") ) )
		
	plot.d1 = ggplot(data.env.hr, aes(x=rh_avg, y=vpd_avg) ) +
		geom_hdr() +
		geom_abline(data=vpd.ref, aes(slope=slope, intercept=intercept, color=temp), linetype="22", alpha=0.5 ) +
		facet_grid(cols=vars(experiment) ) +
		scale_color_manual(values=brewer.pal(6,"OrRd")[2:6] ) +
		theme_bw()
		
	ggplot2::ggsave(file="./4_results/z_05-1d1_env_hour_vpd-rh_all_not-shown.png", device="png", plot=plot.d1, width=16, height=5.33, units="in", dpi=600)
	
		# result: vapor pressure deficit differs significantly over range of temperatures observed when RH held constant
			# evaluate vpd for comparing over time, between experiments
			# however due to small differences between treatments, unlikely to matter for treatment comparisons