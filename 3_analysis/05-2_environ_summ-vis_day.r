###########################
# RASPBERRY Cane Botrytis #
# Environmental Data	  #
# Summarize and Visualize #
# Daily					  #
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

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/raspb_botrytis")

source("./3_analysis/rcb_functions.r")


#############
# A. Import #
#############

### day 
	data.env.day = read_csv(file="./2_data_curated/rasp-caneb_04f_environ_final-day.csv", col_names=T, na=".")


##########################
# B. Day - Visualize Raw #
##########################
	# note: commented out because not needed after initial viewing

#### relative humidity
#	plot.day.raw(df.in=data.env.day, var.env="rh_avg", var.exp="A-2-row", var.period="day", filename.prefix=NULL, section.code="05-2b")
#	plot.day.raw(df.in=data.env.day, var.env="rh_avg", var.exp="A-2-row", var.period="night", filename.prefix=NULL, section.code="05-2b")
#	
#	plot.day.raw(df.in=data.env.day, var.env="rh_avg", var.exp="B-3-row", var.period="day", filename.prefix=NULL, section.code="05-2b")
#	plot.day.raw(df.in=data.env.day, var.env="rh_avg", var.exp="B-3-row", var.period="night", filename.prefix=NULL, section.code="05-2b")
#
#	plot.day.raw(df.in=data.env.day, var.env="rh_avg", var.exp="C-3-row", var.period="day", filename.prefix=NULL, section.code="05-2b")
#	plot.day.raw(df.in=data.env.day, var.env="rh_avg", var.exp="C-3-row", var.period="night", filename.prefix=NULL, section.code="05-2b")
#
#### temperature
#	plot.day.raw(df.in=data.env.day, var.env="temp_avg", var.exp="A-2-row", var.period="day", filename.prefix=NULL, section.code="05-2b")
#	plot.day.raw(df.in=data.env.day, var.env="temp_avg", var.exp="A-2-row", var.period="night", filename.prefix=NULL, section.code="05-2b")
#	
#	plot.day.raw(df.in=data.env.day, var.env="temp_avg", var.exp="B-3-row", var.period="day", filename.prefix=NULL, section.code="05-2b")
#	plot.day.raw(df.in=data.env.day, var.env="temp_avg", var.exp="B-3-row", var.period="night", filename.prefix=NULL, section.code="05-2b")
#
#	plot.day.raw(df.in=data.env.day, var.env="temp_avg", var.exp="C-3-row", var.period="day", filename.prefix=NULL, section.code="05-2b")
#	plot.day.raw(df.in=data.env.day, var.env="temp_avg", var.exp="C-3-row", var.period="night", filename.prefix=NULL, section.code="05-2b")
#

		# results: no consistent differences across blocks (even for peaks) or across time


#################################
# C. Day - Exploratory Analysis #
#################################
# determine which treatment(s) represents min/max for that day in that block
	# visually determine if a specific treatment (e.g., control) was consistently min/max across dates and blocks

### calculate min/max and difference between next highest/lowest value
	## gather variables
	summ.env.day = data.env.day %>% gather(key="var_env", value="value_env", -experiment, -treatment, -block, -week, -day_of_week, -date, -period, -date_rating, -days_since_last)

	## determine min/max (separate object); order values within each group to extract ranked values
	summ.env.day.mm = summ.env.day %>% 
		group_by(experiment, block, week, day_of_week, date, period, date_rating, days_since_last, var_env) %>%
		arrange(desc(value_env) ) %>%
		summarize(value_1=max(value_env, na.rm=TRUE), value_2=value_env[2], value_3=value_env[3], value_4=min(value_env, na.rm=TRUE) ) %>%
		ungroup()
		
		# checks
			# 1 (max) = 2; 4 (min) = 3
#			summ.env.day.mm %>% filter(value_1 == value_2 | value_3 == value_4)
			# 1 = 3; 4 = 2
#			summ.env.day.mm %>% filter(value_1 == value_3 | value_2 == value_4)
			# 1 = 4
#			summ.env.day.mm %>% filter(value_1 == value_4)

	## add comparison flag for min/max ties
		# add empty columns
		summ.env.day.mm = summ.env.day.mm %>% mutate(flag_1eq2=as.integer(NA), flag_1eq3=as.integer(NA), flag_2eq4=as.integer(NA), flag_3eq4=as.integer(NA) )
		
		# fill
		summ.env.day.mm = summ.env.day.mm %>% mutate(
			flag_1eq2=replace(flag_1eq2, value_1 == value_2, 1),
			flag_1eq3=replace(flag_1eq3, value_1 == value_3, 1),
			flag_2eq4=replace(flag_2eq4, value_2 == value_4, 1),
			flag_3eq4=replace(flag_3eq4, value_3 == value_4, 1) )
	
	## calculate difference between max/next highest and 2nd lowest/min
	summ.env.day.mm = summ.env.day.mm %>% mutate(
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
		summ.env.day.mm %>% filter(is.na(diff_max_2) | is.na(diff_3_min) )
		
	## condense flag columns
	summ.env.day.mm = summ.env.day.mm %>% mutate(
		flag_mult_max=case_when(
			(flag_1eq2 == 1 | flag_1eq3 == 1) ~ 1L,
			(is.na(flag_1eq2) & is.na(flag_1eq3) ) ~ 0L),
		flag_mult_min=case_when(
			(flag_2eq4 == 1 | flag_3eq4 == 1) ~ 1L,
			(is.na(flag_2eq4) & is.na(flag_3eq4) ) ~ 0L) )
			
	## remove flag columns
	summ.env.day.mm = summ.env.day.mm %>% select(-flag_1eq2, -flag_1eq3, -flag_2eq4, -flag_3eq4)		

### join and filter
	## join
	summ.env.day = summ.env.day %>% left_join(summ.env.day.mm, by=c(c("experiment","block","week","day_of_week","date","period","date_rating","days_since_last","var_env")) )
	
	## filter
	summ.env.day = summ.env.day %>% filter(value_env == value_1 | value_env == value_4)
	
	## transfer diff value
	summ.env.day = summ.env.day %>% mutate(diff_value=case_when(
		(value_env == value_1) ~ diff_max_2,
		(value_env == value_4) ~ diff_3_min) )
		
		# check
		summ.env.day %>% filter(is.na(diff_value) )

## visualize - over dates 
	# note: commented out because no consistent trends

#	## relative humidity
#	plot.explore(df.in=summ.env.day, var.env="rh_avg", var.exp="A-2-row", var.period="day", y.limit=7.5, interval="day", section.code="05-2c1", width=10, height=6)
#	plot.explore(df.in=summ.env.day, var.env="rh_avg", var.exp="A-2-row", var.period="night", y.limit=7.5, interval="day", section.code="05-2c1", width=10, height=6)
#	
#	plot.explore(df.in=summ.env.day, var.env="rh_avg", var.exp="B-3-row", var.period="day", y.limit=7.5, interval="day", section.code="05-2c1", width=10, height=6)
#	plot.explore(df.in=summ.env.day, var.env="rh_avg", var.exp="B-3-row", var.period="night", y.limit=7.5, interval="day", section.code="05-2c1", width=10, height=6)
#
#	plot.explore(df.in=summ.env.day, var.env="rh_avg", var.exp="C-3-row", var.period="day", y.limit=7.5, interval="day", section.code="05-2c1", width=10, height=6)
#	plot.explore(df.in=summ.env.day, var.env="rh_avg", var.exp="C-3-row", var.period="night", y.limit=7.5, interval="day", section.code="05-2c1", width=10, height=6)
#
#	## temperature
#	plot.explore(df.in=summ.env.day, var.env="temp_avg", var.exp="A-2-row", var.period="day", y.limit=1, interval="day", section.code="05-2c1", width=10, height=6)
#	plot.explore(df.in=summ.env.day, var.env="temp_avg", var.exp="A-2-row", var.period="night", y.limit=1, interval="day", section.code="05-2c1", width=10, height=6)
#	
#	plot.explore(df.in=summ.env.day, var.env="temp_avg", var.exp="B-3-row", var.period="day", y.limit=1, interval="day", section.code="05-2c1", width=10, height=6)
#	plot.explore(df.in=summ.env.day, var.env="temp_avg", var.exp="B-3-row", var.period="night", y.limit=1, interval="day", section.code="05-2c1", width=10, height=6)
#
#	plot.explore(df.in=summ.env.day, var.env="temp_avg", var.exp="C-3-row", var.period="day", y.limit=1, interval="day", section.code="05-2c1", width=10, height=6)
#	plot.explore(df.in=summ.env.day, var.env="temp_avg", var.exp="C-3-row", var.period="night", y.limit=1, interval="day", section.code="05-2c1", width=10, height=6)
#

		# results: no consistent trends across blocks or over time

### visualize - range (treatment max-min) violin - by block 
	# although there are no apparent trends in raw data, if there are statistical differences, is the magnitude large enough to be relevant?
	# note: commented out because superseded by d3 - blocks combined captures trend

#	## relative humidity
#	plot.c2.r = summ.env.day.mm %>% filter(var_env == "rh_avg") %>% {
#	ggplot(., aes(x=as.character(block), y=diff_min_max) ) +
#		geom_violin(scale="count", draw_quantiles=c(0.25, 0.5, 0.75), size=0.2) +
#		facet_grid(cols=vars(period, experiment) ) +
#		scale_y_continuous(breaks=seq(0,16,by=4), minor_breaks=seq(0,16,by=2) ) +
#		coord_cartesian(ylim=c(0,16) ) +
#		theme_bw() +
#		theme(axis.title.x=element_blank(), axis.text.x=element_blank() ) +
#		theme(axis.title.y=element_text(size=9), axis.text.y=element_text(size=7) ) +
#		labs(y="Relative humidity (% point)\nrange among treatments")
#	}
#
#	## temperature
#	plot.c2.t = summ.env.day.mm %>% filter(var_env == "temp_avg") %>% {
#	ggplot(., aes(x=as.character(block), y=diff_min_max) ) +
#		geom_violin(scale="count", draw_quantiles=c(0.25, 0.5, 0.75), size=0.2) +
#		facet_grid(cols=vars(period, experiment) ) +
#		scale_y_continuous(breaks=seq(0,4,by=1), minor_breaks=seq(0,4,by=0.25) ) +
#		coord_cartesian(ylim=c(0,4) ) +
#		theme_bw() +
#		theme(axis.title.x=element_blank(), axis.text.x=element_blank() ) +
#		theme(axis.title.y=element_text(size=9), axis.text.y=element_text(size=7) ) +
#		theme(strip.text=element_blank(), strip.background=element_blank() ) +
#		labs(y="Temperature (C)\nrange among treatments")
#	}
#	
#	plot.c2 = wrap_plots(plot.c2.r, plot.c2.t, ncol=1)
#	
#	ggplot2::ggsave(file="./4_results/05-2c2_env_day_range_block.png", device="png", plot=plot.c2, width=9, height=5.5, units="in", dpi=600)	


### visualize - range (treatment max-min) violin - blocks combined
	# although there are no apparent trends in raw data, if there are statistical differences, is the magnitude large enough to be relevant?
	# note: commented out, plots unlikely to be useful given that statistical differences have small magnitudes

#	## relative humidity
#	plot.c3.r = summ.env.day.mm %>% filter(var_env == "rh_avg") %>% {
#	ggplot(., aes(x=experiment, y=diff_min_max) ) +
#		geom_violin(scale="count", draw_quantiles=c(0.25, 0.5, 0.75), size=0.2) +
#		facet_grid(cols=vars(period) ) +
#		scale_y_continuous(breaks=seq(0,16,by=4), minor_breaks=seq(0,16,by=2) ) +
#		coord_cartesian(ylim=c(0,16) ) +
#		theme_bw() +
#		theme(axis.title.x=element_blank(), axis.text.x=element_blank() ) +
#		theme(axis.title.y=element_text(size=9), axis.text.y=element_text(size=7) ) +
#		labs(y="Relative humidity (% point)\nrange among treatments")
#	}
#
#	## temperature
#	plot.c3.t = summ.env.day.mm %>% filter(var_env == "temp_avg") %>% {
#	ggplot(., aes(x=experiment, y=diff_min_max) ) +
#		geom_violin(scale="count", draw_quantiles=c(0.25, 0.5, 0.75), size=0.2) +
#		facet_grid(cols=vars(period) ) +
#		scale_y_continuous(breaks=seq(0,4,by=1), minor_breaks=seq(0,4,by=0.25) ) +
#		coord_cartesian(ylim=c(0,4) ) +
#		theme_bw() +
#		theme(axis.title.x=element_blank(), axis.text.x=element_blank() ) +
#		theme(axis.title.y=element_text(size=9), axis.text.y=element_text(size=7) ) +
#		theme(strip.text=element_blank(), strip.background=element_blank() ) +
#		labs(y="Temperature (C)\nrange among treatments")
#	}
#	
#	plot.c3 = wrap_plots(plot.c3.r, plot.c3.t, ncol=1)
#	
#	ggplot2::ggsave(file="./4_results/z_05-2c3_env_day_range_block_not-shown.png", device="png", plot=plot.c3, width=6.5, height=6.5, units="in", dpi=600)	


###############################
# D. Day - Visualize Analysis #
###############################

### summarize
	## treatment
		# calculate average
		summ.env.day.1 = data.env.day %>% 
			group_by(experiment, treatment) %>%
			summarize(temp_avg=mean(temp_avg, na.rm=T), rh_avg=mean(rh_avg, na.rm=T), dpt_avg=mean(dpt_avg, na.rm=T), dpt_depress=mean(dpt_depress, na.rm=T) ) %>%
			ungroup()

		# round
		summ.env.day.1 = summ.env.day.1 %>% mutate(
			temp_avg	=round(temp_avg, digits=2),
			rh_avg		=round(rh_avg, 	 digits=1),
			dpt_avg		=round(dpt_avg	  , digits=2),
			dpt_depress =round(dpt_depress, digits=2) )

	## treatment x period
		# calculate average
		summ.env.day.2 = data.env.day %>% 
			group_by(experiment, treatment, period) %>%
			summarize(temp_avg=mean(temp_avg, na.rm=T), rh_avg=mean(rh_avg, na.rm=T), dpt_avg=mean(dpt_avg, na.rm=T), dpt_depress=mean(dpt_depress, na.rm=T) ) %>%
			ungroup()

		# round
		summ.env.day.2 = summ.env.day.2 %>% mutate(
			temp_avg	=round(temp_avg, digits=2),
			rh_avg		=round(rh_avg, 	 digits=1),
			dpt_avg		=round(dpt_avg	  , digits=2),
			dpt_depress =round(dpt_depress, digits=2) )

### make annotation tibbles
	## relative humidity
		# treatment
			# mean letters
			rh1.let = tibble(
				experiment = c("B-3-row","B-3-row","B-3-row","B-3-row"),
				treatment  = c("blade","control","manual","twine"),
				letter_rh  = c("a","b","b","b") )

		# treatment x period
			# mean letters
			rh2.let = tibble(
				experiment = c("A-2-row","A-2-row","A-2-row","A-2-row","A-2-row","A-2-row","A-2-row","A-2-row","C-3-row","C-3-row","C-3-row","C-3-row"),
				period	   = c("day","day","day","day",				   "night","night","night","night",		   "night","night","night","night"),
				treatment  = c("blade","control","manual","twine",	   "blade","control","manual","twine",	   "blade","control","manual","twine"),
				letter_rh  = c("c","a","b","b",						   "ab","c","b","a",					   "a","b","a","b") )

			# fixed effects
			rh2.other = tibble(
				experiment = c("C-3-row"),
				period	   = c("day"),
				text_lab   = c("slice 0.3665"),
				y_position = c(99) )

	## temperature
		# mean letters
		temp.let = tibble(
			experiment= c("A-2-row","A-2-row","A-2-row","A-2-row","A-2-row","A-2-row","A-2-row","A-2-row","B-3-row","B-3-row","B-3-row","B-3-row","B-3-row","B-3-row","B-3-row","B-3-row","C-3-row","C-3-row","C-3-row","C-3-row","C-3-row","C-3-row","C-3-row","C-3-row"),
			period 	  = c("day","day","day","day",				  "night","night","night","night",		  "day","day","day","day",				  "night","night","night","night",		  "day","day","day","day",				  "night","night","night","night"),
			treatment = c("blade","control","manual","twine",	  "blade","control","manual","twine",	  "blade","control","manual","twine",	  "blade","control","manual","twine",	  "blade","control","manual","twine",	  "blade","control","manual","twine"),
			letter_temp=c("c","d","a","b",						  "b","a","b","b",						  "c","c","a","b",						  "b","a","b","b",						  "a","b","a","b",						  "b","a","b","a") )

### join annotation to summary
	## treatment
		# join
		summ.env.day.1 = summ.env.day.1 %>% left_join(rh1.let, by=c(c("experiment","treatment")) )
		
		# combine mean and letter
		summ.env.day.1 = summ.env.day.1 %>% mutate(label_rh=paste(sprintf("%0.1f", rh_avg), " ", letter_rh, sep="") )

	## treatment x period
		# join
		summ.env.day.2 = summ.env.day.2 %>% left_join(rh2.let, by=c(c("experiment","treatment","period")) )
		summ.env.day.2 = summ.env.day.2 %>% left_join(temp.let, by=c(c("experiment","treatment","period")) )

		# combine mean and letter
		summ.env.day.2 = summ.env.day.2 %>% mutate(
			label_rh=case_when(
				(!is.na(letter_rh) ) ~ paste(sprintf("%0.1f", rh_avg), " ", letter_rh, sep=""),
				(is.na(letter_rh) ) ~ sprintf("%0.1f", rh_avg) ),
			label_temp=paste(sprintf("%0.1f", temp_avg), " ", letter_temp, sep="") )	

### names and orders
	## change order of treatments
	data.env.day = data.env.day %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))
	summ.env.day.1 = summ.env.day.1 %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))
	summ.env.day.2 = summ.env.day.2 %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))

	## rename strips
	strip.exp = c("A-2-row"="Ranch 1 2-row", "B-3-row"="Ranch 1 3-row", "C-3-row"="Ranch 2 3-row")
	strip.period = c("day"="Daylight", "night"="Night")


### summarize number of data points for figure caption
	data.env.day %>% group_by(experiment) %>% summarize(date_first=min(date), date_last=max(date) ) %>% mutate(date_last - date_first)
	
### relative humidity
	## treatment
	plot.d.r1 = data.env.day %>% filter(experiment == "B-3-row") %>% {
	ggplot(.) +
		geom_violin(aes(x=treatment, y=rh_avg), scale="count", draw_quantiles=c(0.25, 0.5, 0.75), size=0.2) +
		geom_crossbar(data={summ.env.day.1 %>% filter(experiment == "B-3-row")}, aes(x=treatment, y=rh_avg, ymin=rh_avg, ymax=rh_avg), width=0.25, fatten=1.75, color="red") +
		geom_text(data={summ.env.day.1 %>% filter(experiment == "B-3-row")}, aes(x=treatment, y=rh_avg, label=label_rh ), size=3.5, fontface="bold", color="red", vjust=1.4) +
		facet_grid(cols=vars(experiment), labeller=labeller(experiment=strip.exp) ) +
		scale_x_discrete(expand=expansion(mult=c(0.2,0.2)) ) +
		scale_y_continuous(limits=c(22,99) ) +
		theme_bw() +
		theme(axis.title.x=element_text(size=13, margin=margin(t=8.25)), axis.title.y=element_text(size=13), axis.text=element_text(size=11) ) +
		theme(strip.text=element_text(size=12) ) +
		theme(legend.title=element_text(size=12), legend.text=element_text(size=11) ) +
		labs(x="Treatment", y="Relative humidity (%)")
	}
	ggplot2::ggsave(file="./4_results/05-2d_env_day_stat-analysis_rh_treatment-B.png", device="png", plot=plot.d.r1, width=3.25, height=3.5, units="in", dpi=600)	

	## treatment x period (figure height intended to match size of plot.d.r1 panel area)
	plot.d.r2 = data.env.day %>% filter(experiment %in% c("A-2-row","C-3-row") ) %>% {
	ggplot(.) +
		geom_violin(aes(x=treatment, y=rh_avg), scale="count", draw_quantiles=c(0.25, 0.5, 0.75), size=0.2) +
		geom_crossbar(data={summ.env.day.2 %>% filter(experiment %in% c("A-2-row","C-3-row") )}, aes(x=treatment, y=rh_avg, ymin=rh_avg, ymax=rh_avg), width=0.25, fatten=1.75, color="red") +
		geom_text(data={summ.env.day.2 %>% filter(experiment %in% c("A-2-row","C-3-row") )}, aes(x=treatment, y=rh_avg, label=label_rh ), size=3.5, fontface="bold", color="red", vjust=1.4) +
		geom_text(data=rh2.other, aes(y=y_position, label=text_lab), x=2.5, size=4) +
		facet_grid(rows=vars(period), cols=vars(experiment), labeller=labeller(experiment=strip.exp, period=strip.period) ) +
		scale_x_discrete(expand=expansion(mult=c(0.2,0.2)) ) +
		scale_y_continuous(limits=c(22,99) ) +
		theme_bw() +
		theme(axis.title.x=element_text(size=13, margin=margin(t=8.25)), axis.title.y=element_text(size=13), axis.text=element_text(size=11) ) +
		theme(strip.text=element_text(size=12) ) +
		theme(legend.title=element_text(size=12), legend.text=element_text(size=11) ) +
		labs(x="Treatment", y="Relative humidity (%)")
	}	
	ggplot2::ggsave(file="./4_results/05-2d_env_day_stat-analysis_rh_trt-period-AC.png", device="png", plot=plot.d.r2, width=7, height=6.25, units="in", dpi=600)	

### temperature
	## treatment x period
	plot.d.t = ggplot(data.env.day) +
		geom_violin(aes(x=treatment, y=temp_avg), scale="count", draw_quantiles=c(0.25, 0.5, 0.75), size=0.2) +
		geom_crossbar(data=summ.env.day.2, aes(x=treatment, y=temp_avg, ymin=temp_avg, ymax=temp_avg), width=0.25, fatten=1.75, color="red") +
		geom_text(data=summ.env.day.2, aes(x=treatment, y=temp_avg, label=label_temp ), size=3.5, fontface="bold", color="red", vjust=-0.5) +
		facet_grid(rows=vars(period), cols=vars(experiment), labeller=labeller(experiment=strip.exp, period=strip.period) ) +
		scale_x_discrete(expand=expansion(mult=c(0.18,0.18)) ) +
		scale_y_continuous(breaks=c(5,10,15,20,25) ) +
		theme_bw() +
		theme(axis.title.x=element_text(size=13, margin=margin(t=8.25)), axis.title.y=element_text(size=13), axis.text=element_text(size=10) ) +
		theme(strip.text=element_text(size=12) ) +
		theme(legend.title=element_text(size=12), legend.text=element_text(size=11) ) +
		labs(x="Treatment", y="Temperature (C)")
	ggplot2::ggsave(file="./4_results/05-2d_env_day_stat-analysis_temp_trt-period.png", device="png", plot=plot.d.t, width=7, height=6.25, units="in", dpi=600)	


