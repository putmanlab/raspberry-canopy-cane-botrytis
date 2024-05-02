#####################################
# RASPBERRY Cane Botrytis 			#
# Disease and Environmental Data	#
# Exploratory Analysis				#
# Epidemiology Summary Statistics	#
# Rating date						#
#####################################

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

### environment
	## import hourly data
	data.env.hr = read_csv(file="./2_data_curated/rasp-caneb_04e_environ_final-hour.csv", col_names=T, na=".")
	
	## express time in/convert timezone to UTC-8; datetime was converted to UTC on export
	data.env.hr = data.env.hr %>% mutate(datetime=with_tz(datetime, tzone="Etc/GMT+8") )
	

##############################
# B. Rating Date - Calculate #
##############################

### run summ calc function
	summ.env.rat.l = epi.summ(
		df.in=data.env.hr, in.interval="hour", extreme.method="average", extreme.period="date", out.interval="date_rating", 
		cols.grp=c("experiment","treatment","block"), data.source="environment", lw.1.onset=1.8, lw.1.offset=2.2, lw.2.onset=2.0, lw.2.offset=3.8)

### shorten var names
	summ.env.rat.l = summ.env.rat.l %>% mutate(var_env=str_replace(var_env, "_all", "") )

### adjust totals (hours) by number of days
	# days_since_last = 28,29 for A and B; and 22,21 for C
	
	# change days_since_last to 15 for C rep 1 2018-11-07 (because data missing after 2018-11-01)
	summ.env.rat.l = summ.env.rat.l %>% mutate(days_since_last=replace(days_since_last, block == 1 & date_rating == as_date("2018-11-07"), 15) )
	
		# check
		summ.env.rat.l %>% distinct(experiment, block, date_rating, days_since_last) %>% arrange(date_rating, block) %>% print(n=Inf) 

	# set vector of time-based variables
	time.vars = c("lwe_dpd18","lwe_dpd20","lwe_rh75","lwe_rh80","lwe_rh85","lwe_rh90","temp_time10","temp_time15","temp_time1525","temprh_rh80","temprh_rh90")

	# adjust
	summ.env.rat.l = summ.env.rat.l %>%	mutate(value_env_final=case_when(
		(var_env %in% time.vars ) ~ round( value_env / days_since_last, digits=1),
		(var_env %in% c("rh_avg_maxday","temp_avg_minday") ) ~ value_env) )
		
		# check NAs
		summ.env.rat.l %>% filter(is.na(value_env_final) & !is.na(value_env) )
	
### summarize
	## per rating date
		# calculate
		summ.env.rat.avg = summ.env.rat.l %>% 
			group_by(experiment, treatment, date_rating, var_env) %>%
			summarize(value_env_final=mean(value_env_final, na.rm=T) ) %>%
			ungroup()
		
		# round
		summ.env.rat.avg = summ.env.rat.avg %>% mutate(value_env_final=round(value_env_final, digits=1) )

	## total
		# calculate
		summ.env.avg = summ.env.rat.l %>% 
			group_by(experiment, treatment, var_env) %>%
			summarize(value_env_final=mean(value_env_final, na.rm=T) ) %>%
			ungroup()
		
		# round
		summ.env.avg = summ.env.avg %>% mutate(value_env_final=round(value_env_final, digits=1) )
		
### convert for statistical analysis
	## remove non-time based
	summ.env.rat.w = summ.env.rat.l %>% filter(!(var_env %in% c("rh_avg_maxday","temp_avg_minday")) )

	## convert to proportions
	summ.env.rat.w = summ.env.rat.w %>% mutate(value_env_final_prop=round( value_env_final / 24, digits=3) )

	## remove unneeded column
	summ.env.rat.w = summ.env.rat.w %>% select(-value_env, -value_env_final)
		
	## convert to wide
	summ.env.rat.w = summ.env.rat.w %>% spread(key=var_env, value=value_env_final_prop)
		
### export
	write_csv(summ.env.rat.l, file="./2_data_curated/rasp-caneb_06-3b_environ_summ-epi_rating.csv", na="", col_names=T, append=F)
	write_csv(summ.env.rat.w, file="./2_data_curated/rasp-caneb_06-3b_environ_summ-epi_rating_SAS.csv", na=".", col_names=T, append=F)


##################################
# C. Rating Date - Visualize Raw #
##################################
# data loggers within plots

### prepare
	## names and orders
		## change order of treatments
		summ.env.rat.l = summ.env.rat.l %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))
		summ.env.avg = summ.env.avg %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))

		## rename strips
		strip.exp = c("A-2-row"="Ranch 1 2-row", "B-3-row"="Ranch 1 3-row", "C-3-row"="Ranch 2 3-row")
		strip.var = c(
			"lwe_dpd18"="eSW dpd18", "lwe_dpd20"="eSW dpd20",
			"lwe_rh75"="eSW rh>75", "lwe_rh80"="eSW rh>80", "lwe_rh85"="eSW rh>85", "lwe_rh90"="eSW rh>90",
			"temp_time10"="temp>10", "temp_time15"="temp>15", "temp_time1525"="15<temp<25",
			"temprh_rh80"="t1525 rh>80","temprh_rh90"="t1525 rh>90",
			"rh_avg_maxday"="Daily maximum rh", "temp_avg_minday"="Daily minimum temp")

	## graph settings
	source("./3_analysis/rcb_settings.r")	

#### by rating date - variable x experiment (if one graph, facet_grid will produce blank panels)
	# note: commented out, superseded by by treatment graphs below
#	## A-2-row, B-3-row
#	plot.c1.ab = summ.env.rat.l %>% filter(experiment %in% c("A-2-row","B-3-row") ) %>% {
#	ggplot(., aes(x=as.character(date_rating), y=value_env_per_day, linetype=treatment, color=treatment) ) +
#		geom_line(aes(group=interaction(block, treatment, experiment) ), size=0.25, alpha=0.5) +
#		geom_line(data={summ.env.rat.avg %>% filter(experiment %in% c("A-2-row","B-3-row") ) }, aes(group=interaction(treatment, experiment) ), size=1) +
#		facet_grid(cols=vars(var_env), rows=vars(experiment) ) +
#		theme_bw() +
#		theme(axis.title.x=element_blank(), axis.text.x=element_blank() ) +
#		guides(linetype=guide_legend(direction="horizontal"), color=guide_legend(direction="horizontal") )
#	}
#
#	## C-3-row
#	plot.c1.c = summ.env.rat.l %>% filter(experiment == "C-3-row") %>% {
#	ggplot(., aes(x=as.character(date_rating), y=value_env_per_day, linetype=treatment, color=treatment) ) +
#		geom_line(aes(group=interaction(block, treatment, experiment) ), size=0.25, alpha=0.5) +
#		geom_line(data={summ.env.rat.avg %>% filter(experiment == "C-3-row") }, aes(group=interaction(treatment, experiment) ), size=1) +
#		facet_grid(cols=vars(var_env), rows=vars(experiment) ) +
#		theme_bw() +
#		theme(strip.text.x=element_blank(), strip.background.x=element_blank() ) +
#		guides(linetype=guide_legend(direction="horizontal"), color=guide_legend(direction="horizontal") )
#	}
#	
#	plot.c1 = wrap_plots(plot.c1.ab, plot.c1.c, ncol=1, heights=c(2,1.2), guides="collect") +
#		plot_annotation(theme=theme(legend.position="bottom"))
#	
#	ggplot2::ggsave(file="./4_results/z_06-3c1_env_epi-summ_rating_treatment_not-shown.png", device="png", plot=plot.c1, width=16, height=9, units="in", dpi=600)

		# results: possible differences among relative humidity-based measures for C-3-row only
		# do statistical analysis for lwe_rh90

### by treatment - variable x experiment
	## time-based - horizontal
	plot.c2 = summ.env.rat.l %>% filter(var_env %in% time.vars ) %>% {
	ggplot(.) +
		geom_point(aes(x=value_env_final, y=fct_rev(treatment), shape=as.character(date_rating), color=as.character(date_rating) ), size=2.5, stroke=0.3) +
		geom_crossbar(data={summ.env.avg %>% filter(var_env %in% time.vars) }, aes(x=value_env_final, y=fct_rev(treatment), xmin=value_env_final, xmax=value_env_final), width=0.6, fatten=1, color="black") +
		geom_text(data={summ.env.avg %>% filter(var_env %in% time.vars) }, aes(y=fct_rev(treatment), label=sprintf("%04.1f", value_env_final) ), x=-2.5, size=2.5, color="black") +
		facet_grid(rows=vars(var_env), cols=vars(experiment), labeller=labeller(experiment=strip.exp, var_env=strip.var) ) +
		scale_x_continuous(breaks=c(0,4,8,12,16,20,24), expand=expansion(mult=c(0.2,0.05)) ) +
		scale_y_discrete(expand=expansion(mult=c(0.15,0.15)) ) +
		scale_shape_manual(values=c(vec.rat.shape,vec.rat.shape) ) +
		scale_color_manual(values=c(vec.rat.color,vec.rat.color) ) +
		theme_bw() +
		theme(axis.title.x=element_text(size=10, margin=margin(t=5.5)), axis.title.y=element_text(size=10), axis.text=element_text(size=7.5) ) +
		theme(strip.text.x=element_text(size=7.5, margin=margin(t=2.25, b=2.25)), strip.text.y=element_text(size=7.5, margin=margin(l=2.25, r=2.25)) ) +
		theme(legend.position="bottom", legend.margin=margin(t=-5.5), legend.title=element_text(size=10), legend.text=element_text(size=7.5, margin=margin(l=-8.25)) ) +
		guides(shape=guide_legend(nrow=1, byrow=TRUE), color=guide_legend(nrow=1, byrow=TRUE) ) +
		labs(x="Hours meeting threshold(s) per day", y="Treatment", shape="Rating date", color="Rating date")
	}
	ggplot2::ggsave(file="./4_results/06-3c2_env-epi_rating_explore_hours-day.png", device="png", plot=plot.c2, width=7, height=8.85, units="in", dpi=600)

	## direct average - horizontal
	plot.c3a = summ.env.rat.l %>% filter(var_env == "rh_avg_maxday") %>% {
	ggplot(.) +
		geom_point(aes(x=value_env_final, y=fct_rev(treatment), shape=as.character(date_rating), color=as.character(date_rating) ), size=2.5, stroke=0.3) +
		geom_crossbar(data={summ.env.avg %>% filter(var_env == "rh_avg_maxday") }, aes(x=value_env_final, y=fct_rev(treatment), xmin=value_env_final, xmax=value_env_final), width=0.6, fatten=1, color="black") +
		geom_text(data={summ.env.avg %>% filter(var_env == "rh_avg_maxday") }, aes(y=fct_rev(treatment), label=sprintf("%04.1f", value_env_final) ), x=80.75, size=2.5, color="black") +
		facet_grid(rows=vars(experiment), cols=vars(var_env), labeller=labeller(experiment=strip.exp, var_env=strip.var) ) +
		scale_x_continuous(expand=expansion(mult=c(0.2,0.05)) ) +
		scale_y_discrete(expand=expansion(mult=c(0.15,0.15) ) ) +
		scale_shape_manual(values=c(vec.rat.shape,vec.rat.shape) ) +
		scale_color_manual(values=c(vec.rat.color,vec.rat.color) ) +
		theme_bw() +
		theme(axis.title.x=element_text(size=10, margin=margin(t=5.5)), axis.title.y=element_text(size=10), axis.text=element_text(size=7.5) ) +
		theme(strip.text.x=element_text(size=7.5, margin=margin(t=2.25, b=2.25)), strip.text.y=element_blank() ) +
		theme(legend.position="bottom", legend.margin=margin(t=-5.5), legend.title=element_text(size=10), legend.text=element_text(size=7.5, margin=margin(l=-8.25)) ) +
		guides(shape=guide_legend(nrow=1, byrow=TRUE), color=guide_legend(nrow=1, byrow=TRUE) ) +
		labs(x="Relative humidity (%)", y="Treatment", shape="Rating date", color="Rating date")
	}

	plot.c3b = summ.env.rat.l %>% filter(var_env == "temp_avg_minday") %>% {
	ggplot(.) +
		geom_point(aes(x=value_env_final, y=fct_rev(treatment), shape=as.character(date_rating), color=as.character(date_rating) ), size=2.5, stroke=0.3) +
		geom_crossbar(data={summ.env.avg %>% filter(var_env == "temp_avg_minday") }, aes(x=value_env_final, y=fct_rev(treatment), xmin=value_env_final, xmax=value_env_final), width=0.6, fatten=1, color="black") +
		geom_text(data={summ.env.avg %>% filter(var_env == "temp_avg_minday") }, aes(y=fct_rev(treatment), label=sprintf("%04.1f", value_env_final) ), x=4.25, size=2.5, color="black") +
		facet_grid(rows=vars(experiment), cols=vars(var_env), labeller=labeller(experiment=strip.exp, var_env=strip.var) ) +
		scale_x_continuous(expand=expansion(mult=c(0.2,0.05)) ) +
		scale_y_discrete(expand=expansion(mult=c(0.15,0.15)) ) +
		scale_shape_manual(values=c(vec.rat.shape,vec.rat.shape) ) +
		scale_color_manual(values=c(vec.rat.color,vec.rat.color) ) +
		theme_bw() +
		theme(axis.title.x=element_text(size=10, margin=margin(t=5.5)), axis.text.x=element_text(size=7.5), axis.title.y=element_blank(), axis.text.y=element_blank() ) +
		theme(strip.text.x=element_text(size=7.5, margin=margin(t=2.25, b=2.25)), strip.text.y=element_text(size=6.5, margin=margin(l=2.25, r=2.25)) ) +
		theme(legend.position="bottom", legend.margin=margin(t=-5.5), legend.title=element_text(size=10), legend.text=element_text(size=7.5, margin=margin(l=-8.25)) ) +
		guides(shape=guide_legend(nrow=1, byrow=TRUE), color=guide_legend(nrow=1, byrow=TRUE) ) +
		labs(x="Temperature (C)", y="Treatment", shape="Rating date", color="Rating date")
	}
	
	plot.c3 = wrap_plots(plot.c3a, plot.c3b, nrow=1, guides="collect") +
		plot_annotation(theme=theme(legend.position="bottom") )	

	ggplot2::ggsave(file="./4_results/06-3c3_env-epi_rating_explore_min-max.png", device="png", plot=plot.c3, width=7, height=3.15, units="in", dpi=600)
	
		# results: possible differences for lwe_dpd18, lwe_dpd20, lwe_rh85, lwe_rh90, temp_time1525, especially for C-3-row
			# pattern for rh-based summ stats is basically the same
		# do statistical analysis for lwe_rh90, temp_time1525
		
		
#######################################
# D. Rating Date - Visualize Analysis #
#######################################

### make annotation tibbles
	# mean letters
	d.let = tibble(
		experiment= c("C-3-row","C-3-row","C-3-row","C-3-row",	  "C-3-row","C-3-row","C-3-row","C-3-row"),
		treatment = c("blade","control","manual","twine",		  "blade","control","manual","twine"),
		var_env	  = c("lwe_rh90","lwe_rh90","lwe_rh90","lwe_rh90","temp_time1525","temp_time1525","temp_time1525","temp_time1525"),
		letter	  = c("ab","bc","a","c",						  "b","ab","ab","a"),
		y_position= c(9,9,9,9, 									  20,20,20,20) )
			
### join annotation to summary
	## join
	summ.env.avg.ant = summ.env.avg %>% left_join(d.let, by=c(c("experiment","treatment","var_env")) )
	
	## combine mean and letter
	summ.env.avg.ant = summ.env.avg.ant %>% mutate(label_avglet=paste(sprintf("%0.1f", value_env_final), " ", letter, sep="") )

### names and orders
	## change order of treatments
	summ.env.avg.ant = summ.env.avg.ant %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))
	d.let = d.let %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))
	
### lwe_rh90 - C-3-row
	plot.d1 = summ.env.rat.l %>% filter(experiment == "C-3-row" & var_env == "lwe_rh90") %>% {
	ggplot(.) +
		geom_point(aes(x=treatment, y=value_env_final, shape=as.character(date_rating), color=as.character(date_rating) ), size=4, stroke=0.75 ) +
		geom_crossbar(data={summ.env.avg %>% filter(experiment == "C-3-row" & var_env == "lwe_rh90") }, aes(x=treatment, y=value_env_final, ymin=value_env_final, ymax=value_env_final), width=0.3, fatten=1.5, color="black") +
		geom_text(data={summ.env.avg.ant %>% filter(experiment == "C-3-row" & var_env == "lwe_rh90") }, aes(x=treatment, y=value_env_final+5, label=label_avglet ), size=3.5, color="black") +
		facet_grid(cols=vars(experiment), labeller=labeller(experiment=strip.exp) ) +
		scale_y_continuous(limits=c(0,24), breaks=c(0,4,8,12,16,20,24), expand=expansion(mult=c(0.05,0.05)) ) +
		scale_shape_manual(values=vec.rat.shape) +
		scale_color_manual(values=vec.rat.color) +
		theme_bw() +
		theme(axis.title.x=element_text(size=12, margin=margin(t=5.5)), axis.text.x=element_text(size=10), axis.title.y=element_text(size=12), axis.text.y=element_text(size=10) ) +
		theme(strip.text=element_text(size=11) ) +
		theme(legend.position="bottom", legend.margin=margin(t=-5.5), legend.title=element_text(size=11), legend.text=element_text(size=10, margin=margin(l=-5.5, r=2.75)) ) +
		labs(x="Treatment", y="Hours/day with relative humidity >90%", shape="Rating date", color="Rating date")
	}
	
	plot.d2 = summ.env.rat.l %>% filter(experiment == "C-3-row" & var_env == "temp_time1525") %>% {
	ggplot(.) +
		geom_point(aes(x=treatment, y=value_env_final, shape=as.character(date_rating), color=as.character(date_rating) ), size=4, stroke=0.75 ) +
		geom_crossbar(data={summ.env.avg %>% filter(experiment == "C-3-row" & var_env == "temp_time1525") }, aes(x=treatment, y=value_env_final, ymin=value_env_final, ymax=value_env_final), width=0.3, fatten=1.5, color="black") +
		geom_text(data={summ.env.avg.ant %>% filter(experiment == "C-3-row" & var_env == "temp_time1525") }, aes(x=treatment, y=value_env_final-6, label=label_avglet ), size=3.5, color="black") +
		facet_grid(cols=vars(experiment), labeller=labeller(experiment=strip.exp) ) +
		scale_y_continuous(limits=c(0,24), breaks=c(0,4,8,12,16,20,24), expand=expansion(mult=c(0.05,0.05)) ) +
		scale_shape_manual(values=vec.rat.shape) +
		scale_color_manual(values=vec.rat.color) +
		theme_bw() +
		theme(axis.title.x=element_text(size=12, margin=margin(t=5.5)), axis.text.x=element_text(size=10), axis.title.y=element_text(size=12), axis.text.y=element_text(size=10) ) +
		theme(strip.text=element_text(size=11) ) +
		theme(legend.position="bottom", legend.margin=margin(t=-5.5), legend.title=element_text(size=11), legend.text=element_text(size=10, margin=margin(l=-5.5, r=2.75)) ) +
		labs(x="Treatment", y="Hours/day with 15C < temperature < 25C", shape="Rating date", color="Rating date")
	}
	
	plot.d = wrap_plots(plot.d1, plot.d2, nrow=1, guides="collect") +
		plot_annotation(theme=theme(legend.position="bottom"))
	
	ggplot2::ggsave(file="./4_results/06-3d_env-epi_rating_analysis.png", device="png", plot=plot.d, width=7, height=4.5, units="in", dpi=600)	

