#####################################
# RASPBERRY Cane Botrytis 			#
# Environmental Data				#
# Compare Ranches					#
#####################################
# examine differences between B-3-row and C-3-row that may be due to time of year

## built on Docker putmanlab/exploratory-analysis:420.0

library(conflicted)

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)

library(patchwork) # for combining plots: wrap_plots

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/raspb_botrytis")

source("./3_analysis/rcb_functions.r")


#########################
# A. Import and Prepare #
#########################

### upload
	## environment
	data.env.day = read_csv(file="./2_data_curated/rasp-caneb_04f_environ_final-day.csv", col_names=T, na=".")

	## epidemiological summary statistics
	summ.env.day.l = read_csv(file="./2_data_curated/rasp-caneb_06-2b_environ_summ-epi_day.csv", col_names=T, na="")

### prepare environmental df
	## remove unneeded columns
	data.env.day.t = data.env.day %>% select(-week, -day_of_week, -dpt_avg, -dpt_depress)
	
	## gather
	data.env.day.t = data.env.day.t %>% gather(key="var_x", value="value_env", -experiment, -treatment, -block, -date, -period, -date_rating, -days_since_last)
	
	## make new variable
	data.env.day.t = data.env.day.t %>% mutate(var_env=case_when(
		(var_x == "rh_avg" & period == "day") ~ "rh_day",
		(var_x == "rh_avg" & period == "night") ~ "rh_night",
		(var_x == "temp_avg" & period == "day") ~ "temp_day",
		(var_x == "temp_avg" & period == "night") ~ "temp_night",
		(var_x == "vpd_avg" & period == "day") ~ "vpd_day",
		(var_x == "vpd_avg" & period == "night") ~ "vpd_night") )
		
		# check
		data.env.day.t %>% distinct(var_x, period, var_env)
		
	## remove temporary/old columns
	data.env.day.t = data.env.day.t %>% select(-period, -var_x)
	
### bind
	data.ee.day = bind_rows(data.env.day.t, summ.env.day.l)	

### determine days after treatment
	## calculate
	data.ee.day = data.ee.day %>% mutate(days_after_trt=case_when(
		(experiment %in% c("A-2-row","B-3-row") ) ~ date - as_date("2018-01-05"),
		(experiment == "C-3-row") ~ date - as_date("2018-09-25") ) )
	
	## convert
	data.ee.day = data.ee.day %>% mutate(days_after_trt=as.integer(days_after_trt) )

### reorder columns
	data.ee.day = data.ee.day %>% select(experiment, treatment, block, date, days_after_trt, date_rating, days_since_last, var_env, value_env)
	
### sort
	data.ee.day = data.ee.day %>% arrange(experiment, treatment, block, date, var_env)

### export
	write_csv(data.ee.day, file="./2_data_curated/rasp-caneb_10a_environ-epidem_day.csv", na="", append=F, col_names=T)


################
# B. Visualize #
################

#### all vars - all experiments (note: commented out because superseded by selected variables below)
#	plot.b1 = ggplot(data.ee.day, aes(x=days_after_trt, y=value_env, color=experiment, fill=experiment, linetype=experiment) ) +
#		geom_smooth(method="loess", se=T) +
#		facet_wrap(vars(var_env), ncol=2, scales="free_y") +
#		theme_bw() +
#		theme(legend.position="bottom")
#	ggplot2::ggsave(file="./4_results/z_10b1_env-epi_summ_experiment_exp-ABC_not-shown.png", device="png", plot=plot.b1, width=16, height=9, units="in", dpi=600)
#
#### all vars - B/C only (note: commented out because superseded by selected variables below)
#	plot.b2 = data.ee.day %>% filter(experiment %in% c("B-3-row","C-3-row") ) %>% {
#	ggplot(., aes(x=days_after_trt, y=value_env, color=experiment, fill=experiment, linetype=experiment) ) +
#		geom_smooth(method="loess", se=T) +
#		facet_wrap(vars(var_env), ncol=2, scales="free_y") +
#		theme_bw() +
#		theme(legend.position="bottom")
#	}
#	ggplot2::ggsave(file="./4_results/z_10b2_env-epi_summ_experiment_exp-BC_not-shown.png", device="png", plot=plot.b2, width=16, height=9, units="in", dpi=600)

### summarize
	summ.ee.day = data.ee.day %>%
		group_by(experiment, date, days_after_trt, var_env) %>%
		summarize(env_mean=mean(value_env, na.rm=TRUE), env_sd=sd(value_env, na.rm=TRUE), n_obs=n() ) %>%
		ungroup() %>% 
		mutate(env_stderr= env_sd/sqrt(n_obs) ) %>%
		mutate(
			env_ci_upper = env_mean + qt(1-(0.05/2), n_obs-1) * env_stderr,
			env_ci_lower = env_mean - qt(1-(0.05/2), n_obs-1) * env_stderr)	

### prepare
	## create df to annotate with rating dates
		## make tibbles
		rating.dates.1 = tibble(
			experiment = c("A-2-row","A-2-row","A-2-row","B-3-row","B-3-row","B-3-row","C-3-row","C-3-row","C-3-row"),
			var_env    = c("temp_day","temp_day","temp_day","temp_day","temp_day","temp_day","temp_day","temp_day","temp_day"),
			date	   = as_date(c("2018-02-13","2018-03-13","2018-04-11","2018-02-13","2018-03-13","2018-04-11","2018-10-17","2018-11-07","2018-11-28")),
			label 	   = c("02-13", "03-13", "04-11", "02-13", "03-13", "04-11", "10-17", "11-07", "11-28") )

		rating.dates.2 = tibble(
			experiment = c("A-2-row","A-2-row","A-2-row","B-3-row","B-3-row","B-3-row","C-3-row","C-3-row","C-3-row"),
			var_env    = c("lwe_rh90_all","lwe_rh90_all","lwe_rh90_all","lwe_rh90_all","lwe_rh90_all","lwe_rh90_all","lwe_rh90_all","lwe_rh90_all","lwe_rh90_all"),
			date	   = as_date(c("2018-02-13","2018-03-13","2018-04-11","2018-02-13","2018-03-13","2018-04-11","2018-10-17","2018-11-07","2018-11-28")),
			label 	   = c("02-13", "03-13", "04-11", "02-13", "03-13", "04-11", "10-17", "11-07", "11-28") )
	
		## calculate days after treatment
		rating.dates.1 = rating.dates.1 %>% mutate(days_after_trt=case_when(
			(experiment %in% c("A-2-row","B-3-row") ) ~ date - as_date("2018-01-05"),
			(experiment == "C-3-row") ~ date - as_date("2018-09-25") ) )

		## calculate days after treatment
		rating.dates.2 = rating.dates.2 %>% mutate(days_after_trt=case_when(
			(experiment %in% c("A-2-row","B-3-row") ) ~ date - as_date("2018-01-05"),
			(experiment == "C-3-row") ~ date - as_date("2018-09-25") ) )

	## graph settings
	source("./3_analysis/rcb_settings.r")	

### selected variables - B/C only - raw data
	## rh
	plot.b3.1 = summ.ee.day %>% filter(experiment %in% c("B-3-row","C-3-row") & var_env %in% c("rh_day","rh_night") ) %>% {
	ggplot(., aes(x=days_after_trt) ) +
		geom_ribbon(aes(ymin=env_ci_lower, ymax=env_ci_upper, fill=experiment), alpha=0.3 ) +
		geom_line(aes(y=env_mean, color=experiment, linetype=experiment), size=0.5) +
		facet_grid(rows=vars(var_env), labeller=labeller(var_env=c("rh_day"="During daylight", "rh_night"="During night")) ) +
		scale_x_continuous(breaks=seq(0,98, by=7) ) +
		scale_color_manual(values=vec.exp.color[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
		scale_fill_manual(values=vec.exp.color[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
		scale_linetype_manual(values=vec.exp.line[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
		theme_bw() +
		theme(axis.title.x=element_blank(), axis.text.x=element_blank() ) +
		theme(legend.position="right", legend.spacing.y=unit(0.25, "lines") ) +
		guides(color=guide_legend(byrow=TRUE), fill=guide_legend(byrow=TRUE), linetype=guide_legend(byrow=TRUE) ) +
		labs(y="Relative humidity (%)", color="Experiment", fill="Experiment", linetype="Experiment")
	}
	
	## temp
	plot.b3.2 = summ.ee.day %>% filter(experiment %in% c("B-3-row","C-3-row") & var_env %in% c("temp_day","temp_night") ) %>% {
	ggplot(., aes(x=days_after_trt) ) +
		geom_ribbon(aes(ymin=env_ci_lower, ymax=env_ci_upper, fill=experiment), alpha=0.3 ) +
		geom_line(aes(y=env_mean, color=experiment, linetype=experiment), size=0.5) +
		geom_text(data={rating.dates.1 %>% filter(experiment == "B-3-row")}, aes(x=days_after_trt, y=9.75, label=label, color=experiment), size=3, angle=90) +
		geom_text(data={rating.dates.1 %>% filter(experiment == "C-3-row")}, aes(x=days_after_trt, y=7.75, label=label, color=experiment), size=3, angle=90) +
		facet_grid(rows=vars(var_env), labeller=labeller(var_env=c("temp_day"="During daylight","temp_night"="During night")) ) +
		scale_x_continuous(breaks=seq(0,98, by=7) ) +
		scale_color_manual(values=vec.exp.color[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
		scale_fill_manual(values=vec.exp.color[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
		scale_linetype_manual(values=vec.exp.line[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
		theme_bw() +
		theme(axis.title.x=element_blank(), axis.text.x=element_blank() ) +
		theme(legend.position="right", legend.spacing.y=unit(0.25, "lines") ) +
		guides(color="none", fill="none", linetype="none" ) +
		labs(y="Temperature (C)", color="Experiment", fill="Experiment", linetype="Experiment")
	}

	## hours
	plot.b3.3 = summ.ee.day %>% filter(experiment %in% c("B-3-row","C-3-row") & var_env %in% c("lwe_rh85_all","lwe_rh90_all","temp_time1525_all") ) %>% {
	ggplot(., aes(x=days_after_trt) ) +
		geom_ribbon(aes(ymin=env_ci_lower, ymax=env_ci_upper, fill=experiment), alpha=0.3 ) +
		geom_line(aes(y=env_mean, color=experiment, linetype=experiment), size=0.5) +
		geom_text(data={rating.dates.2 %>% filter(experiment == "B-3-row")}, aes(x=days_after_trt, y=22.25, label=label, color=experiment), size=3, angle=90) +
		geom_text(data={rating.dates.2 %>% filter(experiment == "C-3-row")}, aes(x=days_after_trt, y=19.25, label=label, color=experiment), size=3, angle=90) +
		facet_grid(rows=vars(var_env), labeller=labeller(var_env=c("lwe_rh85_all"="RH>85%", "lwe_rh90_all"="RH>90%", "temp_time1525_all"="15<T<25") ) ) +
		scale_x_continuous(breaks=seq(0,98, by=7) ) +
		scale_y_continuous(limits=c(0,27) ) +
		scale_color_manual(values=vec.exp.color[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
		scale_fill_manual(values=vec.exp.color[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
		scale_linetype_manual(values=vec.exp.line[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
		theme_bw() +
		theme(legend.position="right", legend.spacing.y=unit(0.25, "lines") ) +
		guides(color="none", fill="none", linetype="none" ) +
		labs(x="Days after treatment", y="Hours meeting threshold", color="Experiment", fill="Experiment", linetype="Experiment")
	}
	
	plot.b3 = wrap_plots(plot.b3.1, plot.b3.2, plot.b3.3, ncol=1, heights=c(1,1,1.5), guides="collect") +
		plot_annotation(theme=theme(legend.position="right", legend.box.margin=margin(l=-5.5, r=-5.5) ) )	

	ggplot2::ggsave(file="./4_results/10b3_env-epi_summ_experiment_exp-BC_selected-vars_raw.png", device="png", plot=plot.b3, width=6.5, height=7, units="in", dpi=600)

### selected variables - A/B only - raw data
	## rh
	plot.b4.1 = summ.ee.day %>% filter(experiment %in% c("A-2-row","B-3-row") & var_env %in% c("rh_day","rh_night") ) %>% {
	ggplot(., aes(x=days_after_trt) ) +
		geom_ribbon(aes(ymin=env_ci_lower, ymax=env_ci_upper, fill=experiment), alpha=0.3 ) +
		geom_line(aes(y=env_mean, color=experiment, linetype=experiment), size=0.5) +
		facet_grid(rows=vars(var_env), labeller=labeller(var_env=c("rh_day"="During daylight", "rh_night"="During night")) ) +
		scale_x_continuous(limits=c(0,98), breaks=seq(0,98, by=7) ) +
		scale_color_manual(values=vec.exp.color[1:2], labels=c("A-2-row"="Ranch 1\n2-row", "B-3-row"="Ranch 1\n3-row") ) +
		scale_fill_manual(values=vec.exp.color[1:2], labels=c("A-2-row"="Ranch 1\n2-row", "B-3-row"="Ranch 1\n3-row") ) +
		scale_linetype_manual(values=vec.exp.line[1:2], labels=c("A-2-row"="Ranch 1\n2-row", "B-3-row"="Ranch 1\n3-row") ) +
		theme_bw() +
		theme(axis.title.x=element_blank(), axis.text.x=element_blank() ) +
		theme(legend.position="right", legend.spacing.y=unit(0.25, "lines") ) +
		guides(color=guide_legend(byrow=TRUE), fill=guide_legend(byrow=TRUE), linetype=guide_legend(byrow=TRUE) ) +
		labs(y="Relative humidity (%)", color="Experiment", fill="Experiment", linetype="Experiment")
	}
	
	## temp
	plot.b4.2 = summ.ee.day %>% filter(experiment %in% c("A-2-row","B-3-row") & var_env %in% c("temp_day","temp_night") ) %>% {
	ggplot(., aes(x=days_after_trt) ) +
		geom_ribbon(aes(ymin=env_ci_lower, ymax=env_ci_upper, fill=experiment), alpha=0.3 ) +
		geom_line(aes(y=env_mean, color=experiment, linetype=experiment), size=0.5) +
		geom_text(data={rating.dates.1 %>% filter(experiment == "B-3-row")}, aes(x=days_after_trt, y=9.75, label=label), size=3, angle=90) +
		facet_grid(rows=vars(var_env), labeller=labeller(var_env=c("temp_day"="During daylight","temp_night"="During night")) ) +
		scale_x_continuous(limits=c(0,98), breaks=seq(0,98, by=7) ) +
		scale_color_manual(values=vec.exp.color[1:2], labels=c("A-2-row"="Ranch 1\n2-row", "B-3-row"="Ranch 1\n3-row") ) +
		scale_fill_manual(values=vec.exp.color[1:2], labels=c("A-2-row"="Ranch 1\n2-row", "B-3-row"="Ranch 1\n3-row") ) +
		scale_linetype_manual(values=vec.exp.line[1:2], labels=c("A-2-row"="Ranch 1\n2-row", "B-3-row"="Ranch 1\n3-row") ) +
		theme_bw() +
		theme(axis.title.x=element_blank(), axis.text.x=element_blank() ) +
		theme(legend.position="right", legend.spacing.y=unit(0.25, "lines") ) +
		guides(color="none", fill="none", linetype="none" ) +
		labs(y="Temperature (C)", color="Experiment", fill="Experiment", linetype="Experiment")
	}

	## hours
	plot.b4.3 = summ.ee.day %>% filter(experiment %in% c("A-2-row","B-3-row") & var_env %in% c("lwe_rh85_all","lwe_rh90_all","temp_time1525_all") ) %>% {
	ggplot(., aes(x=days_after_trt) ) +
		geom_ribbon(aes(ymin=env_ci_lower, ymax=env_ci_upper, fill=experiment), alpha=0.3 ) +
		geom_line(aes(y=env_mean, color=experiment, linetype=experiment), size=0.5) +
		geom_text(data={rating.dates.2 %>% filter(experiment == "B-3-row")}, aes(x=days_after_trt, y=22.25, label=label), size=3, angle=90) +
		facet_grid(rows=vars(var_env), labeller=labeller(var_env=c("lwe_rh85_all"="RH>85%", "lwe_rh90_all"="RH>90%", "temp_time1525_all"="15<T<25") ) ) +
		scale_x_continuous(limits=c(0,98), breaks=seq(0,98, by=7) ) +
		scale_y_continuous(limits=c(0,27) ) +
		scale_color_manual(values=vec.exp.color[1:2], labels=c("A-2-row"="Ranch 1\n2-row", "B-3-row"="Ranch 1\n3-row") ) +
		scale_fill_manual(values=vec.exp.color[1:2], labels=c("A-2-row"="Ranch 1\n2-row", "B-3-row"="Ranch 1\n3-row") ) +
		scale_linetype_manual(values=vec.exp.line[1:2], labels=c("A-2-row"="Ranch 1\n2-row", "B-3-row"="Ranch 1\n3-row") ) +
		theme_bw() +
		theme(legend.position="right", legend.spacing.y=unit(0.25, "lines") ) +
		guides(color="none", fill="none", linetype="none" ) +
		labs(x="Days after treatment", y="Hours meeting threshold", color="Experiment", fill="Experiment", linetype="Experiment")
	}
	
	plot.b4 = wrap_plots(plot.b4.1, plot.b4.2, plot.b4.3, ncol=1, heights=c(1,1,1.5), guides="collect") +
		plot_annotation(theme=theme(legend.position="right", legend.box.margin=margin(l=-5.5, r=-5.5) ) )	

	ggplot2::ggsave(file="./4_results/10b4_env-epi_summ_experiment_exp-AB_selected-vars_raw.png", device="png", plot=plot.b4, width=6.5, height=7, units="in", dpi=600)


### selected variables - B/C only - smoothed data (note: not used)
	## rh
#	plot.b4.1 = data.ee.day %>% filter(experiment %in% c("B-3-row","C-3-row") & var_env %in% c("rh_day","rh_night") ) %>% {
#	ggplot(., aes(x=days_after_trt, y=value_env, color=experiment, fill=experiment, linetype=experiment) ) +
#		geom_smooth(method="loess", se=T, size=0.25) +
#		facet_grid(rows=vars(var_env), labeller=labeller(var_env=c("rh_day"="During daylight", "rh_night"="During night")) ) +
#		scale_x_continuous(breaks=seq(0,98, by=7) ) +
#		scale_color_manual(values=vec.exp.color[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
#		scale_fill_manual(values=vec.exp.color[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
#		scale_linetype_manual(values=c("solid","22"), labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
#		theme_bw() +
#		theme(axis.title.x=element_blank(), axis.text.x=element_blank() ) +
#		theme(legend.position="right", legend.spacing.y=unit(0.25, "lines") ) +
#		guides(color=guide_legend(byrow=TRUE), fill=guide_legend(byrow=TRUE), linetype=guide_legend(byrow=TRUE) ) +
#		labs(y="Relative humidity (%)", color="Experiment", fill="Experiment", linetype="Experiment")
#	}
#	
#	## temp
#	plot.b4.2 = data.ee.day %>% filter(experiment %in% c("B-3-row","C-3-row") & var_env %in% c("temp_day","temp_night") ) %>% {
#	ggplot(., aes(x=days_after_trt, y=value_env, color=experiment, fill=experiment, linetype=experiment) ) +
#		geom_smooth(method="loess", se=T, size=0.5) +
#		geom_text(data={rating.dates.1 %>% filter(experiment == "B-3-row")}, aes(x=days_after_trt, y=11.75, label=label, color=experiment), size=3, angle=90) +
#		geom_text(data={rating.dates.1 %>% filter(experiment == "C-3-row")}, aes(x=days_after_trt, y=9.75, label=label, color=experiment), size=3, angle=90) +
#		facet_grid(rows=vars(var_env), labeller=labeller(var_env=c("temp_day"="During daylight","temp_night"="During night")) ) +
#		scale_x_continuous(breaks=seq(0,98, by=7) ) +
#		scale_color_manual(values=vec.exp.color[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
#		scale_fill_manual(values=vec.exp.color[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
#		scale_linetype_manual(values=c("solid","22"), labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
#		theme_bw() +
#		theme(axis.title.x=element_blank(), axis.text.x=element_blank() ) +
#		theme(legend.position="right", legend.spacing.y=unit(0.25, "lines") ) +
#		guides(color="none", fill="none", linetype="none" ) +
#		labs(y="Temperature (C)", color="Experiment", fill="Experiment", linetype="Experiment")
#	}
#
#	## hours
#	plot.b4.3 = data.ee.day %>% filter(experiment %in% c("B-3-row","C-3-row") & var_env %in% c("lwe_rh85_all","lwe_rh90_all","temp_time1525_all") ) %>% {
#	ggplot(., aes(x=days_after_trt, y=value_env, color=experiment, fill=experiment, linetype=experiment) ) +
#		geom_smooth(method="loess", se=T, size=0.5) +
#		geom_text(data={rating.dates.2 %>% filter(experiment == "B-3-row")}, aes(x=days_after_trt, y=15.5, label=label, color=experiment), size=3, angle=90) +
#		geom_text(data={rating.dates.2 %>% filter(experiment == "C-3-row")}, aes(x=days_after_trt, y=13.75, label=label, color=experiment), size=3, angle=90) +
#		facet_grid(rows=vars(var_env), labeller=labeller(var_env=c("lwe_rh85_all"="RH>85%", "lwe_rh90_all"="RH>90%", "temp_time1525_all"="15<T<25") ) ) +
#		scale_x_continuous(breaks=seq(0,98, by=7) ) +
#		scale_color_manual(values=vec.exp.color[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
#		scale_fill_manual(values=vec.exp.color[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
#		scale_linetype_manual(values=c("solid","22"), labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
#		theme_bw() +
#		theme(legend.position="right", legend.spacing.y=unit(0.25, "lines") ) +
#		guides(color="none", fill="none", linetype="none" ) +
#		labs(x="Days after treatment", y="Hours meeting threshold", color="Experiment", fill="Experiment", linetype="Experiment")
#	}
#	
#	plot.b4 = wrap_plots(plot.b4.1, plot.b4.2, plot.b4.3, ncol=1, heights=c(1,1,1.5), guides="collect") +
#		plot_annotation(theme=theme(legend.position="right", legend.box.margin=margin(l=-5.5, r=-5.5) ) )	
#
#	ggplot2::ggsave(file="./4_results/10b4_env-epi_summ_experiment_exp-BC_selected-vars_smooth.png", device="png", plot=plot.b4, width=6.5, height=7, units="in", dpi=600)


##############################################
# C. Visualize - with vapor pressure deficit #
##############################################

### selected variables - B/C only - raw data
	## vpd
	plot.c3.1.2 = summ.ee.day %>% filter(experiment %in% c("B-3-row","C-3-row") & var_env %in% c("vpd_day","vpd_night") ) %>% {
	ggplot(., aes(x=days_after_trt) ) +
		geom_ribbon(aes(ymin=env_ci_lower, ymax=env_ci_upper, fill=experiment), alpha=0.3 ) +
		geom_line(aes(y=env_mean, color=experiment, linetype=experiment), size=0.5) +
		facet_grid(rows=vars(var_env), labeller=labeller(var_env=c("vpd_day"="During daylight", "vpd_night"="During night")) ) +
		scale_x_continuous(breaks=seq(0,98, by=7) ) +
		scale_color_manual(values=vec.exp.color[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
		scale_fill_manual(values=vec.exp.color[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
		scale_linetype_manual(values=vec.exp.line[2:3], labels=c("B-3-row"="Ranch 1\n3-row", "C-3-row"="Ranch 2\n3-row") ) +
		theme_bw() +
		theme(axis.title.x=element_blank(), axis.text.x=element_blank() ) +
		theme(legend.position="right", legend.spacing.y=unit(0.25, "lines") ) +
		guides(color=guide_legend(byrow=TRUE), fill=guide_legend(byrow=TRUE), linetype=guide_legend(byrow=TRUE) ) +
		labs(y="Vapor pressure deficit (kPa)", color="Experiment", fill="Experiment", linetype="Experiment")
	}
		
	plot.c3 = wrap_plots(plot.b3.1, plot.c3.1.2, plot.b3.2, plot.b3.3, ncol=1, heights=c(1,1,1,1.5), guides="collect") +
		plot_annotation(theme=theme(legend.position="right", legend.box.margin=margin(l=-5.5, r=-5.5) ) )	

	ggplot2::ggsave(file="./4_results/z_10c3_env-epi_summ_experiment_exp-BC_selected-vars_raw_not-shown.png", device="png", plot=plot.c3, width=6.5, height=9, units="in", dpi=600)
