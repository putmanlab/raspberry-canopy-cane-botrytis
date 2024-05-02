#####################################
# RASPBERRY Cane Botrytis 			#
# Disease and Environmental Data	#
# Correlations - Last rating date	#
#####################################
# determine if significant treatment effects on temp/rh correlate with disease

## built on Docker putmanlab/exploratory-analysis:420.0

library(conflicted)

library(dplyr)
library(forcats)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)

library(patchwork) # for combining plots: wrap_plots

# install ggpmisc for printing line equation, R-squared via stat_poly_eq(); after dependencies ggpp (after dependency xts), confintr, lmodel2
remotes::install_version("xts", version="0.12-0", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
remotes::install_version("ggpp", version="0.4.5", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
remotes::install_version("confintr", version="0.1.2", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
remotes::install_version("lmodel2", version="1.7-3", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
remotes::install_version("ggpmisc", version="0.5.1", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
library(ggpmisc)

## install ggh4x for force_panelsizes to customize facet width
remotes::install_version("ggh4x", version="0.2.1", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
library(ggh4x)

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
	## daily
	data.env.day = read_csv(file="./2_data_curated/rasp-caneb_04f_environ_final-day.csv", col_names=T, na=".")

	## epidemiological statistics - daily
	summ.env.day.l = read_csv(file="./2_data_curated/rasp-caneb_06-2b_environ_summ-epi_day.csv", col_names=T, na="")
	
### disease
	## import
	in.incid = read_csv(file="./2_data_curated/rasp-caneb_02-1_disease_incidence_final.csv", col_names=T, na=c("","."))
	in.sever = read_csv(file="./2_data_curated/rasp-caneb_02-3_disease_severity_plot-mean_final.csv", col_names=T, na=c("","."))

	## join
	data.dis = in.incid %>% left_join(in.sever, by=c(c("experiment","trt","block","date","days_after_trt")) )
		
	## remove unneeded columns
	data.dis = data.dis %>% select(-infected_canes, -low_n_later, -low_n_above, -low_n_healthy, -low_n_disease, -low_n_canes, -low_n_na, -low_mean_nlesions, -above_length_mean_plot)
		
	## rename column
	data.dis = data.dis %>% rename(treatment=trt)

### set strip labels
	facet.lab.vardis = c("incid"="Incidence (%)", "length_mean_plot"="Lesion length (cm)")
	facet.lab.exp = c('A-2-row'="Ranch 1 2-row", 'B-3-row'="Ranch 1 3-row", 'C-3-row'="Ranch 2 3-row")
	facet.lab.varenv = c(
		"lwe_rh85"="hr of RH>85%", "lwe_rh90"="hr of RH>90%", "rh_day"="%RH daylight", "rh_night"="%RH night",
		"temp_day"="Temp (C) day", "temp_night"="Temp (C) night", "lwe_dpd20"="hr dewpt. depress", "temprh_rh90"="hr 15<T<25, RH>90%")

### graph settings
	source("./3_analysis/rcb_settings.r")	


#########################
# B. Disease with Canes #
#########################
# note: deleted, moved to 09 multivar


####################################
# C. Relative humidity - treatment #
####################################
# environ daily mean vs disease final
# from 05-2d_env_day_stat-analysis_rh_treatment-B

### prepare
	## disease 
		# filter for experiment, final rating date (changed to all experiments)
		data.dis.c = data.dis %>% filter( (experiment %in% c("A-2-row","B-3-row") & date == as_date("2018-04-11") ) | (experiment == "C-3-row" & date == as_date("2018-11-28") ) )
	
		# remove unneeded column
		data.dis.c = data.dis.c %>% select(-date, -days_after_trt, -canes)
		
		# convert to long
		data.dis.c.l = data.dis.c %>% gather(key="var_dis", value="value_dis", -experiment, -treatment, -block)

	## environment - average across time
		# calculate
		data.env.c = data.env.day %>% 
			group_by(experiment, treatment, block) %>% 
			summarize(rh_avg=round( mean(rh_avg, na.rm=TRUE), digits=1) ) %>%
			ungroup()
		
	## join
	data.c = data.dis.c.l %>% left_join(data.env.c, by=c(c("experiment","treatment","block")) )

	## get range
	data.c %>% group_by(var_dis) %>% summarize(value_min=min(value_dis, na.rm=TRUE), value_max=max(value_dis, na.rm=TRUE) )
	data.c %>% summarize(rh_min=min(rh_avg, na.rm=TRUE), rh_max=max(rh_avg, na.rm=TRUE) )
	
	## make annotation tibble
	label.text="leaf removal × period interaction"
	annot.c = tibble(
		experiment = c("A-2-row","A-2-row",		   "C-3-row","C-3-row"),
		var_dis	   = c("incid","length_mean_plot","incid","length_mean_plot"),
		label_text = c(label.text,label.text,	  label.text,label.text),
		rh_avg	   = c(76.05,76.05,76.05,76.05),
		value_dis  = c(0,0,0,0) )

	## names and orders
		# change order of treatments
		data.c = data.c %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))

### visualize (0 excluded for incidence by default because no points in that range; set custom facet limits to include 0 for incidence)
	base.size=11
	plot.c = ggplot(data.c, aes(x=rh_avg, y=value_dis) ) +
		geom_point(aes(shape=treatment, color=treatment), size=2.25) +
		geom_text(data=annot.c, aes(label=label_text), size=3.5, color="black", hjust=0.5) +
		stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.75, label.x=0.05, label.y=0.98, small.r=TRUE ) +
		stat_correlation(method="pearson", use_label(c("P","n")), size=2.75, label.x=0.05, label.y=0.92 ) +
		facet_grid(rows=vars(var_dis), cols=vars(experiment), scales="free_y", switch="y", labeller=labeller(experiment=facet.lab.exp, var_dis=facet.lab.vardis) ) +
		facetted_pos_scales(y=list(
			(var_dis == "incid") ~ scale_y_continuous(limits=c(0,91.5), expand=expansion(mult=c(0.05,0.20)) ),
			(var_dis == "length_mean_plot") ~ scale_y_continuous(limits=c(0,46.3), expand=expansion(mult=c(0.05,0.20)) ) )) +			
		scale_shape_manual(values=vec.trt.shape) +
		scale_color_manual(values=vec.trt.color) +
		theme_bw() +
 		theme(axis.title.x=element_text(size=base.size+2, margin=margin(t=8.25)), axis.title.y=element_blank(), axis.text=element_text(size=base.size) ) +
		theme(strip.text.x=element_text(size=base.size+1, margin=margin(t=2.75, b=2.75)), strip.text.y=element_text(size=base.size+1, margin=margin(l=2.75, r=2.75)), 
			strip.background.y=element_blank(), strip.placement.y="outside" ) + 		
		theme(legend.position="bottom", legend.box.margin=margin(t=-8.25, b=-8.25), 
			legend.title=element_text(size=base.size+1), legend.text=element_text(size=base.size, margin=margin(l=-8.25)) ) +
		labs(x="Relative humidity (%)", shape="Leaf removal", color="Leaf removal")

 	ggplot2::ggsave(file="./4_results/07-1c_dis-env_end_rh_treatment.png", device="png", plot=plot.c, width=7, height=6.2, units="in", dpi=600)

		# results: no signif correlations
		

#############################################
# D. Relative humidity - treatment x period #
#############################################
# environ daily mean vs disease final
# from 05-2d_env_day_stat-analysis_rh_trt-period-AC

### prepare
	## disease
		# filter for experiment, final rating date (changed to all experiments)
		data.dis.d = data.dis %>% filter( (experiment %in% c("A-2-row","B-3-row") & date == as_date("2018-04-11") ) | (experiment == "C-3-row" & date == as_date("2018-11-28") ) )
	
		# remove unneeded column (added for per block)
		data.dis.d = data.dis.d %>% select(-date, -days_after_trt, -canes)
		
		# convert to long
		data.dis.d.l = data.dis.d %>% gather(key="var_dis", value="value_dis", -experiment, -treatment, -block)
	
	## environment - average across time
		# calculate
		data.env.d = data.env.day %>% 
			group_by(experiment, treatment, block, period) %>% 
			summarize(rh_avg=round( mean(rh_avg, na.rm=TRUE), digits=1) ) %>%
			ungroup()
		
	## join
	data.d = data.dis.d.l %>% left_join(data.env.d, by=c(c("experiment","treatment","block")) )
	
	## show ranges to get axis limits
	data.d %>% 
		group_by(period) %>%
		summarize(rh_min=min(rh_avg, na.rm=TRUE), rh_max=max(rh_avg, na.rm=TRUE) ) 

	## get range
	data.d %>% group_by(var_dis) %>% summarize(value_min=min(value_dis, na.rm=TRUE), value_max=max(value_dis, na.rm=TRUE))
	data.d %>% summarize(rh_min=min(rh_avg, na.rm=TRUE), rh_max=max(rh_avg, na.rm=TRUE) )
	
	## make annotation tibble
	annot.d = tibble(
		experiment = c("B-3-row","B-3-row",		   				 "B-3-row","B-3-row",		   			   "C-3-row","C-3-row"),
		period	   = c("day","day",				   				 "night","night",			   			   "day","day"),
		var_dis	   = c("incid","length_mean_plot",				 "incid","length_mean_plot",			   "incid","length_mean_plot"),
		label_text = c("interaction 0.3796","interaction 0.3796","interaction 0.3796","interaction 0.3796","leaf removal 0.3665","leaf removal 0.3665"),
		rh_avg	   = c(74.6,74.6,				   				 74.6,74.6,				  				   74.6,74.6),
		value_dis  = c(-2,-1,						  			 -2,-1,						   			   -2,-1) )

	## names and orders
		# change order of treatments
		data.d = data.d %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))

### visualize
	base.size=10
	## day; x limits = range + 0.25 on either end (total of 10)
	plot.d.d = data.d %>% filter(period == "day") %>% {
	ggplot(., aes(x=rh_avg, y=value_dis) ) +
		geom_point(aes(shape=treatment, color=treatment), size=2) +
		geom_text(data={ annot.d %>% filter(period == "day") }, aes(label=label_text), size=3, color="black", hjust=0.5) +
		stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.2, label.x=0.05, label.y=0.98, small.r=TRUE ) +
		stat_correlation(method="pearson", use_label(c("P","n")), size=2.2, label.x=0.05, label.y=0.90 ) +
		facet_grid(rows=vars(var_dis), cols=vars(period, experiment), scales="free", labeller=labeller(experiment=facet.lab.exp, period=c("day"="Daylight", "night"="Night") ) ) +
		scale_x_continuous(limits=c(63.9,85.3) ) +
		facetted_pos_scales(y=list(
			(var_dis == "incid") ~ scale_y_continuous(limits=c(0,91.5), expand=expansion(mult=c(0.10,0.25)) ),
			(var_dis == "length_mean_plot") ~ scale_y_continuous(limits=c(0,46.3), expand=expansion(mult=c(0.10,0.25)) ) )) +			
		scale_shape_manual(values=vec.trt.shape) +
		scale_color_manual(values=vec.trt.color) +
		theme_bw() +
		theme(axis.title.x=element_text(size=base.size+2, margin=margin(t=8.25)), axis.title.y=element_blank(), axis.text=element_text(size=base.size) ) + 
		theme(strip.text.x=element_text(size=base.size+1, margin=margin(t=2.75, b=2.75)), strip.text.y=element_blank(), strip.background.y=element_blank() ) +
		theme(legend.title=element_text(size=base.size+1), legend.text=element_text(size=base.size, margin=margin(l=-8.25)) ) +
		guides(shape=guide_legend(direction="horizontal"), color=guide_legend(direction="horizontal") ) +
		labs(x="Relative humidity (%)", shape="Leaf removal", color="Leaf removal")
	} 

	## night; x axis same scale as day (range of 10) but with different limits
	plot.d.n = data.d %>% filter(period == "night") %>% {
	ggplot(., aes(x=rh_avg, y=value_dis) ) +
		geom_point(aes(shape=treatment, color=treatment), size=2) +
		geom_text(data={ annot.d %>% filter(period == "night") }, aes(label=label_text), size=3, color="black", hjust=0.5) +
		stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.2, label.x=0.05, label.y=0.98, small.r=TRUE ) +
		stat_correlation(method="pearson", use_label(c("P","n")), size=2.2, label.x=0.05, label.y=0.90 ) +
		facet_grid(rows=vars(var_dis), cols=vars(period, experiment), scales="free", labeller=labeller(experiment=facet.lab.exp, var_dis=facet.lab.vardis, period=c("day"="Daylight", "night"="Night") ) ) +
		scale_x_continuous(limits=c(63.9,85.3) ) +
		facetted_pos_scales(y=list(
			(var_dis == "incid") ~ scale_y_continuous(limits=c(0,91.5), expand=expansion(mult=c(0.10,0.25)) ),
			(var_dis == "length_mean_plot") ~ scale_y_continuous(limits=c(0,46.3), expand=expansion(mult=c(0.10,0.25)) ) )) +			
		scale_shape_manual(values=vec.trt.shape) +
		scale_color_manual(values=vec.trt.color) +
		theme_bw() +
		theme(axis.title.x=element_text(size=base.size+2, margin=margin(t=8.25)), axis.title.y=element_blank(), axis.text.x=element_text(size=base.size), axis.text.y=element_blank() ) +
		theme(strip.text.x=element_text(size=base.size+1, margin=margin(t=2.75, b=2.75)), strip.text.y=element_text(size=base.size+1, margin=margin(l=2.75, r=2.75)) ) +
		theme(legend.title=element_text(size=base.size+1), legend.text=element_text(size=base.size, margin=margin(l=-8.25)) ) +
		guides(shape=guide_legend(direction="horizontal"), color=guide_legend(direction="horizontal") ) +
		labs(x="Relative humidity (%)", shape="Leaf removal", color="Leaf removal")
	}
	
	plot.d = wrap_plots(plot.d.d, plot.d.n, nrow=1, guides="collect") +
		plot_annotation(theme=theme(legend.position="bottom", legend.box.margin=margin(t=-8.25, b=-8.25) ) )

 	ggplot2::ggsave(file="./4_results/07-1d_dis-env_end_rh_treatment-period.png", device="png", plot=plot.d, width=9, height=4.7, units="in", dpi=600)

		# results: no signif correlations
	
		
#######################################
# E. Temperature - treatment x period #
#######################################
# environ daily mean vs disease final
# from 05-2d_env_day_stat-analysis_temp_trt-period

### prepare
	## disease
		# filter for experiment, final rating date
		data.dis.e = data.dis %>% filter( (experiment %in% c("A-2-row","B-3-row") & date == as_date("2018-04-11") ) | (experiment == "C-3-row" & date == as_date("2018-11-28") ) )
	
		# remove unneeded column (added for per block)
		data.dis.e = data.dis.e %>% select(-date, -days_after_trt, -canes)
		
		# convert to long
		data.dis.e.l = data.dis.e %>% gather(key="var_dis", value="value_dis", -experiment, -treatment, -block)
	
	## environment - average across time
		# calculate
		data.env.e = data.env.day %>% 
			group_by(experiment, treatment, block, period) %>% 
			summarize(temp_avg=round( mean(temp_avg, na.rm=TRUE), digits=1) ) %>%
			ungroup()
		
	## join
	data.e = data.dis.e.l %>% left_join(data.env.e, by=c(c("experiment","treatment","block")) )

	## show ranges to get axis limits
	data.e %>% group_by(var_dis) %>% summarize(value_min=min(value_dis, na.rm=TRUE), value_max=max(value_dis, na.rm=TRUE))
	data.e %>% summarize(temp_min=min(temp_avg, na.rm=TRUE), temp_max=max(temp_avg, na.rm=TRUE) )

	## names and orders
		# change order of treatments
		data.e = data.e %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))
		
### visualize
	base.size=10
	## day
	plot.e.d = data.e %>% filter(period == "day") %>% {
	ggplot(., aes(x=temp_avg, y=value_dis) ) +
		geom_point(aes(shape=treatment, color=treatment), size=2) +
		stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.2, label.x=0.05, label.y=0.98, small.r=TRUE ) +
		stat_correlation(method="pearson", use_label(c("P","n")), size=2.2, label.x=0.05, label.y=0.90 ) +
		facet_grid(rows=vars(var_dis), cols=vars(period, experiment), scales="free", labeller=labeller(experiment=facet.lab.exp, period=c("day"="Daylight", "night"="Night") ) ) +
		scale_x_continuous(limits=c(15.95, 21.15) ) +
		facetted_pos_scales(y=list(
			(var_dis == "incid") ~ scale_y_continuous(limits=c(0,91.5), expand=expansion(mult=c(0.05,0.25)) ),
			(var_dis == "length_mean_plot") ~ scale_y_continuous(limits=c(0,46.3), expand=expansion(mult=c(0.05,0.25)) ) )) +			
		scale_shape_manual(values=vec.trt.shape) +
		scale_color_manual(values=vec.trt.color) +
		theme_bw() +
		theme(axis.title.x=element_text(size=base.size+2, margin=margin(t=8.25)), axis.title.y=element_blank(), axis.text=element_text(size=base.size) ) + 
		theme(strip.text.x=element_text(size=base.size+1, margin=margin(t=2.75, b=2.75)), strip.text.y=element_blank(), strip.background.y=element_blank() ) +
		theme(legend.title=element_text(size=base.size+1), legend.text=element_text(size=base.size, margin=margin(l=-8.25)) ) +
		guides(shape=guide_legend(direction="horizontal"), color=guide_legend(direction="horizontal") ) +
		labs(x="Temperature (C)", shape="Leaf removal", color="Leaf removal")
	}

	## night
	plot.e.n = data.e %>% filter(period == "night") %>% {
	ggplot(., aes(x=temp_avg, y=value_dis) ) +
		geom_point(aes(shape=treatment, color=treatment), size=2) +
		stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.2, label.x=0.05, label.y=0.98, small.r=TRUE ) +
		stat_correlation(method="pearson", use_label(c("P","n")), size=2.2, label.x=0.05, label.y=0.90 ) +
		facet_grid(rows=vars(var_dis), cols=vars(period, experiment), scales="free", labeller=labeller(experiment=facet.lab.exp, var_dis=facet.lab.vardis, period=c("day"="Daylight", "night"="Night") ) ) +
		scale_x_continuous(limits=c(9.5,15) ) +
		facetted_pos_scales(y=list(
			(var_dis == "incid") ~ scale_y_continuous(limits=c(0,91.5), expand=expansion(mult=c(0.05,0.25)) ),
			(var_dis == "length_mean_plot") ~ scale_y_continuous(limits=c(0,46.3), expand=expansion(mult=c(0.05,0.25)) ) )) +			
		scale_shape_manual(values=vec.trt.shape) +
		scale_color_manual(values=vec.trt.color) +
		theme_bw() +
		theme(axis.title.x=element_text(size=base.size+2, margin=margin(t=8.25)), axis.title.y=element_blank(), axis.text.x=element_text(size=base.size), axis.text.y=element_blank() ) +
		theme(strip.text.x=element_text(size=base.size+1, margin=margin(t=2.75, b=2.75)), strip.text.y=element_text(size=base.size+1, margin=margin(l=2.75, r=2.75)) ) +
		theme(legend.title=element_text(size=base.size+1), legend.text=element_text(size=base.size, margin=margin(l=-8.25)) ) +
		guides(shape=guide_legend(direction="horizontal"), color=guide_legend(direction="horizontal") ) +
		labs(x="Temperature (C)", shape="Leaf removal", color="Leaf removal")
	}
	
	plot.e = wrap_plots(plot.e.d, plot.e.n, nrow=1, guides="collect") +
		plot_annotation(theme=theme(legend.position="bottom", legend.box.margin=margin(t=-8.25, b=-8.25) ) )

 	ggplot2::ggsave(file="./4_results/07-1e_dis-env_end_temp_treatment-period.png", device="png", plot=plot.e, width=9, height=4.7, units="in", dpi=600)
	
		# results: no signif correlations
		

##########################################
# F. leaf wetness est. rh>90 - treatment #
##########################################
# epidemiology summary stat rating date mean vs disease final
# from 06-3d_env_rating_stat-analysis_lwe-rh90

### prepare
	## disease 
		# filter for experiment, final rating date 
		data.dis.f = data.dis %>% filter( (experiment %in% c("A-2-row","B-3-row") & date == as_date("2018-04-11") ) | (experiment == "C-3-row" & date == as_date("2018-11-28") ) )
	
		# remove unneeded column
		data.dis.f = data.dis.f %>% select(-date, -days_after_trt, -canes)
			
		# convert to long
		data.dis.f.l = data.dis.f %>% gather(key="var_dis", value="value_dis", -experiment, -treatment, -block)

	## environment - average across time
		# calculate
		data.env.f = summ.env.day.l %>% 
			group_by(experiment, treatment, block, var_env) %>% 
			summarize(value_env_avg=round( mean(value_env, na.rm=TRUE), digits=1) ) %>%
			ungroup()
		
		# filter for experiment, variable
		data.env.f = data.env.f %>% filter(var_env == "lwe_rh90_all") 
		
		# convert to wide
		data.env.f = data.env.f %>% spread(key=var_env, value=value_env_avg)

	## join
	data.f = data.dis.f.l %>% left_join(data.env.f, by=c(c("experiment","treatment","block")) )

	## show ranges to get axis limits
	data.f %>% group_by(var_dis) %>% summarize(value_min=min(value_dis, na.rm=TRUE), value_max=max(value_dis, na.rm=TRUE))
	data.f %>% summarize(lwerh90_min=min(lwe_rh90_all, na.rm=TRUE), lwerh90_max=max(lwe_rh90_all, na.rm=TRUE) )

	## make annotation tibble
	annot.f = tibble(
		experiment = c("A-2-row","A-2-row",		   				   "B-3-row","B-3-row"),
		var_dis	   = c("incid","length_mean_plot",				   "incid","length_mean_plot"),
		label_text = c("leaf removal 0.1380","leaf removal 0.1380","leaf removal 0.7852","leaf removal 0.7852"),
		lwe_rh90_all=c(4.7,4.7,4.7,4.7),
		value_dis  = c(0,0,0,0) )

	## names and orders
		# change order of treatments
		data.f = data.f %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))
		
### visualize
	base.size=11
	plot.f = ggplot(data.f, aes(x=lwe_rh90_all, y=value_dis) ) +
		geom_point(aes(shape=treatment, color=treatment), size=2.25) +
		geom_text(data=annot.f, aes(label=label_text), size=3.5, color="black", hjust=0.5) +
		stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.75, label.x=0.05, label.y=0.98, small.r=TRUE ) +
		stat_correlation(method="pearson", use_label(c("P","n")), size=2.75, label.x=0.05, label.y=0.92 ) +
		facet_grid(rows=vars(var_dis), cols=vars(experiment), scales="free_y", switch="y", labeller=labeller(experiment=facet.lab.exp, var_dis=facet.lab.vardis) ) +
		scale_x_continuous(breaks=c(3,4,5,6) ) +
		facetted_pos_scales(y=list(
			(var_dis == "incid") ~ scale_y_continuous(limits=c(0,91.5), expand=expansion(mult=c(0.05,0.20)) ),
			(var_dis == "length_mean_plot") ~ scale_y_continuous(limits=c(0,46.3), expand=expansion(mult=c(0.05,0.20)) ) )) +			
		scale_shape_manual(values=vec.trt.shape) +
		scale_color_manual(values=vec.trt.color) +
		theme_bw() +
 		theme(axis.title.x=element_text(size=base.size+2, margin=margin(t=8.25)), axis.title.y=element_blank(), axis.text=element_text(size=base.size) ) +
		theme(strip.text.x=element_text(size=base.size+1, margin=margin(t=2.75, b=2.75)), strip.text.y=element_text(size=base.size+1, margin=margin(l=2.75, r=2.75)), 
			strip.background.y=element_blank(), strip.placement.y="outside" ) + 		
		theme(legend.position="bottom", legend.box.margin=margin(t=-8.25, b=-8.25), 
			legend.title=element_text(size=base.size+1), legend.text=element_text(size=base.size, margin=margin(l=-8.25)) ) +
		labs(x="Hours/day with relative humidity >90%", shape="Leaf removal", color="Leaf removal")
		
	ggplot2::ggsave(file="./4_results/07-1f_dis-env_end_lwe-rh90_treatment.png", device="png", plot=plot.f, width=7, height=6.2, units="in", dpi=600)

		# results: no signif correlations
		
		
########################################
# G. 15 < temperature < 25 - treatment #
########################################
# epidemiology summary stat rating date mean vs disease final
# from 06-3d_env_rating_stat-analysis_time-temp1525

### prepare
	## disease 
		# filter for experiment, final rating date 
		data.dis.g = data.dis %>% filter( (experiment %in% c("A-2-row","B-3-row") & date == as_date("2018-04-11") ) | (experiment == "C-3-row" & date == as_date("2018-11-28") ) )
	
		# remove unneeded column
		data.dis.g = data.dis.g %>% select(-date, -days_after_trt, -canes)
			
		# convert to long
		data.dis.g.l = data.dis.g %>% gather(key="var_dis", value="value_dis", -experiment, -treatment, -block)

	## environment - average across time
		# calculate
		data.env.g = summ.env.day.l %>% 
			group_by(experiment, treatment, block, var_env) %>% 
			summarize(value_env_avg=round( mean(value_env, na.rm=TRUE), digits=1) ) %>%
			ungroup()
		
		# filter for experiment, variable
		data.env.g = data.env.g %>% filter(var_env == "temp_time1525_all") 

		# convert to wide
		data.env.g = data.env.g %>% spread(key=var_env, value=value_env_avg)

	## join
	data.g = data.dis.g.l %>% left_join(data.env.g, by=c(c("experiment","treatment","block")) )

	## show ranges to get axis limits
	data.g %>% group_by(var_dis) %>% summarize(value_min=min(value_dis, na.rm=TRUE), value_max=max(value_dis, na.rm=TRUE))
	data.g %>% summarize(temp1525_min=min(temp_time1525_all, na.rm=TRUE), temp1525_max=max(temp_time1525_all, na.rm=TRUE) )

	## make annotation tibble
	annot.g = tibble(
		experiment 		 = c("A-2-row","A-2-row",		   			 "B-3-row","B-3-row"),
		var_dis	   		 = c("incid","length_mean_plot",			 "incid","length_mean_plot"),
		label_text 		 = c("leaf removal n.s.","leaf removal n.s.","leaf removal n.s.","leaf removal n.s."),
		temp_time1525_all= c(10.85,10.85,10.85,10.85),
		value_dis  		 = c(0,0,0,0) )

	## names and orders
		# change order of treatments
		data.g = data.g %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))
	
### visualize
	base.size=11

	plot.g = ggplot(data.g, aes(x=temp_time1525_all, y=value_dis) ) +
		geom_point(aes(shape=treatment, color=treatment), size=2.25) +
		geom_text(data=annot.g, aes(label=label_text), size=3.5, color="black", hjust=0.5) +
		stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.75, label.x=0.05, label.y=0.98, small.r=TRUE ) +
		stat_correlation(method="pearson", use_label(c("P","n")), size=2.75, label.x=0.05, label.y=0.92 ) +
		facet_grid(rows=vars(var_dis), cols=vars(experiment), scales="free_y", switch="y", labeller=labeller(experiment=facet.lab.exp, var_dis=facet.lab.vardis) ) +
		scale_x_continuous(breaks=c(8,10,12,14) ) +
		facetted_pos_scales(y=list(
			(var_dis == "incid") ~ scale_y_continuous(limits=c(0,91.5), expand=expansion(mult=c(0.05,0.20)) ),
			(var_dis == "length_mean_plot") ~ scale_y_continuous(limits=c(0,46.3), expand=expansion(mult=c(0.05,0.20)) ) )) +			
		scale_shape_manual(values=vec.trt.shape) +
		scale_color_manual(values=vec.trt.color) +
		theme_bw() +
 		theme(axis.title.x=element_text(size=base.size+2, margin=margin(t=8.25)), axis.title.y=element_blank(), axis.text=element_text(size=base.size) ) +
		theme(strip.text.x=element_text(size=base.size+1, margin=margin(t=2.75, b=2.75)), strip.text.y=element_text(size=base.size+1, margin=margin(l=2.75, r=2.75)), 
			strip.background.y=element_blank(), strip.placement.y="outside" ) + 		
		theme(legend.position="bottom", legend.box.margin=margin(t=-8.25, b=-8.25), 
			legend.title=element_text(size=base.size+1), legend.text=element_text(size=base.size, margin=margin(l=-8.25)) ) +
		labs(x="Hours/day with 15C < temperature < 25C", shape="Leaf removal", color="Leaf removal")
		
	ggplot2::ggsave(file="./4_results/07-1g_dis-env_end_temp-1525_treatment.png", device="png", plot=plot.g, width=7, height=6.2, units="in", dpi=600)

		# results: B-3-row severity weakly signif but is likely spurious because range in x-axis is very small
			# no other signif correlations
