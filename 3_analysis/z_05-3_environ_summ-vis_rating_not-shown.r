###########################
# RASPBERRY Cane Botrytis #
# Environmental Data	  #
# Summarize and Visualize #
# Rating date			  #
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

### rating date 
	data.env.rat = read_csv(file="./2_data_curated/rasp-caneb_04g_environ_final-rating.csv", col_names=T, na=".")


##################################
# B. Rating Date - Visualize Raw #
##################################
		
### visualize
	# note: commented out because superseded by section C analysis below
	
#	## relative humidity
#	plot.b.r = ggplot(data.env.rat, aes(x=as.character(block), y=rh_avg, color=treatment) ) +
#		geom_point(aes(shape=treatment), position=position_jitter(w=0.1) ) +
#		geom_line(aes(linetype=treatment, group=treatment), size=0.2) +
#		facet_grid(cols=vars(experiment, date_rating), rows=vars(period), scales="free_y" ) +
#		scale_shape_manual(values=c(0,1,2,5) ) +
#		theme_bw() +
#		theme(legend.position="bottom", legend.margin=margin(t=-5.5, b=0) ) +
#		labs(x="Block", y="Relative humidity (%)")
#	ggplot2::ggsave(file="./4_results/05-3b_env_rating_raw_rh.png", device="png", plot=plot.b.r, width=9, height=5.5, units="in", dpi=600)	
#
#	## temperature
#	plot.b.t = ggplot(data.env.rat, aes(x=as.character(block), y=temp_avg, color=treatment) ) +
#		geom_point(aes(shape=treatment), position=position_jitter(w=0.1) ) +
#		geom_line(aes(linetype=treatment, group=treatment), size=0.2) +
#		facet_grid(cols=vars(experiment, date_rating), rows=vars(period), scales="free_y" ) +
#		scale_shape_manual(values=c(0,1,2,5) ) +
#		theme_bw() +
#		theme(legend.position="bottom", legend.margin=margin(t=-5.5, b=0) ) +
#		labs(x="Block", y="Temperature (C)")
#	ggplot2::ggsave(file="./4_results/05-3b_env_rating_raw_temp.png", device="png", plot=plot.b.t, width=9, height=5.5, units="in", dpi=600)	
#
#	## dewpoint depression
#	plot.b.d = ggplot(data.env.rat, aes(x=as.character(block), y=dpt_depress, color=treatment) ) +
#		geom_point(aes(shape=treatment), position=position_jitter(w=0.1) ) +
#		geom_line(aes(linetype=treatment, group=treatment), size=0.2) +
#		facet_grid(cols=vars(experiment, date_rating), rows=vars(period), scales="free_y" ) +
#		scale_shape_manual(values=c(0,1,2,5) ) +
#		theme_bw() +
#		theme(legend.position="bottom", legend.margin=margin(t=-5.5, b=0) ) +
#		labs(x="Block", y="Dewpoint depression (C)")
#	ggplot2::ggsave(file="./4_results/05-3b_env_rating_raw_dpt-depress.png", device="png", plot=plot.b.d, width=9, height=5.5, units="in", dpi=600)	

	## results: no trends consistent across blocks or rating dates
	

#######################################
# C. Rating Date - Visualize Analysis #
#######################################

### summarize
	## treatment x period
		# calculate average
		summ.env.rat.1 = data.env.rat %>% 
			group_by(experiment, treatment, period) %>%
			summarize(temp_avg=mean(temp_avg, na.rm=T), rh_avg=mean(rh_avg, na.rm=T), dpt_avg=mean(dpt_avg, na.rm=T), dpt_depress=mean(dpt_depress, na.rm=T) ) %>%
			ungroup()

		# round
		summ.env.rat.1 = summ.env.rat.1 %>% mutate(
			temp_avg=round(temp_avg, digits=2),
			rh_avg	=round(rh_avg, 	 digits=1),
			dpt_avg		=round(dpt_avg	  , digits=2),
			dpt_depress =round(dpt_depress, digits=2) )

	## treatment x rating date
		# calculate average
		summ.env.rat.2 = data.env.rat %>% 
			group_by(experiment, treatment, date_rating) %>%
			summarize(temp_avg=mean(temp_avg, na.rm=T), rh_avg=mean(rh_avg, na.rm=T), dpt_avg=mean(dpt_avg, na.rm=T), dpt_depress=mean(dpt_depress, na.rm=T) ) %>%
			ungroup()

		# round
		summ.env.rat.2 = summ.env.rat.2 %>% mutate(
			temp_avg=round(temp_avg, digits=2),
			rh_avg	=round(rh_avg, 	 digits=1),
			dpt_avg		=round(dpt_avg	  , digits=2),
			dpt_depress =round(dpt_depress, digits=2) )

### make annotation tibbles
	## relative humidity
		# treatment x period
			# slices
			rh1.other = tibble(
				experiment= c("A-2-row",	 "A-2-row", 	"B-3-row",	   	   "B-3-row", 		  "C-3-row",	   	   "C-3-row"),
				period 	  = c("day",		 "night", 		"day",		   	   "night", 		  "day",		   	   "night"),
				text_lab  = c("slice 0.2087","slice 0.6206","interaction n.s.","interaction n.s.","interaction n.s.","interaction n.s."),
				y_position= c(90,60,90,60,90,60) )

		# treatment x rating date
			# mean letters
			rh2.let = tibble(
				experiment = c("C-3-row","C-3-row","C-3-row","C-3-row"),
				date_rating= c("2018-11-28","2018-11-28","2018-11-28","2018-11-28"),
				treatment  = c("manual","blade","twine","control"),
				letter 	   = c("a","a","a","a"),
				y_position = c(90,90,90,90) )

			# slices
			rh2.other = tibble(
				experiment = c("A-2-row",	 	  "A-2-row", 		 "A-2-row",	   	    "B-3-row",	 	  "B-3-row", 		 "B-3-row",	   	   	 "C-3-row",		"C-3-row"),
				date_rating= c("2018-02-13",	  "2018-03-13", 	 "2018-04-11",		"2018-02-13",	  "2018-03-13", 	 "2018-04-11",		 "2018-10-17",	"2018-11-07"),
				text_lab   = c("interaction n.s.","interaction n.s.","interaction n.s.","interaction n.s.","interaction n.s.","interaction n.s.","slice 0.8115","slice 0.4542"),
				y_position = c(90,90,60,90,90,60,90,90) )

	## temperature
		# mean letters
		temp.let = tibble(
			experiment= c("A-2-row","A-2-row","A-2-row","A-2-row","B-3-row","B-3-row","B-3-row","B-3-row","C-3-row","C-3-row","C-3-row","C-3-row"),
			period 	  = c("day","day","day","day",				  "day","day","day","day",				  "day","day","day","day"),
			treatment = c("manual","twine","blade","control",	  "manual","twine","blade","control",	  "manual","twine","blade","control"),
			letter 	  = c("a","ab","bc","c",					  "a","ab","ab","b",					  "a","a","a","a"),
			y_position= c(9,9,9,9,								  9,9,9,9,								  9,9,9,9) )

		# slices
		temp.other = tibble(
			experiment= c("A-2-row",	 "B-3-row",		"C-3-row"),
			period 	  = c("night",		 "night",		"night"),
			text_lab  = c("slice 0.2473","slice 0.6974","slice 0.0788"),
			y_position= c(21,21,21) )

### prepare
	## names and orders
		# change order of treatments
		data.env.rat = data.env.rat %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))
		summ.env.rat.1 = summ.env.rat.1 %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))
		summ.env.rat.2 = summ.env.rat.2 %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))
		rh2.let = rh2.let %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))	
		temp.let = temp.let %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))	
	
	## graph settings
	source("./3_analysis/rcb_settings.r")	

### visualize - relative humidity
	# note: superseded by daily data, but retained to examine correlations with disease data

	## treatment x period
	plot.c.r1 = ggplot(data.env.rat) +
		geom_point(aes(x=treatment, y=rh_avg, shape=as.character(date_rating), color=as.character(date_rating) ), size=2 ) +
		geom_crossbar(data=summ.env.rat.1, aes(x=treatment, y=rh_avg, ymin=rh_avg, ymax=rh_avg), width=0.2, fatten=2, color="black") +
		geom_text(data=summ.env.rat.1, aes(x=treatment, y=rh_avg, label=rh_avg), size=2.5, color="black", hjust=-0.4) +
		geom_text(data=rh1.other, aes(y=y_position, label=text_lab), x=2.5, size=3) +
		facet_grid(rows=vars(period), cols=vars(experiment) ) +
		scale_x_discrete(expand=c(0.2,0.2) ) +
		scale_shape_manual(values=c(vec.rat.shape,vec.rat.shape) ) +
		scale_color_manual(values=c(vec.rat.color,vec.rat.color) ) +
		theme_bw() +
		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
		theme(legend.position="bottom", legend.margin=margin(t=-2.75), legend.spacing.y=unit(0, "lines") ) +
		guides(shape=guide_legend(nrow=2, byrow=TRUE), color=guide_legend(nrow=2, byrow=TRUE) ) +
		labs(x="Treatment", y="Relative humidity (%)", shape="rating date", color="rating date")
	ggplot2::ggsave(file="./4_results/z_05-3c_env_rating_stat-analysis_rh_trt-period_not-shown.png", device="png", plot=plot.c.r1, width=7, height=6.5, units="in", dpi=600)	

	## treatment x rating date (if one graph, facet_grid will produce blank panels
		# A-2-row, B-3-row
		plot.c.r2.ab = data.env.rat %>% filter(experiment %in% c("A-2-row","B-3-row") ) %>% {
		ggplot(.) +
			geom_point(aes(x=treatment, y=rh_avg, color=period, shape=period), size=2 ) +
			geom_crossbar(data={summ.env.rat.2 %>% filter(experiment %in% c("A-2-row","B-3-row") )}, aes(x=treatment, y=rh_avg, ymin=rh_avg, ymax=rh_avg), width=0.2, fatten=2, color="black") +
			geom_text(data={summ.env.rat.2 %>% filter(experiment %in% c("A-2-row","B-3-row") )}, aes(x=treatment, y=rh_avg, label=rh_avg), size=2.5, color="black", hjust=-0.4) +
			geom_text(data={rh2.other %>% filter(experiment %in% c("A-2-row","B-3-row") ) }, aes(y=y_position, label=text_lab), x=2.5, size=3) +
			facet_grid(rows=vars(date_rating), cols=vars(experiment) ) +
			scale_x_discrete(expand=c(0.2,0.2) ) +
			scale_shape_manual(values=vec.per.shape ) +
			scale_color_manual(values=vec.per.color ) +
			theme_bw() +
			theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
			theme(legend.position="bottom", legend.margin=margin(t=-2.75), legend.spacing.y=unit(0, "lines") ) +
#			guides(shape=guide_legend(nrow=2, byrow=TRUE), color=guide_legend(nrow=2, byrow=TRUE) ) +
			labs(x="Treatment", y="Relative humidity (%)", shape="rating date", color="rating date")
		}

		# C-3-row
		plot.c.r2.c = data.env.rat %>% filter(experiment == "C-3-row") %>% {
		ggplot(.) +
			geom_point(aes(x=treatment, y=rh_avg, color=period, shape=period), size=2 ) +
			geom_crossbar(data={summ.env.rat.2 %>% filter(experiment == "C-3-row")}, aes(x=treatment, y=rh_avg, ymin=rh_avg, ymax=rh_avg), width=0.2, fatten=2, color="black") +
			geom_text(data={summ.env.rat.2 %>% filter(experiment == "C-3-row")}, aes(x=treatment, y=rh_avg, label=rh_avg), size=2.5, color="black", hjust=-0.4) +
			geom_text(data={rh2.let %>% filter(experiment == "C-3-row") }, aes(x=treatment, y=y_position, label=letter), size=3) +
			geom_text(data={rh2.other %>% filter(experiment == "C-3-row") }, aes(y=y_position, label=text_lab), x=2.5, size=3) +
			facet_grid(rows=vars(date_rating), cols=vars(experiment) ) +
			scale_x_discrete(expand=c(0.2,0.2) ) +
			scale_shape_manual(values=vec.per.shape ) +
			scale_color_manual(values=vec.per.color ) +
			theme_bw() +
			theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
			theme(axis.title.y=element_blank(), axis.text.y=element_blank() ) +
			theme(legend.position="bottom", legend.margin=margin(t=-2.75), legend.spacing.y=unit(0, "lines") ) +
#			guides(shape=guide_legend(nrow=2, byrow=TRUE), color=guide_legend(nrow=2, byrow=TRUE) ) +
			labs(x="Treatment", y="Relative humidity (%)", shape="rating date", color="rating date")
		}
		
		plot.c.r2 = wrap_plots(plot.c.r2.ab, plot.c.r2.c, nrow=1, widths=c(2.1,1), guides="collect") +
			plot_annotation(theme=theme(legend.position="bottom"))
		
		ggplot2::ggsave(file="./4_results/z_05-3c_env_rating_stat-analysis_rh_trt-ratingdate_not-shown.png", device="png", plot=plot.c.r2, width=7, height=6.5, units="in", dpi=600)	

### visualize - temperature
	# note: commented out because superseded by daily data

	## treatment x period
	plot.c.t = ggplot(data.env.rat) +
		geom_point(aes(x=treatment, y=temp_avg, shape=as.character(date_rating), color=as.character(date_rating) ), size=2 ) +
		geom_crossbar(data=summ.env.rat.1, aes(x=treatment, y=temp_avg, ymin=temp_avg, ymax=temp_avg), width=0.2, fatten=2, color="black") +
		geom_text(data=summ.env.rat.1, aes(x=treatment, y=temp_avg, label=temp_avg), size=2.5, color="black", hjust=-0.4) +
		geom_text(data=temp.let, aes(x=treatment, y=y_position, label=letter), size=3) +
		geom_text(data=temp.other, aes(y=y_position, label=text_lab), x=2.5, size=3) +
		facet_grid(rows=vars(period), cols=vars(experiment) ) +
		scale_x_discrete(expand=c(0.2,0.2) ) +
		scale_shape_manual(values=c(vec.rat.shape,vec.rat.shape) ) +
		scale_color_manual(values=c(vec.rat.color,vec.rat.color) ) +
		theme_bw() +
		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
		theme(legend.position="bottom", legend.margin=margin(t=-2.75), legend.spacing.y=unit(0, "lines") ) +
		guides(shape=guide_legend(nrow=2, byrow=TRUE), color=guide_legend(nrow=2, byrow=TRUE) ) +
		labs(x="Treatment", y="Temperature (C)", shape="rating date", color="rating date")
	ggplot2::ggsave(file="./4_results/z_05-3c_env_rating_stat-analysis_temp_not-shown.png", device="png", plot=plot.c.t, width=7, height=6.5, units="in", dpi=600)	

	