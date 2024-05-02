#####################################
# RASPBERRY Cane Botrytis 			#
# Multivariate 						#
# Rating dates						#
# Visualize							#
#####################################
# organize data for exploratory correlation analyses among disease, environment, canes

# NOTE: not used, very little added to story not shown by end data

## built on Docker putmanlab/exploratory-analysis:420.0

#library(conflicted)
#
#library(dplyr)
#library(forcats)
#library(ggplot2)
#library(readr)
#library(stringr)
#library(tidyr)
#
#library(patchwork) # for combining plots: wrap_plots
#library(rstatix) # for correlation statistics: cor_test
#
## install ggpmisc for printing line equation, R-squared via stat_poly_eq(); after dependencies ggpp (after dependency xts), confintr, lmodel2
#remotes::install_version("xts", version="0.12-0", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
#remotes::install_version("ggpp", version="0.4.5", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
#remotes::install_version("confintr", version="0.1.2", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
#remotes::install_version("lmodel2", version="1.7-3", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
#remotes::install_version("ggpmisc", version="0.5.1", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
#library(ggpmisc)
#
#conflict_prefer("date", "lubridate")
#conflict_prefer("filter", "dplyr")
#conflict_prefer("lag", "dplyr")
#conflict_prefer("spread", "tidyr")
#
#setwd("/home/raspb_botrytis")
#
#source("./3_analysis/rcb_functions.r")
#
#
##############
## A. Import #
##############
#
#### upload
#	data.canedis = read_csv(file="./2_data_curated/rasp-caneb_08_multivar_rating_cane-dis.csv", col_names=T, na=c("","."))
#	data.caneenv = read_csv(file="./2_data_curated/rasp-caneb_08_multivar_rating_cane-env.csv", col_names=T, na=c("","."))
#	data.envdis = read_csv(file="./2_data_curated/rasp-caneb_08_multivar_rating_env-dis.csv", col_names=T, na=c("","."))
#	data.rowdis = read_csv(file="./2_data_curated/rasp-caneb_08_multivar_rating_row-dis.csv", col_names=T, na=c("","."))
#	data.rowenv = read_csv(file="./2_data_curated/rasp-caneb_08_multivar_rating_row-env.csv", col_names=T, na=c("","."))
#
#### filter (variables selected from z_not-shown_08-2_multivar_org_rating.r section D
#	data.caneenv = data.caneenv %>% filter(var_env %in% c("lwe_rh80","lwe_rh90","rh_day","rh_night","temp_day","temp_night","temp_time1525","temprh_rh90") )
#	data.envdis = data.envdis %>% filter(var_env %in% c("lwe_rh80","lwe_rh90","rh_day","rh_night","temp_day","temp_night","temp_time1525","temprh_rh90") )
#	data.rowenv = data.rowenv %>% filter(var_env %in% c("lwe_rh80","lwe_rh90","rh_day","rh_night","temp_day","temp_night","temp_time1525","temprh_rh90") )
#
#### change column type
#	data.rowdis = data.rowdis %>% mutate(plot_row=as.character(plot_row) )
#	data.rowenv = data.rowenv %>% mutate(plot_row=as.character(plot_row) )
#
#### settings for graphs (use different shapes/colors for experiments, treatments)
#	vec.exp.shape = c(1,0,2)
#	vec.trt.shape = c(5,6,7,8)
#	vec.rat.shape = c(10,11,13)
#
#	vec.exp.color = scales::hue_pal()(8)[c(1,5,7)]
#	vec.trt.color = scales::hue_pal()(8)[c(2,4,6,8)]
#	vec.rat.color = c(scales::hue_pal()(8)[3],"black","grey33")
#
#### convert date_rating_alt to character for graphing
#	data.canedis = data.canedis %>% mutate(date_rating_alt=as.character(date_rating_alt) )
#	data.caneenv = data.caneenv %>% mutate(date_rating_alt=as.character(date_rating_alt) )
#	data.envdis = data.envdis %>% mutate(date_rating_alt=as.character(date_rating_alt) )
#	data.rowdis = data.rowdis %>% mutate(date_rating_alt=as.character(date_rating_alt) )
#	data.rowenv = data.rowenv %>% mutate(date_rating_alt=as.character(date_rating_alt) )
#
#
##################################################
## B. Correlate - Across Experiments, Treatments #
##################################################
#
#### set aesthetic settings for this section; factor (column) and vector of values
#	shape.b.factor="date_rating_alt"
#	shape.b.vector=vec.rat.shape
#	color.b.factor="experiment"
#	color.b.vector=vec.exp.color
#
#### cane-dis
#	p.b1 = ggplot(data.canedis, aes(x=canes, y=value_dis_chg) ) +
#		geom_point(aes(shape=.data[[shape.b.factor]], color=.data[[color.b.factor]] ), size=1) +
#		stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#		stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.93 ) +
#		facet_grid(rows=vars(var_dis), scales="free_y") +
#		scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#		scale_shape_manual(values=shape.b.vector) +
#		scale_color_manual(values=color.b.vector) +
#		theme_bw() +
# 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#		theme(legend.position="right", legend.margin=margin(l=-2.75) )
#
#	ggplot2::ggsave(file="./4_results/z_09-2_rating_b1_cane-dis_across_exp-ABC_not-shown.png", device="png", plot=p.b1, width=4, height=5.5, units="in", dpi=600)
#
#		# results
#			# incid: signif but is spurious due to clustering between A/B (3rd date), C
#			# severity: not signif
#		# conclusion: due to experimental design cannot say the correlation means anything
#			
#### cane-env
#	## set up function
#	plot.b2 = function(df.x, vec.env, var.shape, vec.shape, var.color, vec.color) {
#		## filter
#		df.x = df.x %>% filter(var_env %in% vec.env) 
#		## plot
#		p = ggplot(df.x, aes(x=canes, y=value_env) ) +
#			geom_point(aes(shape=.data[[var.shape]], color=.data[[var.color]] ), size=1) +
#			stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#			stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.92 ) +
#			facet_grid(rows=vars(var_env), scales="free_y") +
#			scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#			scale_shape_manual(values=vec.shape) +
#			scale_color_manual(values=vec.color) +
#			theme_bw() +
#	 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#			theme(legend.position="right", legend.margin=margin(l=-2.75) )
#		## send plot as output
#		return(p)
#	}
#	
#	p.b2.1 = plot.b2(df.x=data.caneenv, vec.env=c("lwe_rh80","lwe_rh90","rh_day","rh_night"), var.shape=shape.b.factor, vec.shape=shape.b.vector, var.color=color.b.factor, vec.color=color.b.vector)
#	p.b2.2 = plot.b2(df.x=data.caneenv, vec.env=c("temp_day","temp_night","temp_time1525","temprh_rh90"), var.shape=shape.b.factor, vec.shape=shape.b.vector, var.color=color.b.factor, vec.color=color.b.vector)
#
#	p.b2 = wrap_plots(p.b2.1, p.b2.2, nrow=1, guides="collect") + 
#		plot_annotation(theme=theme(legend.position="right"))
#
#	ggplot2::ggsave(file="./4_results/z_09-2_rating_b2_cane-env_across_exp-ABC_not-shown.png", device="png", plot=p.b2, width=6, height=8.5, units="in", dpi=600)
#
#		# results: all except lwe_rh80, lwe_rh90, rh_night are signif, but all are spurious due to clustering
#		# conclusions: due to experimental design cannot say the correlation means anything
#					
#### env-dis
#	p.b3 = ggplot(data.envdis, aes(x=value_env, y=value_dis_chg) ) +
#		geom_point(aes(shape=.data[[shape.b.factor]], color=.data[[color.b.factor]] ), size=1) +
#		stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#		stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.91 ) +
#		facet_grid(rows=vars(var_dis), cols=vars(var_env), scales="free") +
#		scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#		scale_shape_manual(values=shape.b.vector) +
#		scale_color_manual(values=color.b.vector) +
#		theme_bw() +
# 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#		theme(legend.position="bottom", legend.margin=margin(t=-2.75) )
#
#	ggplot2::ggsave(file="./4_results/z_09-2_rating_b3_env-dis_across_exp-ABC_not-shown.png", device="png", plot=p.b3, width=12, height=4.5, units="in", dpi=600)
#
#		# results
#			# incid: all vars except temp_night, temprh_rh90 signif, but are due to clustering between A/B date 3 vs others
#			# severity
#				# lwe_rh80, lwe_rh90, rh_day, rh_night: all signif; clustering by rating date apparent
#				# temp-based: all not signif
#		# conclusion: due to experimental design cannot say the correlation means anything
#
#### row-dis
#	p.b4 = ggplot(data.rowdis, aes(x=plot_row, y=value_dis_chg) ) +
#		geom_point(aes(shape=.data[[shape.b.factor]], color=.data[[color.b.factor]] ), size=1, position=position_jitter(w=0.05) ) +
#		facet_grid(rows=vars(var_dis), scales="free_y") +
#		scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#		scale_shape_manual(values=shape.b.vector) +
#		scale_color_manual(values=color.b.vector) +
#		theme_bw() +
# 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#		theme(legend.position="right", legend.margin=margin(l=-2.75) )
#	
#	ggplot2::ggsave(file="./4_results/z_09-2_rating_b4_row-dis_across_exp-ABC_not-shown.png", device="png", plot=p.b4, width=4, height=5.5, units="in", dpi=600)
#
#### row-env
#	## set up function
#	plot.b5 = function(df.x, vec.env, var.shape, vec.shape, var.color, vec.color) {
#		## filter
#		df.x = df.x %>% filter(var_env %in% vec.env) 
#		## plot
#		p = ggplot(df.x, aes(x=plot_row, y=value_env) ) +
#			geom_point(aes(shape=.data[[shape.b.factor]], color=.data[[color.b.factor]] ), size=1, position=position_jitter(w=0.05) ) +
#			stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#			stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.92 ) +
#			facet_grid(rows=vars(var_env), scales="free_y") +
#			scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#			scale_shape_manual(values=vec.shape) +
#			scale_color_manual(values=vec.color) +
#			theme_bw() +
#	 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#			theme(legend.position="right", legend.margin=margin(l=-2.75) )
#		## send plot as output
#		return(p)
#	}
#	
#	p.b5.1 = plot.b5(df.x=data.rowenv, vec.env=c("lwe_rh80","lwe_rh90","rh_day","rh_night"), var.shape=shape.b.factor, vec.shape=shape.b.vector, var.color=color.b.factor, vec.color=color.b.vector)
#	p.b5.2 = plot.b5(df.x=data.rowenv, vec.env=c("temp_day","temp_night","temp_time1525","temprh_rh90"), var.shape=shape.b.factor, vec.shape=shape.b.vector, var.color=color.b.factor, vec.color=color.b.vector)
#
#	p.b5 = wrap_plots(p.b5.1, p.b5.2, nrow=1, guides="collect") + 
#		plot_annotation(theme=theme(legend.position="right"))
#
#	ggplot2::ggsave(file="./4_results/z_09-2_rating_b5_row-env_across_exp-ABC_not-shown.png", device="png", plot=p.b5, width=6, height=8.5, units="in", dpi=600)
#
#
####################################################
## C. Correlate - By Experiment, Across Treatments #
####################################################
#
#### set aesthetic settings for this section; factor (column) and vector of values
#	shape.c.factor="date_rating_alt"
#	shape.c.vector=vec.rat.shape
#	color.c.factor="treatment"
#	color.c.vector=vec.trt.color
#
#### cane-dis
#	p.c1 = ggplot(data.canedis, aes(x=canes, y=value_dis_chg) ) +
#		geom_point(aes(shape=.data[[shape.c.factor]], color=.data[[color.c.factor]] ), size=1) +
#		stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#		stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.93 ) +
#		facet_grid(rows=vars(var_dis), cols=vars(experiment), scales="free_y") +
#		scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#		scale_shape_manual(values=shape.c.vector) +
#		scale_color_manual(values=color.c.vector) +
#		theme_bw() +
# 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#		theme(legend.position="right", legend.margin=margin(l=-2.75) )
#
#	ggplot2::ggsave(file="./4_results/z_09-2_rating_c1_cane-dis_by-exp_exp-ABC_not-shown.png", device="png", plot=p.c1, width=9, height=5.5, units="in", dpi=600)
#
#		# results: no signif correlations
#		
#### cane-env
#	## set up function
#	plot.c2 = function(df.x, vec.env, var.shape, vec.shape, var.color, vec.color) {
#		## filter
#		df.x = df.x %>% filter(var_env %in% vec.env) 
#		## plot
#		p = ggplot(df.x, aes(x=canes, y=value_env) ) +
#			geom_point(aes(shape=.data[[var.shape]], color=.data[[var.color]] ), size=1) +
#			stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#			stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.92 ) +
#			facet_grid(rows=vars(var_env), cols=vars(experiment), scales="free_y") +
#			scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#			scale_shape_manual(values=vec.shape) +
#			scale_color_manual(values=vec.color) +
#			theme_bw() +
#	 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#			theme(legend.position="right", legend.margin=margin(l=-2.75) )
#		## send plot as output
#		return(p)
#	}
#	
#	p.c2.1 = plot.c2(df.x=data.caneenv, vec.env=c("lwe_rh80","lwe_rh90","rh_day","rh_night"), var.shape=shape.c.factor, vec.shape=shape.c.vector, var.color=color.c.factor, vec.color=color.c.vector)
#	p.c2.2 = plot.c2(df.x=data.caneenv, vec.env=c("temp_day","temp_night","temp_time1525","temprh_rh90"), var.shape=shape.c.factor, vec.shape=shape.c.vector, var.color=color.c.factor, vec.color=color.c.vector)
#
#	p.c2 = wrap_plots(p.c2.1, p.c2.2, nrow=1, guides="collect") + 
#		plot_annotation(theme=theme(legend.position="right"))
#
#	ggplot2::ggsave(file="./4_results/z_09-2_rating_c2_cane-env_by-exp_exp-ABC_not-shown.png", device="png", plot=p.c2, width=14, height=7.4, units="in", dpi=600)
#
#		# results
#			# C-3-row rh_day: signif positive correlation, appears genuine
#		# conclusion: see if correlations hold when examined by experiment+treatment
#			# C-3-row rh_day: only twine signif, appears to be due to outliers
#		
#### env-dis
#	## set up function
#	plot.c3 = function(df.x, var.dis, var.shape, vec.shape, var.color, vec.color) {
#		## convert to wide
#		df.x = df.x %>% spread(key=var_dis, value=value_dis_chg)
#		## plot
#		p = ggplot(df.x, aes(x=value_env, y=.data[[var.dis]] ) ) +
#			geom_point(aes(shape=.data[[var.shape]], color=.data[[var.color]] ), size=1) +
#			stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#			stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.91 ) +
#			facet_grid(rows=vars(experiment), cols=vars(var_env), scales="free_x") +
#			scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#			scale_shape_manual(values=vec.shape) +
#			scale_color_manual(values=vec.color) +
#			theme_bw() +
#	 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#			theme(legend.position="bottom", legend.margin=margin(t=-2.75) )
#		## set filename var
#			   if (var.dis == "incid") 			   { var.dis = "incidence"
#		} else if (var.dis == "length_mean_plots") { var.dis = "severity"
#		} else { print('Invalid var.dis') }
#		## save
#		ggplot2::ggsave(file=paste("./4_results/z_09-2_rating_c3_env-dis_by-exp_exp-ABC_", var.dis, "_not-shown.png", sep=""), 
#			device="png", plot=p, width=12, height=4.5, units="in", dpi=600)
#	}
#
#	plot.c3(df.x=data.envdis, var.dis="incid", var.shape=shape.c.factor, vec.shape=shape.c.vector, var.color=color.c.factor, vec.color=color.c.vector)
#	plot.c3(df.x=data.envdis, var.dis="length_mean_plots", var.shape=shape.c.factor, vec.shape=shape.c.vector, var.color=color.c.factor, vec.color=color.c.vector)
#		
#		# results
#			# incid: almost all are signif but there is strong clustering between rating 3 vs 1/2
#			# severity: almost all are signif but there is strong clustering between rating 3 vs 1/2
#				# except for C-3-row lwe_rh90
#
#### row-dis
#	p.c4 = ggplot(data.rowdis, aes(x=plot_row, y=value_dis_chg) ) +
#		geom_point(aes(shape=.data[[shape.b.factor]], color=.data[[color.b.factor]] ), size=1, position=position_jitter(w=0.05) ) +
#		facet_grid(rows=vars(var_dis), cols=vars(experiment), scales="free_y") +
#		scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#		scale_shape_manual(values=shape.c.vector) +
#		scale_color_manual(values=color.c.vector) +
#		theme_bw() +
# 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#		theme(legend.position="right", legend.margin=margin(l=-2.75) )
#
#	ggplot2::ggsave(file="./4_results/z_09-2_rating_c4_row-dis_by-exp_exp-ABC_not-shown.png", device="png", plot=p.c4, width=9, height=5.5, units="in", dpi=600)
#
#### row-env
#	## set up function
#	plot.c5 = function(df.x, vec.env, var.shape, vec.shape, var.color, vec.color) {
#		## filter
#		df.x = df.x %>% filter(var_env %in% vec.env) 
#		## plot
#		p = ggplot(df.x, aes(x=plot_row, y=value_env) ) +
#			geom_point(aes(shape=.data[[shape.b.factor]], color=.data[[color.b.factor]] ), size=1, position=position_jitter(w=0.05) ) +
#			facet_grid(rows=vars(var_env), cols=vars(experiment), scales="free_y") +
#			scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#			scale_shape_manual(values=vec.shape) +
#			scale_color_manual(values=vec.color) +
#			theme_bw() +
#	 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#			theme(legend.position="right", legend.margin=margin(l=-2.75) )
#		## send plot as output
#		return(p)
#	}
#	
#	p.c5.1 = plot.c5(df.x=data.rowenv, vec.env=c("lwe_rh80","lwe_rh90","rh_day","rh_night"), var.shape=shape.c.factor, vec.shape=shape.c.vector, var.color=color.c.factor, vec.color=color.c.vector)
#	p.c5.2 = plot.c5(df.x=data.rowenv, vec.env=c("temp_day","temp_night","temp_time1525","temprh_rh90"), var.shape=shape.c.factor, vec.shape=shape.c.vector, var.color=color.c.factor, vec.color=color.c.vector)
#
#	p.c5 = wrap_plots(p.c5.1, p.c5.2, nrow=1, guides="collect") + 
#		plot_annotation(theme=theme(legend.position="right"))
#
#	ggplot2::ggsave(file="./4_results/z_09-2_rating_c5_row-env_by-exp_exp-ABC_not-shown.png", device="png", plot=p.c5, width=14, height=7.4, units="in", dpi=600)
#			
#
####################################################
## D. Correlate - By Treatment, Across Experiments #
####################################################
#
#### set aesthetic settings for this section; factor (column) and vector of values
#	shape.d.factor="date_rating_alt"
#	shape.d.vector=vec.rat.shape
#	color.d.factor="experiment"
#	color.d.vector=vec.exp.color
#
#### cane-dis
#	p.d1 = ggplot(data.canedis, aes(x=canes, y=value_dis_chg) ) +
#		geom_point(aes(shape=.data[[shape.d.factor]], color=.data[[color.d.factor]] ), size=1) +
#		stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#		stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.93 ) +
#		facet_grid(rows=vars(var_dis), cols=vars(treatment), scales="free_y") +
#		scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#		scale_shape_manual(values=shape.d.vector) +
#		scale_color_manual(values=color.d.vector) +
#		theme_bw() +
# 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#		theme(legend.position="right", legend.margin=margin(l=-2.75) )
#
#	ggplot2::ggsave(file="./4_results/z_09-2_rating_d1_cane-dis_by-trt_exp-ABC_not-shown.png", device="png", plot=p.d1, width=9, height=5, units="in", dpi=600)
#
#		# results
#			# incid: weak control and manual; but appears due to clustering between A/B date 3 vs. rest
#			# severity: control only signif, but appears due to outlier
#		# conclusion: due to experimental design cannot say the correlation means anything
#			
#### cane-env
#	## set up function
#	plot.d2 = function(df.x, vec.env, var.shape, vec.shape, var.color, vec.color) {
#		## filter
#		df.x = df.x %>% filter(var_env %in% vec.env) 
#		## plot
#		p = ggplot(df.x, aes(x=canes, y=value_env) ) +
#			geom_point(aes(shape=.data[[var.shape]], color=.data[[var.color]] ), size=1) +
#			stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#			stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.92 ) +
#			facet_grid(rows=vars(var_env), cols=vars(treatment), scales="free_y") +
#			scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#			scale_shape_manual(values=vec.shape) +
#			scale_color_manual(values=vec.color) +
#			theme_bw() +
#	 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#			theme(legend.position="right", legend.margin=margin(l=-2.75) )
#		## send plot as output
#		return(p)
#	}
#	
#	p.d2.1 = plot.d2(df.x=data.caneenv, vec.env=c("lwe_rh80","lwe_rh90","rh_day","rh_night"), var.shape=shape.d.factor, vec.shape=shape.d.vector, var.color=color.d.factor, vec.color=color.d.vector)
#	p.d2.2 = plot.d2(df.x=data.caneenv, vec.env=c("temp_day","temp_night","temp_time1525","temprh_rh90"), var.shape=shape.d.factor, vec.shape=shape.d.vector, var.color=color.d.factor, vec.color=color.d.vector)
#
#	p.d2 = wrap_plots(p.d2.1, p.d2.2, nrow=1, guides="collect") + 
#		plot_annotation(theme=theme(legend.position="right"))
#
#	ggplot2::ggsave(file="./4_results/z_09-2_rating_d2_cane-env_by-trt_exp-ABC_not-shown.png", device="png", plot=p.d2, width=14, height=7.4, units="in", dpi=600)
#	
#		# results
#			# rh-based (lwe_, rh_) all not signif
#			# temp-based (temp_, temprh_): all signif (temprh_rh90 weak), but all are clustered with little variation
#		# conclusion: due to clustering and experimental design cannot say the correlation means anything
#
#### env-dis
#	## set up function
#	plot.d3 = function(df.x, var.dis, var.shape, vec.shape, var.color, vec.color) {
#		## convert to wide
#		df.x = df.x %>% spread(key=var_dis, value=value_dis_chg)
#		## plot
#		p = ggplot(df.x, aes(x=value_env, y=.data[[var.dis]] ) ) +
#			geom_point(aes(shape=.data[[var.shape]], color=.data[[var.color]] ), size=1) +
#			stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#			stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.91 ) +
#			facet_grid(rows=vars(treatment), cols=vars(var_env), scales="free_x") +
#			scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#			scale_shape_manual(values=vec.shape) +
#			scale_color_manual(values=vec.color) +
#			theme_bw() +
#	 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#			theme(legend.position="bottom", legend.margin=margin(t=-2.75) )
#		## set filename var
#			   if (var.dis == "incid") 			   { var.dis = "incidence"
#		} else if (var.dis == "length_mean_plots") { var.dis = "severity"
#		} else { print('Invalid var.dis') }
#		## save
#		ggplot2::ggsave(file=paste("./4_results/z_09-2_rating_d3_env-dis_by-trt_exp-ABC_", var.dis, "_not-shown.png", sep=""), 
#			device="png", plot=p, width=12, height=8, units="in", dpi=600)
#	}
#
#	plot.d3(df.x=data.envdis, var.dis="incid", var.shape=shape.d.factor, vec.shape=shape.d.vector, var.color=color.d.factor, vec.color=color.d.vector)
#	plot.d3(df.x=data.envdis, var.dis="length_mean_plots", var.shape=shape.d.factor, vec.shape=shape.d.vector, var.color=color.d.factor, vec.color=color.d.vector)
#
#		# results
#			# incid: numerous signif correlations (all 4 rh-based), but all are due to clustering between A/B vs C
#			# severity: numerous signif correlations (all 4 rh-based), clustering is not as strong
#		# conclusion: due to experimental design cannot say the correlation means anything
#
#
############################################
## E. Correlate - By Experiment, Treatment #
############################################
#
#### set aesthetic settings for this section; factor (column) and vector of values
#	shape.e.factor="date_rating_alt"
#	shape.e.vector=vec.rat.shape
#	color.e.factor="date_rating_alt"
#	color.e.vector=vec.rat.color
#
#### cane-dis
#	## set up function
#	plot.e1 = function(df.x, var.dis, var.shape, vec.shape, var.color, vec.color) {
#		## convert to wide
#		df.x = df.x %>% spread(key=var_dis, value=value_dis_chg)
#		## plot
#		p = ggplot(df.x, aes(x=canes, y=.data[[var.dis]] ) ) +
#			geom_point(aes(shape=.data[[var.shape]], color=.data[[var.color]] ), size=1) +
#			stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#			stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.91 ) +
#			facet_grid(rows=vars(experiment), cols=vars(treatment)) +
#			scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#			scale_shape_manual(values=vec.shape) +
#			scale_color_manual(values=vec.color) +
#			theme_bw() +
#	 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#			theme(legend.position="bottom", legend.margin=margin(t=-2.75) )
#		## send plot as output
#		return(p)
#	}
#	
#	p.e1.1 = plot.e1(df.x=data.canedis, var.dis="incid", var.shape=shape.e.factor, vec.shape=shape.e.vector, var.color=color.e.factor, vec.color=color.e.vector)
#	p.e1.2 = plot.e1(df.x=data.canedis, var.dis="length_mean_plots", var.shape=shape.e.factor, vec.shape=shape.e.vector, var.color=color.e.factor, vec.color=color.e.vector)
#
#	p.e1 = wrap_plots(p.e1.1, p.e1.2, nrow=1, guides="collect") + 
#		plot_annotation(theme=theme(legend.position="bottom"))
#		
#	ggplot2::ggsave(file="./4_results/z_09-2_rating_e1_cane-dis_by-exp-trt_exp-ABC_not-shown.png", device="png", plot=p.e1, width=12, height=6, units="in", dpi=600)
#
#		# results: none are signif
#			
#### cane-env
#	## set up function
#	plot.e2 = function(df.x, var.exp, var.shape, vec.shape, var.color, vec.color) {
#		# set plot function 
#		subplot.e2 = function(df.x, vec.env, var.shape, vec.shape, var.color, vec.color) {
#			# filter
#			df.x = df.x %>% filter(var_env %in% vec.env) 
#			# plot
#			sp = ggplot(df.x, aes(x=canes, y=value_env) ) +
#				geom_point(aes(shape=.data[[var.shape]], color=.data[[var.color]] ), size=1) +
#				stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#				stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.92 ) +
#				facet_grid(rows=vars(var_env), cols=vars(treatment), scales="free_y") +
#				scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#				scale_shape_manual(values=vec.shape) +
#				scale_color_manual(values=vec.color) +
#				theme_bw() +
#		 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#				theme(legend.position="right", legend.margin=margin(t=-2.75) )
#			# send plot as output
#			return(sp)
#		}
#		# filter for experiment
#		df.x = df.x %>% filter(experiment == var.exp)
#		# make individual panels
#		p.1 = subplot.e2(df.x=df.x, vec.env=c("lwe_rh80","lwe_rh90","rh_day","rh_night"), var.shape=var.shape, vec.shape=vec.shape, var.color=var.color, vec.color=vec.color)
#		p.2 = subplot.e2(df.x=df.x, vec.env=c("temp_day","temp_night","temp_time1525","temprh_rh90"), var.shape=var.shape, vec.shape=vec.shape, var.color=var.color, vec.color=vec.color )
#		# combine
#		p = wrap_plots(p.1, p.2, nrow=1, guides="collect") + 
#			plot_annotation(theme=theme(legend.position="right"))
#		# set filename var
#		var.exp = var.exp %>% str_replace_all(c("-2-row" = "","-3-row" = "") )
#		# save plot
#		ggplot2::ggsave(file=paste("./4_results/z_09-2_rating_e2_cane-env_by-exp-trt_exp-", var.exp, "_not-shown.png", sep=""),
#			device="png", plot=p, width=14, height=7.4, units="in", dpi=600)
#	}
#
#	plot.e2(df.x=data.caneenv, var.exp="A-2-row", var.shape=shape.e.factor, vec.shape=shape.e.vector, var.color=color.e.factor, vec.color=color.e.vector)
#	plot.e2(df.x=data.caneenv, var.exp="B-3-row", var.shape=shape.e.factor, vec.shape=shape.e.vector, var.color=color.e.factor, vec.color=color.e.vector)
#	plot.e2(df.x=data.caneenv, var.exp="C-3-row", var.shape=shape.e.factor, vec.shape=shape.e.vector, var.color=color.e.factor, vec.color=color.e.vector)
#	
#		# results
#			# A-2-row: none signif
#			# B-3-row: none signif
#			# C-3-row: rh_day for twine only but looks like due to outliers; rest not signif
#
#### env-dis
#	## set up function
#	plot.e3 = function(df.x, var.dis, var.shape, vec.shape, var.color, vec.color) {
#		# set plot function
#		plot.exp.e3 = function(df.x, var.exp, var.dis, var.shape, vec.shape, var.color, vec.color) {
#			# filter for experiment
#			df.x = df.x %>% filter(experiment == var.exp)
#			# convert to wide
#			df.x = df.x %>% spread(key=var_dis, value=value_dis_chg)
#			# plot
#			p = ggplot(df.x, aes(x=value_env, y=.data[[var.dis]] ) ) +
#				geom_point(aes(shape=.data[[var.shape]], color=.data[[var.color]] ), size=1) +
#				stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#				stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.91 ) +
#				facet_grid(rows=vars(treatment), cols=vars(var_env), scales="free_x") +
#				scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#				scale_shape_manual(values=vec.shape) +
#				scale_color_manual(values=vec.color) +
#				theme_bw() +
#		 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#				theme(legend.position="bottom", legend.margin=margin(t=-2.75) )
#			# set filename variables
#				# experiment
#				var.exp = var.exp %>% str_replace_all(c("-2-row" = "","-3-row" = "") )
#				# disease variable
#					   if (var.dis == "incid") 			   { var.dis = "incidence"
#				} else if (var.dis == "length_mean_plots") { var.dis = "severity"
#				} else { print('Invalid var.dis') }
#			# save
#			ggplot2::ggsave(file=paste("./4_results/z_09-2_rating_e3_env-dis_by-exp-trt_", var.dis, "_exp-", var.exp, "_not-shown.png", sep=""), 
#				device="png", plot=p, width=12, height=8, units="in", dpi=600)
#		}
#		# run for each experiment
#		plot.exp.e3(df.x=df.x, var.exp="A-2-row", var.dis=var.dis, var.shape=var.shape, vec.shape=vec.shape, var.color=var.color, vec.color=vec.color)
#		plot.exp.e3(df.x=df.x, var.exp="B-3-row", var.dis=var.dis, var.shape=var.shape, vec.shape=vec.shape, var.color=var.color, vec.color=vec.color)
#		plot.exp.e3(df.x=df.x, var.exp="C-3-row", var.dis=var.dis, var.shape=var.shape, vec.shape=vec.shape, var.color=var.color, vec.color=vec.color)	
#	}
#
#	plot.e3(df.x=data.envdis, var.dis="incid", var.shape=shape.e.factor, vec.shape=shape.e.vector, var.color=color.e.factor, vec.color=color.e.vector)
#	plot.e3(df.x=data.envdis, var.dis="length_mean_plots", var.shape=shape.e.factor, vec.shape=shape.e.vector, var.color=color.e.factor, vec.color=color.e.vector)
#
#		# results incidence
#			# A-2-row, B-3-row: all 4 rh-based, some temp-based are signif but due to strong clustering between date 3 vs date 1/2
#			# C-3-row: fewer treatments are signif, clustering apparent but not as strong
#		# results severity
#			# A-2-row: all 4 rh-based, some temp-based are signif but due to strong clustering between date 3 vs date 1/2
#			# B-3-row: most 4 rh-based, some temp-based are signif but due to strong clustering between date 3 vs date 1/2
#			# C-3-row: lwe_rh90 looks more like a real correlation
#		# conclusions: due to experimental design cannot say the correlation means anything
#
#
############################################
## F. Correlate - By Experiment, Treatment #
############################################
## for figure
#
#### set aesthetic settings for this section; factor (column) and vector of values
#	shape.f.factor="date_rating_alt"
#	shape.f.vector=c(1,0,2)
#	color.f.factor="date_rating_alt"
#	color.f.vector=scales::hue_pal()(8)[c(1,5,7)]
#
#	## set strip labels
#	facet.lab.vardis = c("incid"="Incidence (%)", "length_mean_plots"="Lesion length (cm)")
#	facet.lab.exp = c('A-2-row'="Ranch 1 2-row", 'B-3-row'="Ranch 1 3-row", 'C-3-row'="Ranch 2 3-row")
#	facet.lab.varenv = c(
#		"lwe_rh80"="hr of RH>80%", "lwe_rh90"="hr of RH>90%", "rh_day"="%RH daylight", "rh_night"="%RH night",
#		"temp_day"="Temp (C) day", "temp_night"="Temp (C) night", "temp_time1525"="hr of 15<Temp<25", "temprh_rh90"="hr 15<Temp<25, RH>90%")
#
#### cane-dis
#	p.f1 = ggplot(data.canedis, aes(x=canes, y=value_dis_chg) ) +
#		geom_point(aes(shape=.data[[shape.f.factor]], color=.data[[color.f.factor]] ), size=1) +
#		stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.5, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#		stat_correlation(method="pearson", use_label(c("P","n")), size=2.5, label.x=0.05, label.y=0.93 ) +
#		facet_grid(rows=vars(var_dis), cols=vars(experiment), scales="free_y", labeller=labeller(experiment=facet.lab.exp, var_dis=facet.lab.vardis) ) +
#		scale_x_continuous(breaks=c(40,60,80,100) ) +
#		scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#		scale_shape_manual(values=shape.f.vector) +
#		scale_color_manual(values=color.f.vector) +
#		theme_bw() +
# 		theme(axis.title.x=element_text(margin=margin(t=8.25)), axis.title.y=element_blank() ) +
#		theme(legend.position="right", legend.margin=margin(l=-2.75) ) +
#		labs(x="# of canes", shape="Rating date", color="Rating date") 
#
#	ggplot2::ggsave(file="./4_results/z_09-2_rating_f1_cane-dis_not-shown.png", device="png", plot=p.f1, width=9, height=5.5, units="in", dpi=600)
#		
#### cane-env
#	## set up function
#	plot.f2 = function(df.x, vec.env, var.shape, vec.shape, var.color, vec.color) {
#		## filter
#		df.x = df.x %>% filter(var_env %in% vec.env) 
#		## plot
#		p = ggplot(df.x, aes(x=canes, y=value_env) ) +
#			geom_point(aes(shape=.data[[var.shape]], color=.data[[var.color]] ), size=0.9) +
#			stat_correlation(method="pearson", use_label(c("r","R.CI")), size=1.9, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#			stat_correlation(method="pearson", use_label(c("P","n")), size=1.9, label.x=0.05, label.y=0.90 ) +
#			facet_grid(rows=vars(var_env), cols=vars(experiment), scales="free_y", labeller=labeller(experiment=facet.lab.exp, var_env=facet.lab.varenv) ) +
#			scale_x_continuous(breaks=c(40,60,80,100) ) +
#			scale_y_continuous(expand=expansion(mult=c(0.05,0.27)) ) +
#			scale_shape_manual(values=vec.shape) +
#			scale_color_manual(values=vec.color) +
#			theme_bw() +
#	 		theme(axis.title.x=element_text(margin=margin(t=8.25)), axis.title.y=element_blank(), axis.text=element_text(size=7) ) +
#	 		theme(strip.text.x=element_text(size=7, margin=margin(t=2.75, b=2.75)), strip.text.y=element_text(size=7, margin=margin(l=2.75, r=2.75)) ) +
#			theme(legend.position="right", legend.margin=margin(l=-2.75) ) +
#			labs(x="# of canes", shape="Rating date", color="Rating date") 
#		## send plot as output
#		return(p)
#	}
#	
#	p.f2.1 = plot.f2(df.x=data.caneenv, vec.env=c("lwe_rh80","lwe_rh90","rh_day","rh_night"), var.shape=shape.f.factor, vec.shape=shape.f.vector, var.color=color.f.factor, vec.color=color.f.vector)
#	p.f2.2 = plot.f2(df.x=data.caneenv, vec.env=c("temp_day","temp_night","temp_time1525","temprh_rh90"), var.shape=shape.f.factor, vec.shape=shape.f.vector, var.color=color.f.factor, vec.color=color.f.vector)
#
#	p.f2 = wrap_plots(p.f2.1, p.f2.2, nrow=1, guides="collect") + 
#		plot_annotation(theme=theme(legend.position="right"))
#
#	ggplot2::ggsave(file="./4_results/z_09-2_rating_f2_cane-env_not-shown.png", device="png", plot=p.f2, width=9, height=5.75, units="in", dpi=600)
#		
#### env-dis
#	## set up function
#	plot.f3 = function(df.x, var.dis, var.shape, vec.shape, var.color, vec.color) {
#		## convert to wide
#		df.x = df.x %>% spread(key=var_dis, value=value_dis_chg)
#		## set y axis title
#			   if (var.dis == "incid") 			   { y.title = "Incidence (%)"
#		} else if (var.dis == "length_mean_plots") { y.title = "Lesion length (cm)"
#		} else { print('Invalid var.dis') }		
#		## plot
#		p = ggplot(df.x, aes(x=value_env, y=.data[[var.dis]] ) ) +
#			geom_point(aes(shape=.data[[var.shape]], color=.data[[var.color]] ), size=0.9) +
#			stat_correlation(method="pearson", use_label(c("r","R.CI")), size=1.7, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#			stat_correlation(method="pearson", use_label(c("P","n")), size=1.7, label.x=0.05, label.y=0.91 ) +
#			facet_grid(rows=vars(experiment), cols=vars(var_env), scales="free_x", labeller=labeller(experiment=facet.lab.exp, var_env=facet.lab.varenv) ) +
#			scale_y_continuous(expand=expansion(mult=c(0.05,0.28)) ) +
#			scale_shape_manual(values=vec.shape) +
#			scale_color_manual(values=vec.color) +
#			theme_bw() +
#	 		theme(axis.title.x=element_blank(), axis.text=element_text(size=8) ) +
#	 		theme(strip.text.x=element_text(size=5.75, margin=margin(t=2.75, b=2.75)), strip.text.y=element_text(size=6, margin=margin(l=2.75, r=2.75)) ) +
#			theme(legend.position="bottom", legend.margin=margin(t=-5.5) ) +
#			labs(y=y.title, shape="Rating date", color="Rating date")
#		## set filename var
#			   if (var.dis == "incid") 			   { var.dis = "incidence"
#		} else if (var.dis == "length_mean_plots") { var.dis = "severity"
#		} else { print('Invalid var.dis') }
#		## save
#		ggplot2::ggsave(file=paste("./4_results/z_09-2_rating_f3_env-dis_", var.dis, "_not-shown.png", sep=""), 
#			device="png", plot=p, width=9, height=4.25, units="in", dpi=600)
#	}
#
#	plot.f3(df.x=data.envdis, var.dis="incid", var.shape=shape.f.factor, vec.shape=shape.f.vector, var.color=color.f.factor, vec.color=color.f.vector)
#	plot.f3(df.x=data.envdis, var.dis="length_mean_plots", var.shape=shape.f.factor, vec.shape=shape.f.vector, var.color=color.f.factor, vec.color=color.f.vector)
#		
