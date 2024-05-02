#####################################
# RASPBERRY Cane Botrytis 			#
# Multivariate 						#
# End - final date, overall average	#
# Visualize							#
#####################################
# organize data for exploratory correlation analyses among disease, environment, canes


## built on Docker putmanlab/exploratory-analysis:420.0

library(conflicted)

library(dplyr)
library(forcats)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)


library(patchwork) # for combining plots: wrap_plots
library(rstatix) # for correlation statistics: cor_test

# install ggpmisc for printing line equation, R-squared via stat_poly_eq(); after dependencies ggpp (after dependency xts), confintr, lmodel2
remotes::install_version("xts", version="0.12-0", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
remotes::install_version("ggpp", version="0.4.5", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
remotes::install_version("confintr", version="0.1.2", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
remotes::install_version("lmodel2", version="1.7-3", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
remotes::install_version("ggpmisc", version="0.5.1", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
library(ggpmisc)

## install ggh4x for custom facet scales: factted_pos_scales()
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

### upload
	data.canedis = read_csv(file="./2_data_curated/rasp-caneb_08_multivar_end_cane-dis.csv", col_names=T, na=c("","."))
	in.caneenv = read_csv(file="./2_data_curated/rasp-caneb_08_multivar_end_cane-env.csv", col_names=T, na=c("","."))
	in.envdis = read_csv(file="./2_data_curated/rasp-caneb_08_multivar_end_env-dis.csv", col_names=T, na=c("","."))
	data.rowdis = read_csv(file="./2_data_curated/rasp-caneb_08_multivar_end_row-dis.csv", col_names=T, na=c("","."))
	data.rowenv = read_csv(file="./2_data_curated/rasp-caneb_08_multivar_end_row-env.csv", col_names=T, na=c("","."))

### filter (variables chosen from z_not-shown_08-1_multivar_org_end.r section C
	data.caneenv = in.caneenv %>% filter(var_env %in% c("lwe_dpd20","lwe_rh85","lwe_rh90","rh_day","rh_night","temp_day","temp_night","temprh_rh90") )
	data.envdis = in.envdis %>%   filter(var_env %in% c("lwe_dpd20","lwe_rh85","lwe_rh90","rh_day","rh_night","temp_day","temp_night","temprh_rh90") )
	data.rowenv = data.rowenv %>%   filter(var_env %in% c("lwe_dpd20","lwe_rh85","lwe_rh90","rh_day","rh_night","temp_day","temp_night","temprh_rh90") )

	data.caneenv.2 = in.caneenv %>% filter(var_env %in% c("lwe_dpd20","lwe_rh85","lwe_rh90","rh_day","rh_night","vpd_day","vpd_night","temp_day","temp_night","temprh_rh90") )
	data.envdis.2 = in.envdis %>%   filter(var_env %in% c("lwe_dpd20","lwe_rh85","lwe_rh90","rh_day","rh_night","vpd_day","vpd_night","temp_day","temp_night","temprh_rh90") )

### change column type
	data.rowdis = data.rowdis %>% mutate(plot_row=as.character(plot_row) )
	data.rowenv = data.rowenv %>% mutate(plot_row=as.character(plot_row) )

### prepare for graphs
	## names and orders
		# change order of treatments
		data.canedis = data.canedis %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))
		data.caneenv = data.caneenv %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))
		data.envdis = data.envdis %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))
		data.rowdis = data.rowdis %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))
		data.rowenv = data.rowenv %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))

		data.caneenv.2 = data.caneenv.2 %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))
		data.envdis.2 = data.envdis.2 %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))

	## graph settings
	source("./3_analysis/rcb_settings.r")	


#################################################
# B. Correlate - Across Experiments, Treatments #
#################################################

### set aesthetic settings for this section; factor (column) and vector of values
	shape.b.factor="treatment"
	shape.b.vector=vec.trt.shape
	color.b.factor="experiment"
	color.b.vector=vec.exp.color

### cane-dis (note: replaced by figure version in section F [combined with c1] )
#	p.b1 = ggplot(data.canedis, aes(x=canes, y=value_dis) ) +
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
#	ggplot2::ggsave(file="./4_results/z_09-1_end_b1_cane-dis_across_exp-ABC_not-shown.png", device="png", plot=p.b1, width=4, height=5.5, units="in", dpi=600)

		# results
			# incid: signif but is spurious due to clustering between A/B, C
			# severity: weak evidence, likely due to outliers (top right)
		# conclusion: due to experimental design cannot say the correlation means anything
			
### cane-env (note: superseded by section C)
	## set up function
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
#	p.b2.1 = plot.b2(df.x=data.caneenv, vec.env=c("lwe_dpd20","lwe_rh85","lwe_rh90","rh_day"), var.shape=shape.b.factor, vec.shape=shape.b.vector, var.color=color.b.factor, vec.color=color.b.vector)
#	p.b2.2 = plot.b2(df.x=data.caneenv, vec.env=c("rh_night","temp_day","temp_night","temprh_rh90"), var.shape=shape.b.factor, vec.shape=shape.b.vector, var.color=color.b.factor, vec.color=color.b.vector)
#
#	p.b2 = wrap_plots(p.b2.1, p.b2.2, nrow=1, guides="collect") + 
#		plot_annotation(theme=theme(legend.position="right"))
#
#	ggplot2::ggsave(file="./4_results/z_09-1_end_b2_cane-env_across_exp-ABC_not-shown.png", device="png", plot=p.b2, width=6, height=8.5, units="in", dpi=600)

		# results
			# lwe_dpd20: not signif
			# lwe_rh85, rh_day: signif negative correlation (not expected), but A/B vs C are clustered and each group appears to be + correlated
			# lwe_rh90, rh_night: signif/weak positive correlation
			# temp_day, temp_night, temprh_rh90: signif but is spurious due to clustering between A/B, C
		# conclusions
			# lwe_rh90, rh_night: correlations do not hold when examined by experiment, therefore are suspect
			# others: due to experimental design cannot say the correlation means anything
					
### env-dis (note: superseded by section C below but not commented to show clustering between experiments)
	p.b3 = ggplot(data.envdis, aes(x=value_env, y=value_dis) ) +
		geom_point(aes(shape=.data[[shape.b.factor]], color=.data[[color.b.factor]] ), size=1) +
		stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
		stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.91 ) +
		facet_grid(rows=vars(var_dis), cols=vars(var_env), scales="free") +
		scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
		scale_shape_manual(values=shape.b.vector) +
		scale_color_manual(values=color.b.vector) +
		theme_bw() +
 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
		theme(legend.position="bottom", legend.margin=margin(t=-2.75) )

	ggplot2::ggsave(file="./4_results/z_09-1_end_b3_env-dis_across_exp-ABC_not-shown.png", device="png", plot=p.b3, width=12, height=4.5, units="in", dpi=600)

		# results
			# incid: all vars except lwe_dpd20, rh_night signif, but are due to clustering between A/B vs C
			# severity: none signif
		# conclusion: due to experimental design cannot say the correlation means anything

	## with vapor pressure deficit
	p.b3.2 = ggplot(data.envdis.2, aes(x=value_env, y=value_dis) ) +
		geom_point(aes(shape=.data[[shape.b.factor]], color=.data[[color.b.factor]] ), size=1) +
		stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
		stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.91 ) +
		facet_grid(rows=vars(var_dis), cols=vars(var_env), scales="free") +
		scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
		scale_shape_manual(values=shape.b.vector) +
		scale_color_manual(values=color.b.vector) +
		theme_bw() +
 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
		theme(legend.position="bottom", legend.margin=margin(t=-2.75) )

	ggplot2::ggsave(file="./4_results/z_09-1_end_b3-2_env-dis_across_exp-ABC_not-shown.png", device="png", plot=p.b3.2, width=16, height=4.5, units="in", dpi=600)

### row-dis (note: commented out due to no pattern)
#	p.b4 = ggplot(data.rowdis, aes(x=plot_row, y=value_dis) ) +
#		geom_point(aes(shape=.data[[shape.b.factor]], color=.data[[color.b.factor]] ), size=1, position=position_jitter(w=0.05) ) +
#		facet_grid(rows=vars(var_dis), scales="free_y") +
#		scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#		scale_shape_manual(values=shape.b.vector) +
#		scale_color_manual(values=color.b.vector) +
#		theme_bw() +
# 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#		theme(legend.position="right", legend.margin=margin(l=-2.75) )
#	
#	ggplot2::ggsave(file="./4_results/z_09-1_end_b4_row-dis_across_exp-ABC_not-shown.png", device="png", plot=p.b4, width=4, height=5.5, units="in", dpi=600)

		# results: no pattern
		
### row-env (note: commented out due to no pattern)
	## set up function
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
#	p.b5.1 = plot.b5(df.x=data.rowenv, vec.env=c("lwe_dpd20","lwe_rh85","lwe_rh90","rh_day"), var.shape=shape.b.factor, vec.shape=shape.b.vector, var.color=color.b.factor, vec.color=color.b.vector)
#	p.b5.2 = plot.b5(df.x=data.rowenv, vec.env=c("rh_night","temp_day","temp_night","temprh_rh90"), var.shape=shape.b.factor, vec.shape=shape.b.vector, var.color=color.b.factor, vec.color=color.b.vector)
#
#	p.b5 = wrap_plots(p.b5.1, p.b5.2, nrow=1, guides="collect") + 
#		plot_annotation(theme=theme(legend.position="right"))
#
#	ggplot2::ggsave(file="./4_results/z_09-1_end_b5_row-env_across_exp-ABC_not-shown.png", device="png", plot=p.b5, width=6, height=8.5, units="in", dpi=600)

		# results: no pattern

###################################################
# C. Correlate - By Experiment, Across Treatments #
###################################################

### set aesthetic settings for this section; factor (column) and vector of values
	shape.c.factor="treatment"
	shape.c.vector=vec.trt.shape
	color.c.factor="treatment"
	color.c.vector=vec.trt.color

### cane-dis (note: replaced by figure version in section F [combined with c1] )
#	p.c1 = ggplot(data.canedis, aes(x=canes, y=value_dis) ) +
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
#	ggplot2::ggsave(file="./4_results/z_09-1_end_c1_cane-dis_by-exp_exp-ABC_not-shown.png", device="png", plot=p.c1, width=9, height=5.5, units="in", dpi=600)

		# results: no signif correlations
		
### cane-env (note: replaced by figure version in section F)
	## get ranges
#	data.caneenv %>% group_by(var_env) %>% summarize(value_min=min(value_env, na.rm=TRUE), value_max=max(value_env, na.rm=TRUE) )
#
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
#	p.c2.1 = plot.c2(df.x=data.caneenv, vec.env=c("lwe_dpd20","lwe_rh85","lwe_rh90","rh_day"), var.shape=shape.c.factor, vec.shape=shape.c.vector, var.color=color.c.factor, vec.color=color.c.vector)
#	p.c2.2 = plot.c2(df.x=data.caneenv, vec.env=c("rh_night","temp_day","temp_night","temprh_rh90"), var.shape=shape.c.factor, vec.shape=shape.c.vector, var.color=color.c.factor, vec.color=color.c.vector)
#
#	p.c2 = wrap_plots(p.c2.1, p.c2.2, nrow=1, guides="collect") + 
#		plot_annotation(theme=theme(legend.position="right"))
#
#	ggplot2::ggsave(file="./4_results/z_09-1_end_c2_cane-env_by-exp_exp-ABC_not-shown.png", device="png", plot=p.c2, width=14, height=7.4, units="in", dpi=600)

		# results
			# C-3-row rh_day: signif positive correlation, appears genuine
			# B-3-row temp_day: signif negative correlation
		# conclusion: see if correlations hold when examined by experiment+treatment
			# C-3-row rh_day: for twine only
			# B-3-row temp_day: for twine only
		
### env-dis (note: replaced by figure version in section F)
	## set up function
#	plot.c3 = function(df.x, var.dis, var.shape, vec.shape, var.color, vec.color) {
#		## convert to wide
#		df.x = df.x %>% spread(key=var_dis, value=value_dis)
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
#		} else if (var.dis == "length_mean_plot") { var.dis = "severity"
#		} else { print('Invalid var.dis') }
#		## save
#		ggplot2::ggsave(file=paste("./4_results/z_09-1_end_c3_env-dis_by-exp_exp-ABC_", var.dis, "_not-shown.png", sep=""), 
#			device="png", plot=p, width=12, height=4.5, units="in", dpi=600)
#	}
#
#	plot.c3(df.x=data.envdis, var.dis="incid", var.shape=shape.c.factor, vec.shape=shape.c.vector, var.color=color.c.factor, vec.color=color.c.vector)
#	plot.c3(df.x=data.envdis, var.dis="length_mean_plot", var.shape=shape.c.factor, vec.shape=shape.c.vector, var.color=color.c.factor, vec.color=color.c.vector)
		
		# results
			# incid: none are signif
			# severity: none are signif

### row-dis
	p.c4 = ggplot(data.rowdis, aes(x=plot_row, y=value_dis) ) +
		geom_point(aes(shape=.data[[shape.b.factor]], color=.data[[color.b.factor]] ), size=2, position=position_jitter(w=0.05) ) +
		facet_grid(rows=vars(var_dis), cols=vars(experiment), scales="free_y") +
		scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
		scale_shape_manual(values=shape.c.vector) +
		scale_color_manual(values=color.c.vector) +
		theme_bw() +
 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
		theme(legend.position="right", legend.margin=margin(l=-2.75) )

	ggplot2::ggsave(file="./4_results/z_09-1_end_c4_row-dis_by-exp_exp-ABC_not-shown.png", device="png", plot=p.c4, width=9, height=5.5, units="in", dpi=600)

		# results: gradients in disease across plot_row
		
### row-env
	## set up function
	plot.c5 = function(df.x, vec.env, var.shape, vec.shape, var.color, vec.color) {
		## filter
		df.x = df.x %>% filter(var_env %in% vec.env) 
		## plot
		p = ggplot(df.x, aes(x=plot_row, y=value_env) ) +
			geom_point(aes(shape=.data[[shape.b.factor]], color=.data[[color.b.factor]] ), size=2, position=position_jitter(w=0.05) ) +
			facet_grid(rows=vars(var_env), cols=vars(experiment), scales="free_y") +
			scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
			scale_shape_manual(values=vec.shape) +
			scale_color_manual(values=vec.color) +
			theme_bw() +
	 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
			theme(legend.position="right", legend.margin=margin(l=-2.75) )
		## send plot as output
		return(p)
	}
	
	p.c5.1 = plot.c5(df.x=data.rowenv, vec.env=c("lwe_dpd20","lwe_rh85","lwe_rh90","rh_day"), var.shape=shape.c.factor, vec.shape=shape.c.vector, var.color=color.c.factor, vec.color=color.c.vector)
	p.c5.2 = plot.c5(df.x=data.rowenv, vec.env=c("rh_night","temp_day","temp_night","temprh_rh90"), var.shape=shape.c.factor, vec.shape=shape.c.vector, var.color=color.c.factor, vec.color=color.c.vector)

	p.c5 = wrap_plots(p.c5.1, p.c5.2, nrow=1, guides="collect") + 
		plot_annotation(theme=theme(legend.position="right"))

	ggplot2::ggsave(file="./4_results/z_09-1_end_c5_row-env_by-exp_exp-ABC_not-shown.png", device="png", plot=p.c5, width=14, height=7.4, units="in", dpi=600)
		
		# results: possibility of gradient in environmental conditions for B-3-row plot_row=4
		

###################################################
# D. Correlate - By Treatment, Across Experiments #
###################################################
# note: covered by sections B and C above

### set aesthetic settings for this section; factor (column) and vector of values
#	shape.d.factor="experiment"
#	shape.d.vector=vec.exp.shape
#	color.d.factor="experiment"
#	color.d.vector=vec.exp.color
#
#### cane-dis
#	p.d1 = ggplot(data.canedis, aes(x=canes, y=value_dis) ) +
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
#	ggplot2::ggsave(file="./4_results/z_09-1_end_d1_cane-dis_by-trt_exp-ABC_not-shown.png", device="png", plot=p.d1, width=9, height=5, units="in", dpi=600)
#
#		# results
#			# incid: blade, control, manual; but appears due to clustering between A/B and C; one outlier in middle gives appearance of genuine correlation
#			# severity: control only signif, but appears due to clustering between A/C and B
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
#	p.d2.1 = plot.d2(df.x=data.caneenv, vec.env=c("lwe_dpd20","lwe_rh85","lwe_rh90","rh_day"), var.shape=shape.d.factor, vec.shape=shape.d.vector, var.color=color.d.factor, vec.color=color.d.vector)
#	p.d2.2 = plot.d2(df.x=data.caneenv, vec.env=c("rh_night","temp_day","temp_night","temprh_rh90"), var.shape=shape.d.factor, vec.shape=shape.d.vector, var.color=color.d.factor, vec.color=color.d.vector)
#
#	p.d2 = wrap_plots(p.d2.1, p.d2.2, nrow=1, guides="collect") + 
#		plot_annotation(theme=theme(legend.position="right"))
#
#	ggplot2::ggsave(file="./4_results/z_09-1_end_d2_cane-env_by-trt_exp-ABC_not-shown.png", device="png", plot=p.d2, width=14, height=7.4, units="in", dpi=600)
#	
#		# results
#			# lwe_dpd20: twine only
#			# lwe_rh90: control, twine
#			# rh_day: weak, but is negative and due to clustering between A/B vs C
#			# rh_night: twine only
#			# temp_day, temp_night: significant all treatments, but due to clustering between A/B vs C
#		# conclusion: not consistent across treatments, doesn't make sense for only twine or only twine/control to have signif correlation
#
#### env-dis
#	## set up function
#	plot.d3 = function(df.x, var.dis, var.shape, vec.shape, var.color, vec.color) {
#		## convert to wide
#		df.x = df.x %>% spread(key=var_dis, value=value_dis)
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
#		} else if (var.dis == "length_mean_plot") { var.dis = "severity"
#		} else { print('Invalid var.dis') }
#		## save
#		ggplot2::ggsave(file=paste("./4_results/z_09-1_end_d3_env-dis_by-trt_exp-ABC_", var.dis, "_not-shown.png", sep=""), 
#			device="png", plot=p, width=12, height=8, units="in", dpi=600)
#	}
#
#	plot.d3(df.x=data.envdis, var.dis="incid", var.shape=shape.d.factor, vec.shape=shape.d.vector, var.color=color.d.factor, vec.color=color.d.vector)
#	plot.d3(df.x=data.envdis, var.dis="length_mean_plot", var.shape=shape.d.factor, vec.shape=shape.d.vector, var.color=color.d.factor, vec.color=color.d.vector)

		# results
			# incid: numerous signif correlations, but all are due to clustering between A/B vs C
			# severity: a couple signif correlations, but all are due to clustering between A/B vs C
		# conclusion: due to experimental design cannot say the correlation means anything


###########################################
# E. Correlate - By Experiment, Treatment #
###########################################
# note: sample size small, covered by sections B and C above

### cane-dis
	## set up function
#	plot.e1 = function(df.x, var.dis) {
#		## convert to wide
#		df.x = df.x %>% spread(key=var_dis, value=value_dis)
#		## plot
#		p = ggplot(df.x, aes(x=canes, y=.data[[var.dis]] ) ) +
#			geom_point(size=1) +
#			stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#			stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.91 ) +
#			facet_grid(rows=vars(experiment), cols=vars(treatment)) +
#			scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#			theme_bw() +
#	 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#			theme(legend.position="bottom", legend.margin=margin(t=-2.75) )
#		## send plot as output
#		return(p)
#	}
#	
#	p.e1.1 = plot.e1(df.x=data.canedis, var.dis="incid")
#	p.e1.2 = plot.e1(df.x=data.canedis, var.dis="length_mean_plot")
#
#	p.e1 = wrap_plots(p.e1.1, p.e1.2, nrow=1, guides="collect") + 
#		plot_annotation(theme=theme(legend.position="bottom"))
#		
#	ggplot2::ggsave(file="./4_results/z_09-1_end_e1_cane-dis_by-exp-trt_exp-ABC_not-shown.png", device="png", plot=p.e1, width=12, height=6, units="in", dpi=600)
#
#		# results incid
#			# A-2-row: none
#			# B-2-row: twine, control positive corr (blade non-signif negative corr)
#			# C-3-row: twine only, but is negative
#		# results severity
#			# A-2-row: none
#			# B-3-row: blade only, but is negative
#			# C-3-row: blade only
#		# conclusion: few correlations and are inconsistent, some are reverse of what is expected, doesn't make sense for only twine to be signif but not other blade, manual
#			
#### cane-env
#	## set up function
#	plot.e2 = function(df.x, var.exp) {
#		# set plot function 
#		subplot.e2 = function(df.x, vec.env) {
#			# filter
#			df.x = df.x %>% filter(var_env %in% vec.env) 
#			# plot
#			sp = ggplot(df.x, aes(x=canes, y=value_env) ) +
#				geom_point(size=1) +
#				stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#				stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.92 ) +
#				facet_grid(rows=vars(var_env), cols=vars(treatment), scales="free_y") +
#				scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#				theme_bw() +
#		 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#				theme(legend.position="right", legend.margin=margin(t=-2.75) )
#			# send plot as output
#			return(sp)
#		}
#		# filter for experiment
#		df.x = df.x %>% filter(experiment == var.exp)
#		# make individual panels
#		p.1 = subplot.e2(df.x=df.x, vec.env=c("lwe_dpd20","lwe_rh85","lwe_rh90","rh_day") )
#		p.2 = subplot.e2(df.x=df.x, vec.env=c("rh_night","temp_day","temp_night","temprh_rh90") )
#		# combine
#		p = wrap_plots(p.1, p.2, nrow=1, guides="collect") + 
#			plot_annotation(theme=theme(legend.position="right"))
#		# set filename var
#		var.exp = var.exp %>% str_replace_all(c("-2-row" = "","-3-row" = "") )
#		# save plot
#		ggplot2::ggsave(file=paste("./4_results/z_09-1_end_e2_cane-env_by-exp-trt_exp-", var.exp, "_not-shown.png", sep=""),
#			device="png", plot=p, width=14, height=7.4, units="in", dpi=600)
#	}
#
#	plot.e2(df.x=data.caneenv, var.exp="A-2-row")
#	plot.e2(df.x=data.caneenv, var.exp="B-3-row")
#	plot.e2(df.x=data.caneenv, var.exp="C-3-row")
#	
#		# results
#			# A-2-row: lwe_dpd20 (weak), lwe_rh85, rh_night positive correlation for twine only; no other signif
#			# B-3-row: 
#				# lwe_dpd20, lwe_rh90 positive correlation for twine only
#				# temp_day weak negative correlation for twine only
#			# C-3-row: lwe_dpd20 for blade only, rh_day for twine only, 
#		# conclusion: very few and inconsistent
#			# does not make sense that only twine showed a correlation and not blade/manual
#			# on the other hand, correlation appeared for twine only in both A-2-row and B-3-row
#
#### env-dis
#	## set up function
#	plot.e3 = function(df.x, var.dis) {
#		# set plot function
#		plot.exp.e3 = function(df.x, var.exp, var.dis) {
#			# filter for experiment
#			df.x = df.x %>% filter(experiment == var.exp)
#			# convert to wide
#			df.x = df.x %>% spread(key=var_dis, value=value_dis)
#			# plot
#			p = ggplot(df.x, aes(x=value_env, y=.data[[var.dis]] ) ) +
#				geom_point(size=1) +
#				stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.25, label.x=0.05, label.y=0.98, small.r=TRUE ) +
#				stat_correlation(method="pearson", use_label(c("P","n")), size=2.25, label.x=0.05, label.y=0.91 ) +
#				facet_grid(rows=vars(treatment), cols=vars(var_env), scales="free_x") +
#				scale_y_continuous(expand=expansion(mult=c(0.05,0.20)) ) +
#				theme_bw() +
#		 		theme(axis.title.x=element_text(margin=margin(t=8.25)) ) +
#				theme(legend.position="bottom", legend.margin=margin(t=-2.75) )
#			# set filename variables
#				# experiment
#				var.exp = var.exp %>% str_replace_all(c("-2-row" = "","-3-row" = "") )
#				# disease variable
#					   if (var.dis == "incid") 			   { var.dis = "incidence"
#				} else if (var.dis == "length_mean_plot") { var.dis = "severity"
#				} else { print('Invalid var.dis') }
#			# save
#			ggplot2::ggsave(file=paste("./4_results/z_09-1_end_e3_env-dis_by-exp-trt_", var.dis, "_exp-", var.exp, "_not-shown.png", sep=""), 
#				device="png", plot=p, width=12, height=8, units="in", dpi=600)
#		}
#		# run for each experiment
#		plot.exp.e3(df.x=df.x, var.exp="A-2-row", var.dis=var.dis)
#		plot.exp.e3(df.x=df.x, var.exp="B-3-row", var.dis=var.dis)
#		plot.exp.e3(df.x=df.x, var.exp="C-3-row", var.dis=var.dis)	
#	}
#
#	plot.e3(df.x=data.envdis, var.dis="incid")
#	plot.e3(df.x=data.envdis, var.dis="length_mean_plot")

		# results incidence
			# A-2-row
				# lwe_dpd20, lwe_rh90, rh_night weak positive correlation for blade only
				# no other variables, treatments signif
			# B-3-row
				# lwe_dpd20, lwe_rh90, rh_night (weak), temprh_rh90 (weak) positive correlation for twine only
				# temp_day (weak) negative correlation for twine only
				# rh_day (weak) positive correlation for manual
				# no other variables, treatments signif
			# C-3-row
				# rh_night, temp_day: weak negative correlation for blade only, but in very narrow incid range
		# results severity
			# A-2-row
				# rh_day (negative), temprh_rh90 (negative) weak signif corr for blade only
			# B-3-row
				# lwe_dpd20 (weak negative), lwe_rh85 (weak negative), temp_night (positive) for manual only: due to outlier
				# rh_day (negative) for twine only: due to outlier
			# C-3-row
				# lwe_dpd20 (weak) positive corr for blade only
				# lwe_rh85, rh_day (weak) positive corr for manual only
		# conclusions: no clear correlations
			# only incidence B-3-row twine (lwe_dpd20, lwe_rh90, temp_day), severity C-3-row blade (lwe_dpd20) agrees with cane-dis correlation results
			# not consistent across treatments, variables, or experiments
			# some not in expected direction


##########################
# F. Correlate - Figures #
##########################
# for figures

### set aesthetic settings for this section; factor (column) and vector of values
	## by experiment, treatment (section B)
	shape.fb.factor="experiment"
	shape.fb.vector=vec.exp.shape
	color.fb.factor="experiment"
	color.fb.vector=vec.exp.color

	## by experiment, across treatments (section C)
	shape.fc.factor="treatment"
	shape.fc.vector=vec.trt.shape
	color.fc.factor="treatment"
	color.fc.vector=vec.trt.color

### get axis limits
	data.caneenv %>% 
		group_by(var_env) %>%
		summarize(value_min=min(value_env, na.rm=TRUE), value_max=max(value_env, na.rm=TRUE) )

	data.caneenv.2 %>% 
		group_by(var_env) %>%
		summarize(value_min=min(value_env, na.rm=TRUE), value_max=max(value_env, na.rm=TRUE) )

### names and orders
	## set strip labels
	facet.lab.vardis = c("incid"="Incidence (%)", "length_mean_plot"="Lesion length (cm)")
	facet.lab.exp = c('A-2-row'="Ranch 1 2-row", 'B-3-row'="Ranch 1 3-row", 'C-3-row'="Ranch 2 3-row")
	facet.lab.varenv = c(
		"lwe_rh85"="RH>85%", "lwe_rh90"="RH>90%", "rh_day"="Daylight", "rh_night"="Night", "vpd_day"="Daylight", "vpd_night"="Night",
		"temp_day"="Daylight", "temp_night"="Night", "lwe_dpd20"="dewpt. depress. 2C", "temprh_rh90"="15<T<25, RH>90%")	

### cane-dis - across experiments, treatments (from b1) + by treatment, across experiments (from c1)
	base.size=9
	## set up functions
		# across experiments, treatments (from b1)
		plot.fb1 = function(df.x, var.dis, var.shape, vec.shape, var.color, vec.color) {
			# filter for disease variable
			df.x = df.x %>% filter(var_dis == var.dis)
			# set y axis title
				   if (var.dis == "incid") 			   { y.title = "Incidence (%)"
			} else if (var.dis == "length_mean_plot") { y.title = "Lesion length (cm)"
			} else { print('Invalid var.dis') }		
			# make dummy variable for strip label
			df.x = df.x %>% mutate(strip_label="All Experiments")
			# plot
			p = ggplot(df.x, aes(x=canes, y=value_dis) ) +
				geom_point(aes(shape=.data[[var.shape]], color=.data[[var.color]] ), size=1.25) +
				stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.5, label.x=0.05, label.y=0.98, small.r=TRUE ) +
				stat_correlation(method="pearson", use_label(c("P","n")), size=2.5, label.x=0.05, label.y=0.91 ) +
				facet_grid(cols=vars(strip_label) ) +
				scale_x_continuous(breaks=c(40,60,80,100) ) +
				scale_y_continuous(expand=expansion(mult=c(0.05,0.23)) ) +
				scale_shape_manual(values=vec.shape, labels=facet.lab.exp) +
				scale_color_manual(values=vec.color, labels=facet.lab.exp) +
				theme_bw() +
		 		theme(strip.text=element_text(size=base.size+1, margin=margin(t=2.75, b=2.75) ) ) +
				theme(legend.title=element_text(size=base.size+1), legend.text=element_text(size=base.size, margin=margin(l=-8.25)), legend.spacing.x=unit(0.3, "lines") ) +
				labs(x="# of canes", y=y.title, shape="Experiment", color="Experiment")
		}
		
		# by experiment, treatments (from c1)
		plot.fc1 = function(df.x, var.dis, var.shape, vec.shape, var.color, vec.color, vec.lab.exp) {
			# filter for disease variable
			df.x = df.x %>% filter(var_dis == var.dis)
			# plot
			p = ggplot(df.x, aes(x=canes, y=value_dis) ) +
				geom_point(aes(shape=.data[[var.shape]], color=.data[[var.color]] ), size=1.25) +
				stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.5, label.x=0.05, label.y=0.98, small.r=TRUE ) +
				stat_correlation(method="pearson", use_label(c("P","n")), size=2.5, label.x=0.05, label.y=0.91 ) +
				facet_grid(cols=vars(experiment), labeller=labeller(experiment=vec.lab.exp) ) +
				scale_x_continuous(breaks=c(40,60,80,100) ) +
				scale_y_continuous(expand=expansion(mult=c(0.05,0.23)) ) +
				scale_shape_manual(values=vec.shape) +
				scale_color_manual(values=vec.color) +
				theme_bw() +
		 		theme(strip.text=element_text(size=base.size+1, margin=margin(t=2.75, b=2.75) ) ) +
				theme(legend.title=element_text(size=base.size+1), legend.text=element_text(size=base.size, margin=margin(l=-8.25)), legend.spacing.x=unit(0.3, "lines") ) +
				labs(x="# of canes", shape="Leaf removal", color="Leaf removal")
		}
		
	## plot
	p.fb1.1 = plot.fb1(df.x=data.canedis, var.dis="incid", 			   var.shape=shape.fb.factor, vec.shape=shape.fb.vector, var.color=color.fb.factor, vec.color=color.fb.vector)
	p.fb1.2 = plot.fb1(df.x=data.canedis, var.dis="length_mean_plot", var.shape=shape.fb.factor, vec.shape=shape.fb.vector, var.color=color.fb.factor, vec.color=color.fb.vector)
	p.fc1.1 = plot.fc1(df.x=data.canedis, var.dis="incid", 			   var.shape=shape.fc.factor, vec.shape=shape.fc.vector, var.color=color.fc.factor, vec.color=color.fc.vector, vec.lab.exp=facet.lab.exp)
	p.fc1.2 = plot.fc1(df.x=data.canedis, var.dis="length_mean_plot", var.shape=shape.fc.factor, vec.shape=shape.fc.vector, var.color=color.fc.factor, vec.color=color.fc.vector, vec.lab.exp=facet.lab.exp)

	p.fb1.1 = p.fb1.1 + theme(axis.title.x=element_blank(), 									  axis.title.y=element_text(size=base.size+2), axis.text.x=element_blank(), 			 axis.text.y=element_text(size=base.size) )
	p.fb1.2 = p.fb1.2 + theme(axis.title.x=element_text(size=base.size+2, margin=margin(t=8.25)), axis.title.y=element_text(size=base.size+2), axis.text.x=element_text(size=base.size), axis.text.y=element_text(size=base.size) )
	p.fc1.1 = p.fc1.1 + theme(axis.title.x=element_blank(),										  axis.title.y=element_blank(), 			   axis.text.x=element_blank(),				 axis.text.y=element_blank() )
	p.fc1.2 = p.fc1.2 + theme(axis.title.x=element_text(size=base.size+2, margin=margin(t=8.25)), axis.title.y=element_blank(), 			   axis.text.x=element_text(size=base.size), axis.text.y=element_blank() )

	p.fb1.2 = p.fb1.2 + theme(strip.text=element_blank(), strip.background=element_blank() )
	p.fc1.2 = p.fc1.2 + theme(strip.text=element_blank(), strip.background=element_blank() )

	p.fb1.1 = p.fb1.1 + labs(tag="A") + theme(plot.tag.position=c( 0.10,0.99) )
	p.fc1.1 = p.fc1.1 + labs(tag="B") + theme(plot.tag.position=c(-0.02,0.99) )

	p.fb1 = wrap_plots(p.fb1.1, p.fb1.2, ncol=1, guides="collect") +
		plot_annotation(theme=theme(legend.position="right", legend.box.margin=margin(l=-11, r=0) ) )
	p.fc1 = wrap_plots(p.fc1.1, p.fc1.2, ncol=1, guides="collect") +
		plot_annotation(theme=theme(legend.position="right", legend.box.margin=margin(l=-11, r=-8.25) ) )

	p.fbc = wrap_plots(p.fb1, p.fc1, nrow=1, widths=c(1,3.1) )

	ggplot2::ggsave(file="./4_results/09-1_end_fig-bc1_cane-dis.png", device="png", plot=p.fbc, width=9, height=4.8, units="in", dpi=600)
	
### cane-env - across experiments, treatments (from b2)
 	base.size=8
	## set up function
	plot.fc2 = function(df.x, vec.env, var.shape, vec.shape, var.color, vec.color) {
		## filter
		df.x = df.x %>% filter(var_env %in% vec.env) 
		## plot
		p = ggplot(df.x, aes(x=canes, y=value_env) ) +
			geom_point(aes(shape=.data[[var.shape]], color=.data[[var.color]] ), size=1) +
			stat_correlation(method="pearson", use_label(c("r","R.CI")), size=1.45, label.x=0.05, label.y=0.98, small.r=TRUE ) +
			stat_correlation(method="pearson", use_label(c("P","n")), size=1.45, label.x=0.05, label.y=0.92 ) +
			facet_grid(cols=vars(var_env), rows=vars(experiment), labeller=labeller(experiment=facet.lab.exp, var_env=facet.lab.varenv) ) +
			scale_x_continuous(breaks=c(40,60,80,100) ) +
			scale_shape_manual(values=vec.shape, labels=facet.lab.exp) +
			scale_color_manual(values=vec.color, labels=facet.lab.exp) +
			theme_bw() +
			theme(axis.title.x=element_text(size=base.size+2, margin=margin(t=8.25)), axis.title.y=element_text(size=base.size+2), axis.text.x=element_text(size=base.size), axis.text.y=element_text(size=base.size) ) +
		 	theme(strip.text=element_text(size=base.size+1) ) +
			theme(legend.position="bottom", legend.title=element_text(size=base.size+1), legend.text=element_text(size=base.size, margin=margin(l=-8.25)) ) +
			guides(shape=guide_legend(override.aes=list(size=2)), color=guide_legend(override.aes=list(size=2)) ) +
			labs(x="# of canes", shape="Leaf removal", color="Leaf removal")
		## send plot as output
		return(p)
	}
		
	p.fc2.1.1 = plot.fc2(df.x=data.caneenv, vec.env=c("rh_day","rh_night"), var.shape=shape.fc.factor, vec.shape=shape.fc.vector, var.color=color.fc.factor, vec.color=color.fc.vector) +
		scale_y_continuous(limits=c(63.9,85.3), expand=expansion(mult=c(0.05,0.25)) ) +
		theme(strip.text.y=element_blank(), strip.background.y=element_blank() ) +
		labs(y="Relative humidity (%)")

	p.fc2.1.2 = plot.fc2(df.x=data.caneenv, vec.env=c("temp_day","temp_night"), var.shape=shape.fc.factor, vec.shape=shape.fc.vector, var.color=color.fc.factor, vec.color=color.fc.vector) +
		scale_y_continuous(limits=c(9.8,20.8), expand=expansion(mult=c(0.05,0.25)) ) +
		theme(strip.text.y=element_blank(), strip.background.y=element_blank() ) +
		labs(y="Temperature (C)")

	p.fc2.2 = plot.fc2(df.x=data.caneenv, vec.env=c("lwe_dpd20","lwe_rh85","lwe_rh90","temprh_rh90"), var.shape=shape.fc.factor, vec.shape=shape.fc.vector, var.color=color.fc.factor, vec.color=color.fc.vector) +
		scale_y_continuous(limits=c(0,12), breaks=c(0,3,6,9,12), expand=expansion(mult=c(0.05,0.25)) ) +
		theme(strip.text.x=element_text(size=base.size-1.5)) +
		labs(y="Hours/day meeting threshold")

	p.fc2 = wrap_plots(p.fc2.1.1, p.fc2.1.2, p.fc2.2, nrow=1, guides="collect", widths=c(1,1,2.05) ) + 
		plot_annotation(theme=theme(legend.position="bottom", legend.box.margin=margin(t=-11, b=-11) ) )

	ggplot2::ggsave(file="./4_results/09-1_end_fig-c2_cane-env.png", device="png", plot=p.fc2, width=9, height=5, units="in", dpi=600)

#### cane-env - across experiments, treatments (from b2) - with vpd
# 	base.size=8
#		
#	p.fc2.2.1.1 = plot.fc2(df.x=data.caneenv, vec.env=c("rh_day","rh_night"), var.shape=shape.fc.factor, vec.shape=shape.fc.vector, var.color=color.fc.factor, vec.color=color.fc.vector) +
#		scale_y_continuous(limits=c(63.9,85.3), expand=expansion(mult=c(0.05,0.25)) ) +
#		theme(strip.text.y=element_blank(), strip.background.y=element_blank() ) +
#		labs(y="Relative humidity (%)")
#
#	p.fc2.2.1.2 = plot.fc2(df.x=data.caneenv.2, vec.env=c("vpd_day","vpd_night"), var.shape=shape.fc.factor, vec.shape=shape.fc.vector, var.color=color.fc.factor, vec.color=color.fc.vector) +
#		scale_y_continuous(limits=c(-0.858,-0.201), expand=expansion(mult=c(0.05,0.25)) ) +
#		theme(strip.text.y=element_blank(), strip.background.y=element_blank() ) +
#		labs(y="Vapor pressure deficit (kPa)")
#
#	p.fc2.2.1.3 = plot.fc2(df.x=data.caneenv, vec.env=c("temp_day","temp_night"), var.shape=shape.fc.factor, vec.shape=shape.fc.vector, var.color=color.fc.factor, vec.color=color.fc.vector) +
#		scale_y_continuous(limits=c(9.8,20.8), expand=expansion(mult=c(0.05,0.25)) ) +
#		theme(strip.text.y=element_blank(), strip.background.y=element_blank() ) +
#		labs(y="Temperature (C)")
#
#	p.fc2.2.2 = plot.fc2(df.x=data.caneenv, vec.env=c("lwe_dpd20","lwe_rh85","lwe_rh90","temprh_rh90"), var.shape=shape.fc.factor, vec.shape=shape.fc.vector, var.color=color.fc.factor, vec.color=color.fc.vector) +
#		scale_y_continuous(limits=c(0,12), breaks=c(0,3,6,9,12), expand=expansion(mult=c(0.05,0.25)) ) +
#		theme(strip.text.x=element_text(size=base.size-1.5)) +
#		labs(y="Hours/day meeting threshold")
#
#	p.fc2.2 = wrap_plots(p.fc2.2.1.1, p.fc2.2.1.2, p.fc2.2.1.3, p.fc2.2.2, nrow=1, guides="collect", widths=c(1,1,1,2.05) ) + 
#		plot_annotation(theme=theme(legend.position="bottom", legend.box.margin=margin(t=-11, b=-11) ) )
#
#	ggplot2::ggsave(file="./4_results/z_09-1_end_fig-c2-2_cane-env_not-shown.png", device="png", plot=p.fc2.2, width=11.5, height=5, units="in", dpi=600)

### env-dis - by experiment, across treatments (from c3)
	base.size=8
	## set up function
	plot.fc3 = function(df.x, var.dis, var.shape, vec.shape, var.color, vec.color) {
		## filter
		df.x = df.x %>% filter(var_dis == var.dis)
		## set y axis title
			   if (var.dis == "incid") 			   { y.title = "Incidence (%)"
		} else if (var.dis == "length_mean_plot") { y.title = "Lesion length (cm)"
		} else { print('Invalid var.dis') }		
		## plot 
		p = ggplot(df.x, aes(x=value_env, y=value_dis ) ) +
			geom_point(aes(shape=.data[[var.shape]], color=.data[[var.color]] ), size=1.25) +
			stat_correlation(method="pearson", use_label(c("r","R.CI")), size=2.1, label.x=0.05, label.y=0.98, small.r=TRUE ) +
			stat_correlation(method="pearson", use_label(c("P","n")), size=2.1, label.x=0.05, label.y=0.90 ) +
			facet_grid(rows=vars(experiment), cols=vars(var_env), scales="free_x", labeller=labeller(experiment=facet.lab.exp, var_env=facet.lab.varenv) ) +
#			scale_x_continuous(limits=c(0,12), breaks=c(0,3,6,9,12) ) +
			scale_shape_manual(values=vec.shape) +
			scale_color_manual(values=vec.color) +
			theme_bw() + 
	 		theme(axis.title.x=element_text(size=base.size+2, margin=margin(t=5.5) ), axis.title.y=element_text(size=base.size+2),  axis.text=element_text(size=base.size) ) +
			theme(strip.text.x=element_text(size=base.size, margin=margin(t=2.75, b=2.75)), strip.text.y=element_text(size=base.size+1, margin=margin(l=2.75, r=2.75))  ) +
			theme(legend.title=element_text(size=base.size+1), legend.text=element_text(size=base.size, margin=margin(l=-8.25)) ) + 
			theme(legend.position="bottom") + 
			labs(x="Hours/day meeting threshold", y=y.title, shape="Leaf removal", color="Leaf removal")
		## send plot as output
		return(p)
	}

	## modify dataset
		# remove variables that are duplicates of 07-1 figures
		data.envdis.fc3 = data.envdis %>% filter(var_env %in% c("lwe_dpd20","lwe_rh85","temprh_rh90") )
	
	p.fc3.1 = plot.fc3(df.x=data.envdis.fc3, var.dis="incid", var.shape=shape.fc.factor, vec.shape=shape.fc.vector, var.color=color.fc.factor, vec.color=color.fc.vector) +
		scale_y_continuous(limits=c(0,91.5), expand=expansion(mult=c(0.05,0.27)) ) +
		theme(strip.text.y=element_blank(), strip.background.y=element_blank() ) 
	p.fc3.2 = plot.fc3(df.x=data.envdis.fc3, var.dis="length_mean_plot", var.shape=shape.fc.factor, vec.shape=shape.fc.vector, var.color=color.fc.factor, vec.color=color.fc.vector) +
		scale_y_continuous(limits=c(0,46.3), expand=expansion(mult=c(0.05,0.27)) )

	p.fc3 = wrap_plots(p.fc3.1, p.fc3.2, nrow=1, guides="collect") +
		plot_annotation(theme=theme(legend.position="bottom", legend.box.margin=margin(t=-11, b=-11) ) )
		
	ggplot2::ggsave(file="./4_results/09-1_end_fig-c3_env-dis.png", device="png", plot=p.fc3, width=9, height=5.6, units="in", dpi=600)

#### env-dis - by experiment, across treatments (from c3) - with vapor pressure deficit
#	base.size=8
#	## modify dataset
#		# remove variables that are duplicates of 07-1 figures
#		data.envdis.fc3.2 = data.envdis.2 %>% filter(var_env %in% c("vpd_day","vpd_night","lwe_dpd20","lwe_rh85","temprh_rh90") )
#	
#	p.fc3.2.1 = plot.fc3(df.x=data.envdis.fc3.2, var.dis="incid", var.shape=shape.fc.factor, vec.shape=shape.fc.vector, var.color=color.fc.factor, vec.color=color.fc.vector) +
#		scale_y_continuous(limits=c(0,91.5), expand=expansion(mult=c(0.05,0.27)) ) +
#		theme(strip.text.y=element_blank(), strip.background.y=element_blank() ) 
#	p.fc3.2.2 = plot.fc3(df.x=data.envdis.fc3.2, var.dis="length_mean_plot", var.shape=shape.fc.factor, vec.shape=shape.fc.vector, var.color=color.fc.factor, vec.color=color.fc.vector) +
#		scale_y_continuous(limits=c(0,46.3), expand=expansion(mult=c(0.05,0.27)) )
#
#	p.fc3.2 = wrap_plots(p.fc3.2.1, p.fc3.2.2, nrow=1, guides="collect") +
#		plot_annotation(theme=theme(legend.position="bottom", legend.box.margin=margin(t=-11, b=-11) ) )
#		
#	ggplot2::ggsave(file="./4_results/z_09-1_end_fig-c3-2_env-dis_not-shown.png", device="png", plot=p.fc3.2, width=14, height=5.6, units="in", dpi=600)

