#####################################
# RASPBERRY Cane Botrytis 			#
# Multivariate 						#
# Dataset prep						#
# End - final date, overall average	#
#####################################
# organize data for exploratory correlation analyses among disease, environment, canes

## built on Docker putmanlab/exploratory-analysis:420.0

library(conflicted)

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)

library(rstatix) # for correlation statistics: cor_test

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/raspb_botrytis")

source("./3_analysis/rcb_functions.r")


#############
# A. Import #
#############

### disease
	## import
	in.incid = read_csv(file="./2_data_curated/rasp-caneb_02-1_disease_incidence_final.csv", col_names=T, na=c("","."))
	in.sever = read_csv(file="./2_data_curated/rasp-caneb_02-3_disease_severity_plot-mean_final.csv", col_names=T, na=c("","."))

	## join
	in.data = in.incid %>% left_join(in.sever, by=c(c("experiment","trt","block","date","days_after_trt")) )
		
	## remove unneeded columns
	in.data = in.data %>% select(-days_after_trt, -infected_canes, -low_n_later, -low_n_above, -low_n_healthy, -low_n_disease, -low_n_canes, -low_n_na, -low_mean_nlesions, -above_length_mean_plot)
		
	## rename column
	in.data = in.data %>% rename(treatment=trt)
	
### environment
	## raw env (rh_day, rh_night, temp_day, temp_night)
		# upload
		data.env.day = read_csv(file="./2_data_curated/rasp-caneb_04f_environ_final-day.csv", col_names=T, na=".")
	
		# remove unneeded columns
		data.env.day = data.env.day %>% select(-week, -day_of_week, -date_rating, -days_since_last, -dpt_avg, -dpt_depress)
		
		# gather
		data.env.day = data.env.day %>% gather(key="var_x", value="value_env", -experiment, -treatment, -block, -date, -period)
		
		# make new variable
		data.env.day = data.env.day %>% mutate(var_env=case_when(
			(var_x == "rh_avg" & period == "day") ~ "rh_day",
			(var_x == "rh_avg" & period == "night") ~ "rh_night",
			(var_x == "temp_avg" & period == "day") ~ "temp_day",
			(var_x == "temp_avg" & period == "night") ~ "temp_night",
			(var_x == "vpd_avg" & period == "day") ~ "vpd_day",
			(var_x == "vpd_avg" & period == "night") ~ "vpd_night") )
			
			# check
			data.env.day %>% distinct(var_x, period, var_env)
			
		# remove temporary/old columns
		data.env.day = data.env.day %>% select(-period, -var_x)

	## epidemiology summary stats
		# upload
		summ.env.day.l = read_csv(file="./2_data_curated/rasp-caneb_06-2b_environ_summ-epi_day.csv", col_names=T, na="")
	
		# remove unneeded columns
		summ.env.day.l = summ.env.day.l %>% select(-date_rating, -days_since_last)
		
		# modify var name
		summ.env.day.l = summ.env.day.l %>% mutate(var_env=str_replace(var_env, "_all", "") )
	
	## bind
	data.ee.day = bind_rows(data.env.day, summ.env.day.l)	
	
	## organize
		# change column order
		data.ee.day = data.ee.day %>% select(experiment, treatment, block, date, var_env, value_env)
	
		# sort
		data.ee.day = data.ee.day %>% arrange(experiment, treatment, block, date, var_env)

### layout
	## upload
	in.layout = read_csv(file="./1_methods/treatment layout.csv", col_names=T, na=c("","."))


##############
# B. Prepare #
##############

### prepare
	## filter for final rating date
	in.data = in.data %>% filter( (experiment %in% c("A-2-row","B-3-row") & date == as_date("2018-04-11") ) | (experiment == "C-3-row" & date == as_date("2018-11-28") ) )

	## remove unneeded column
	in.data = in.data %>% select(-date)

	## canes
	data.cane = in.data %>% select(-incid, -length_mean_plot)
	
	## disease
		# split
		data.dis = in.data %>% select(-canes)

		# convert to long
		data.dis = data.dis %>% gather(key="var_dis", value="value_dis", -experiment, -treatment, -block)

	## environment 
		# average over all dates
		data.env.end = data.ee.day %>% 
			group_by(experiment, treatment, block, var_env) %>% 
			summarize(value_env=mean(value_env, na.rm=TRUE) ) %>%
			ungroup()
			
		# round
		data.env.end = data.env.end %>% mutate(value_env=case_when(
			(!(var_env %in% c("vpd_day","vpd_night")) ) ~ round(value_env, digits=1),
			  (var_env %in% c("vpd_day","vpd_night"))   ~ round(value_env, digits=3) ) )			

### make datasets
	data.canedis = data.dis %>% left_join(data.cane, by=c(c("experiment","treatment","block")) )
	data.caneenv = data.env.end %>% left_join(data.cane, by=c(c("experiment","treatment","block")) )
	data.envdis = data.dis %>% left_join(data.env.end, by=c(c("experiment","treatment","block")) )
	data.rowdis = data.dis %>% left_join(in.layout, by=c(c("experiment","treatment","block")) )
	data.rowenv = data.env.end %>% left_join(in.layout, by=c(c("experiment","treatment","block")) )

### export
	write_csv(data.canedis, file="./2_data_curated/rasp-caneb_08_multivar_end_cane-dis.csv", na=".", append=F, col_names=T)
	write_csv(data.caneenv, file="./2_data_curated/rasp-caneb_08_multivar_end_cane-env.csv", na=".", append=F, col_names=T)
	write_csv(data.envdis,  file="./2_data_curated/rasp-caneb_08_multivar_end_env-dis.csv", na=".", append=F, col_names=T)
	write_csv(data.rowdis,  file="./2_data_curated/rasp-caneb_08_multivar_end_row-dis.csv", na=".", append=F, col_names=T)
	write_csv(data.rowenv,  file="./2_data_curated/rasp-caneb_08_multivar_end_row-env.csv", na=".", append=F, col_names=T)


##########################################
# C. Examine correlation among variables #
##########################################

### make df
	## copy df
	data.env.end.2 = data.env.end
	
	## rename variables
	data.env.end.2 = data.env.end.2 %>% rename(var_env_2=var_env, value_env_2=value_env)
	
	## join
	data.c = data.env.end %>% left_join(data.env.end.2, by=c(c("experiment","treatment","block")) )

### correlate
	corr.c = data.c %>% group_by(var_env, var_env_2) %>% cor_test(vars=c("value_env", "value_env_2"), method="pearson") %>% ungroup()

### visualize - correlation matrix
	plot.c = ggplot(corr.c, aes(x=var_env, y=var_env_2) ) +
		geom_tile(color="black", fill="white") +
		geom_point(aes(color=cor, size=abs(cor)), shape=15) +
		geom_point(data={. %>% filter(p > 0.05)}, size=6, shape=4) +
		coord_fixed(ratio=1) +
		scale_color_distiller(type="div", palette="RdBu", guide="colorbar", aesthetics="color", limits=c(-1,1), direction=-1 ) +
		scale_x_discrete(expand=c(0,0)) +
 		scale_y_discrete(expand=c(0,0)) +
 		theme_bw() +
 		theme(axis.text.x=element_text(angle=30, hjust=1) ) +
 		theme(axis.title.y=element_text(margin=margin(r=5.5)) ) +
		guides(color=guide_colorbar(direction="vertical", title.position="left") ) +
 		scale_size(range=c(1,9), guide=NULL)
	
	ggplot2::ggsave(file="./4_results/z_08-1_multivar_end_corr-among-env_not-shown.png", device="png", plot=plot.c, width=8, height=8, units="in", dpi=600)
	

