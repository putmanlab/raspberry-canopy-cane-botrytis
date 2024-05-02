#####################################
# RASPBERRY Cane Botrytis 			#
# Multivariate 						#
# Dataset prep						#
# Rating date						#
#####################################
# organize data for exploratory correlation analyses among disease, environment, canes

# NOTE: not used, very little added to story not shown by end data

## built on Docker putmanlab/exploratory-analysis:420.0

#library(conflicted)
#
#library(dplyr)
#library(ggplot2)
#library(lubridate)
#library(readr)
#library(stringr)
#library(tidyr)
#
#library(rstatix) # for correlation statistics: cor_test
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
#### disease
#	## import
#	in.incid = read_csv(file="./2_data_curated/rasp-caneb_02-1_disease_incidence_final.csv", col_names=T, na=c("","."))
#	in.sever = read_csv(file="./2_data_curated/rasp-caneb_02-2_disease_severity_cane-total_final.csv", col_names=T, na=".")
#
#	## average severity
#	 in.sever = in.sever %>% 
#		group_by(experiment, trt, block, date, days_after_trt) %>%
#		summarize(length_mean_plots=round( mean(lesion_cane_total, na.rm=TRUE), digits=2) ) %>%
#		ungroup()
#		
#	## join
#	in.data = in.incid %>% left_join(in.sever, by=c(c("experiment","trt","block","date","days_after_trt")) )
#		
#	## remove unneeded columns
#	in.data = in.data %>% select(-infected_canes, -days_after_trt)
#		
#	## rename column
#	in.data = in.data %>% rename(treatment=trt)
#	
#### environment
#	## raw env (rh_day, rh_night, temp_day, temp_night)
#		# upload
#		data.env.rat = read_csv(file="./2_data_curated/rasp-caneb_04g_environ_final-rating.csv", col_names=T, na=".")
#	
#		# remove unneeded columns
#		data.env.rat = data.env.rat %>% select(-days_since_last, -dpt_avg, -dpt_depress)
#		
#		# gather
#		data.env.rat = data.env.rat %>% gather(key="var_x", value="value_env", -experiment, -treatment, -block, -date_rating, -period)
#		
#		# make new variable
#		data.env.rat = data.env.rat %>% mutate(var_env=case_when(
#			(var_x == "rh_avg" & period == "day") ~ "rh_day",
#			(var_x == "rh_avg" & period == "night") ~ "rh_night",
#			(var_x == "temp_avg" & period == "day") ~ "temp_day",
#			(var_x == "temp_avg" & period == "night") ~ "temp_night") )
#			
#			# check
#			data.env.rat %>% distinct(var_x, period, var_env)
#			
#		# remove temporary/old columns
#		data.env.rat = data.env.rat %>% select(-period, -var_x)
#
#	## epidemiology summary stats
#		# upload
#		summ.env.rat.l = read_csv(file="./2_data_curated/rasp-caneb_06-3b_environ_summ-epi_rating.csv", col_names=T, na="")
#	
#		# remove unneeded columns
#		summ.env.rat.l = summ.env.rat.l %>% select(-days_since_last, -value_env)
#
#		# update column name
#		summ.env.rat.l = summ.env.rat.l %>% rename(value_env=value_env_final)
#	
#	## bind
#	data.env.rat = bind_rows(data.env.rat, summ.env.rat.l)	
#	
#	## organize
#		# change column order
#		data.env.rat = data.env.rat %>% select(experiment, treatment, block, date_rating, var_env, value_env)
#	
#		# sort
#		data.env.rat = data.env.rat %>% arrange(experiment, treatment, block, date_rating, var_env)
#
#### layout
#	## upload
#	in.layout = read_csv(file="./1_methods/treatment layout.csv", col_names=T, na=c("","."))
#
#
################################################
## B. Prepare - Disease Change per rating date #
################################################
## calculate new disease since last rating date for correlation with rating period; rating period is between dates, whereas raw disease is sum since beginning
#
#### prepare
#	## remove other columns
#	data.dis.chg = in.data %>% select(-canes)
#
#	# convert to long
#	data.dis.chg = data.dis.chg %>% gather(key="var_dis", value="value_dis", -experiment, -treatment, -block, -date)
#
#	## assign date number
#	data.dis.chg = data.dis.chg %>% mutate(date_numb=case_when(
#		(date == as_date("2018-02-13")) ~ "date_1",
#		(date == as_date("2018-03-13")) ~ "date_2",
#		(date == as_date("2018-04-11")) ~ "date_3",
#		(date == as_date("2018-10-17")) ~ "date_1",
#		(date == as_date("2018-11-07")) ~ "date_2",
#		(date == as_date("2018-11-28")) ~ "date_3" ) )
#		
#	## remove columns
#	data.dis.chg = data.dis.chg %>% select(-date)
#
#### calculate difference		
#	## spread
#	data.dis.chg = data.dis.chg %>% spread(key=date_numb, value=value_dis)
#	
#	## calculate difference
#	data.dis.chg = data.dis.chg %>% mutate(
#		date_2_new = date_2 - date_1,
#		date_3_new = date_3 - date_2)
#
#### clean up
#	## remove old columns
#	data.dis.chg = data.dis.chg %>% select(-date_2, -date_3)
#	
#	## gather
#	data.dis.chg = data.dis.chg %>% gather(key="date_numb", value="value_dis_chg", -experiment, -treatment, -block, -var_dis)
#	
#	## add dates back in
#	data.dis.chg = data.dis.chg %>% mutate(date=case_when(
#		(experiment %in% c("A-2-row","B-3-row") & date_numb == "date_1") 	 ~ as_date("2018-02-13"),
#		(experiment %in% c("A-2-row","B-3-row") & date_numb == "date_2_new") ~ as_date("2018-03-13"),
#		(experiment %in% c("A-2-row","B-3-row") & date_numb == "date_3_new") ~ as_date("2018-04-11"),
#		(experiment== "C-3-row" & date_numb == "date_1") 	  ~ as_date("2018-10-17"),
#		(experiment== "C-3-row" & date_numb == "date_2_new") ~ as_date("2018-11-07"),
#		(experiment== "C-3-row" & date_numb == "date_3_new") ~ as_date("2018-11-28") ) )
#	
#	## remove date_numb
#	data.dis.chg = data.dis.chg %>% select(-date_numb)		
#
#	## order columns
#	data.dis.chg = data.dis.chg %>% select(experiment, treatment, block, date, var_dis, value_dis_chg) 
#	
#	## rename column
#	data.dis.chg = data.dis.chg %>% rename(date_rating=date)
#	
#	## sort
#	data.dis.chg = data.dis.chg %>% arrange(var_dis, experiment, treatment, block, date_rating)
#
#
###############
## C. Prepare #
###############
#
#### prepare
#	## canes
#		# filter for final rating date
#		data.cane = in.data %>% filter( (experiment %in% c("A-2-row","B-3-row") & date == as_date("2018-04-11") ) | (experiment == "C-3-row" & date == as_date("2018-11-28") ) )
#
#		# remove unneeded column
#		data.cane = data.cane %>% select(-date)
#
#		# remove other columns
#		data.cane = data.cane %>% select(-incid, -length_mean_plots)
#
#	## disease
#		# remove group with too many missing (inserted before initial check)
#		data.dis.chg = data.dis.chg %>% filter(!(experiment == "C-3-row" & date_rating == as_date("2018-11-28") & block == 1) )
#
#		# add date_rating column for combined graphing
#		data.dis.chg = data.dis.chg %>% mutate(date_rating_alt=case_when(
#			(date_rating %in% as_date(c("2018-02-13","2018-10-17")) ) ~ "1",
#			(date_rating %in% as_date(c("2018-03-13","2018-11-07")) ) ~ "2",
#			(date_rating %in% as_date(c("2018-04-11","2018-11-28")) ) ~ "3") )
#		
#		# reorder columns
#		data.dis.chg = data.dis.chg %>% select(experiment, treatment, block, date_rating, date_rating_alt, var_dis, value_dis_chg)
#
#	## environment
#		# remove group with too many missing (inserted before initial check)
#		data.env.rat = data.env.rat %>% filter(!(experiment == "C-3-row" & date_rating == as_date("2018-11-28") & block == 1) )
#
#		## add date_rating column for combined graphing
#		data.env.rat = data.env.rat %>% mutate(date_rating_alt=case_when(
#			(date_rating %in% as_date(c("2018-02-13","2018-10-17")) ) ~ "1",
#			(date_rating %in% as_date(c("2018-03-13","2018-11-07")) ) ~ "2",
#			(date_rating %in% as_date(c("2018-04-11","2018-11-28")) ) ~ "3") )			
#
#		# reorder columns
#		data.env.rat = data.env.rat %>% select(experiment, treatment, block, date_rating, date_rating_alt, var_env, value_env)
#	
#### make datasets
#	data.canedis = data.dis.chg %>% left_join(data.cane, by=c(c("experiment","treatment","block")) )
#	data.caneenv = data.env.rat %>% left_join(data.cane, by=c(c("experiment","treatment","block")) )
#	data.envdis = data.dis.chg %>% left_join(data.env.rat, by=c(c("experiment","treatment","block","date_rating","date_rating_alt")) )
#	data.rowdis = data.dis.chg %>% left_join(in.layout, by=c(c("experiment","treatment","block")) )
#	data.rowenv = data.env.rat %>% left_join(in.layout, by=c(c("experiment","treatment","block")) )
#
#### export
#	write_csv(data.canedis, file="./2_data_curated/rasp-caneb_08_multivar_rating_cane-dis.csv", na=".", append=F, col_names=T)
#	write_csv(data.caneenv, file="./2_data_curated/rasp-caneb_08_multivar_rating_cane-env.csv", na=".", append=F, col_names=T)
#	write_csv(data.envdis,  file="./2_data_curated/rasp-caneb_08_multivar_rating_env-dis.csv", na=".", append=F, col_names=T)
#	write_csv(data.rowdis,  file="./2_data_curated/rasp-caneb_08_multivar_rating_row-dis.csv", na=".", append=F, col_names=T)
#	write_csv(data.rowenv,  file="./2_data_curated/rasp-caneb_08_multivar_rating_row-env.csv", na=".", append=F, col_names=T)
#
#
###########################################
## D. Examine correlation among variables #
###########################################
#
#### make df
#	## copy df
#	data.env.rat.2 = data.env.rat
#	
#	## rename variables
#	data.env.rat.2 = data.env.rat.2 %>% rename(var_env_2=var_env, value_env_2=value_env)
#	
#	## join
#	data.d = data.env.rat %>% left_join(data.env.rat.2, by=c(c("experiment","treatment","block","date_rating")) )
#
#### correlate
#	corr.d = data.d %>% group_by(var_env, var_env_2) %>% cor_test(vars=c("value_env", "value_env_2"), method="pearson") %>% ungroup()
#
#### visualize - correlation matrix
#	plot.d = ggplot(corr.d, aes(x=var_env, y=var_env_2) ) +
#		geom_tile(color="black", fill="white") +
#		geom_point(aes(color=cor, size=abs(cor)), shape=15) +
#		geom_point(data={. %>% filter(p > 0.05)}, size=6, shape=4) +
#		coord_fixed(ratio=1) +
#		scale_color_distiller(type="div", palette="RdBu", guide="colorbar", aesthetics="color", limits=c(-1,1), direction=-1 ) +
#		scale_x_discrete(expand=c(0,0)) +
# 		scale_y_discrete(expand=c(0,0)) +
# 		theme_bw() +
# 		theme(axis.text.x=element_text(angle=30, hjust=1) ) +
# 		theme(axis.title.y=element_text(margin=margin(r=5.5)) ) +
#		guides(color=guide_colorbar(direction="vertical", title.position="left") ) +
# 		scale_size(range=c(1,9), guide=NULL)
#	
#	ggplot2::ggsave(file="./4_results/z_08-2_multivar_rating_corr-among-env_not-shown.png", device="png", plot=plot.d, width=8, height=8, units="in", dpi=600)
#	
#
