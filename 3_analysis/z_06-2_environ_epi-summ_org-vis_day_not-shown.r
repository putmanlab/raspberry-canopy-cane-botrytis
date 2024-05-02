#####################################
# RASPBERRY Cane Botrytis 			#
# Disease and Environmental Data	#
# Exploratory Analysis				#
# Epidemiology Summary Statistics	#
# Daily								#
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
	

######################
# B. Day - Calculate #
######################
# data loggers within plots

### run summary function
	summ.env.day.l = epi.summ(
		df.in=data.env.hr, in.interval="hour", extreme.method="none", extreme.period="date", out.interval="date", 
		cols.grp=c("experiment","treatment","block"), data.source="environment", lw.1.onset=1.8, lw.1.offset=2.2, lw.2.onset=2.0, lw.2.offset=3.8)

		# check for extra rows from grouping
		summ.env.day.l.chk = summ.env.day.l %>% 
			group_by(experiment, treatment, block, date, date_rating, days_since_last) %>%
			summarize(ct=n()) %>%
			ungroup()
			
			summ.env.day.l.chk %>% 
				group_by(experiment, treatment, block, date, days_since_last) %>% 
				summarize(ct=n(), ct_distinct=n_distinct(date_rating) ) %>% filter(ct_distinct > 1) %>% print(n=5)
	
			summ.env.day.l.chk %>% 
				group_by(experiment, treatment, block, date, date_rating) %>% 
				summarize(ct=n(), ct_distinct=n_distinct(days_since_last) ) %>% filter(ct_distinct > 1) %>% print(n=5)
	
### export
	write_csv(summ.env.day.l, file="./2_data_curated/rasp-caneb_06-2b_environ_summ-epi_day.csv", na="", col_names=T, append=F)


####################################
# B. Day - Visualize Raw - by date #
####################################
# data loggers within plots

### raw by block 
	# note: commented out because superseded by C below

#	## temp_time10_all
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="temp_time10_all", var.exp="A-2-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="temp_time10_all", var.exp="B-3-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="temp_time10_all", var.exp="C-3-row", filename.prefix=NULL, section.code="06-2b")
#
#	## temp_time15_all
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="temp_time15_all", var.exp="A-2-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="temp_time15_all", var.exp="B-3-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="temp_time15_all", var.exp="C-3-row", filename.prefix=NULL, section.code="06-2b")
#
#	## temp_time1525_all
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="temp_time1525_all", var.exp="A-2-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="temp_time1525_all", var.exp="B-3-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="temp_time1525_all", var.exp="C-3-row", filename.prefix=NULL, section.code="06-2b")
#
#	## temprh_rh80_all
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="temprh_rh80_all", var.exp="A-2-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="temprh_rh80_all", var.exp="B-3-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="temprh_rh80_all", var.exp="C-3-row", filename.prefix=NULL, section.code="06-2b")
#
#	## temprh_rh90_all
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="temprh_rh90_all", var.exp="A-2-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="temprh_rh90_all", var.exp="B-3-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="temprh_rh90_all", var.exp="C-3-row", filename.prefix=NULL, section.code="06-2b")
#
#	## lwe_rh75_all
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_rh75_all", var.exp="A-2-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_rh75_all", var.exp="B-3-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_rh75_all", var.exp="C-3-row", filename.prefix=NULL, section.code="06-2b")
#
#	## lwe_rh80_all
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_rh80_all", var.exp="A-2-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_rh80_all", var.exp="B-3-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_rh80_all", var.exp="C-3-row", filename.prefix=NULL, section.code="06-2b")
#
#	## lwe_rh85_all
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_rh85_all", var.exp="A-2-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_rh85_all", var.exp="B-3-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_rh85_all", var.exp="C-3-row", filename.prefix=NULL, section.code="06-2b")
#
#	## lwe_rh90_all
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_rh90_all", var.exp="A-2-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_rh90_all", var.exp="B-3-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_rh90_all", var.exp="C-3-row", filename.prefix=NULL, section.code="06-2b")
#
#	## lwe_dpd18_all
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_dpd18_all", var.exp="A-2-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_dpd18_all", var.exp="B-3-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_dpd18_all", var.exp="C-3-row", filename.prefix=NULL, section.code="06-2b")
#
#	## lwe_dpd20_all
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_dpd20_all", var.exp="A-2-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_dpd20_all", var.exp="B-3-row", filename.prefix=NULL, section.code="06-2b")
#	plot.day.epi.summ(df.in=summ.env.day.l, var.env="lwe_dpd20_all", var.exp="C-3-row", filename.prefix=NULL, section.code="06-2b")


#########################################
# C. Day - Visualize Raw - by treatment #
#########################################
# data loggers within plots

### names and orders
	## change order of treatments
	summ.env.day.l = summ.env.day.l %>% mutate(treatment=fct_relevel(treatment, c("blade","manual","twine","control")))

### raw by treatment
	plot.c = ggplot(summ.env.day.l, aes(x=treatment, y=value_env) ) +
		geom_violin(scale="count", draw_quantiles=c(0.25, 0.5, 0.75), size=0.2) +
		facet_grid(cols=vars(var_env), rows=vars(experiment) ) +
		theme_bw()
	ggplot2::ggsave(file="./4_results/z_06-2c_env_epi-summ_day_treatment_not-shown.png", device="png", plot=plot.c, width=16, height=9, units="in", dpi=600)
	
	# possible differences among treatments for lwe_dpd18, lwe_dpd20, lwe_rh90 (include lwe_rh80 for comparison)

### convert for statistical analysis
	## convert to proportions
	summ.env.day.w = summ.env.day.l %>% mutate(value_env_prop=round( value_env / 24, digits=3) )
	
	## remove old value column
	summ.env.day.w = summ.env.day.w %>% select(-value_env)
	
	## remove _all
	summ.env.day.w = summ.env.day.w %>% mutate(var_env=str_replace(var_env, "_all", "") )
	
	## convert to wide
	summ.env.day.w = summ.env.day.w %>% spread(key=var_env, value=value_env_prop)
	
### export
	write_csv(summ.env.day.w, file="./2_data_curated/rasp-caneb_06-2c_environ_summ-epi_day_proportion.csv", na=".", col_names=T, append=F)

