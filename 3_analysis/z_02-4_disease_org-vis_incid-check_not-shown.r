###########################################
# RASPBERRY Cane Botrytis 				  #
# Organize and Visualize  				  #
# Disease - Incidence and Severity		  #
# incidence check
###########################################

# determine incidence in plot when some canes have severity = 0

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


######################
# A. incidence check #
######################

### disease
	## import
	in.incid = read_csv(file="./2_data_curated/rasp-caneb_02-1_disease_incidence_final.csv", col_names=T, na=c("","."))
	in.sever = read_csv(file="./2_data_curated/rasp-caneb_02-3_disease_severity_plot-mean_final.csv", col_names=T, na=c("","."))

### join
	## shorten printing of column names
#	options(pillar.min_title_chars=12)

	## join
	data.dis = in.incid %>% left_join(in.sever, by=c(c("experiment","trt","block","date","days_after_trt")) )
	
	## remove unneeded columns
	data.dis = data.dis %>% select(-low_mean_nlesions, -length_mean_plot, -above_length_mean_plot)
	
### check
	data.dis %>% 
		select(-incid) %>%
		filter(
			low_n_disease < low_n_canes & 
			( 
				(infected_canes >  10 & low_n_disease + low_n_above < 10) | 
				(infected_canes <= 10 & low_n_disease + low_n_above < infected_canes)
			) ) %>%
		mutate(n_diff=case_when(
			(infected_canes <= 10) ~ infected_canes - (low_n_disease + low_n_above),
			(infected_canes >  10) ~ 10 - (low_n_disease + low_n_above) ) ) %>%
#		arrange(infected_canes) %>%
		select(experiment, trt, block, date, low_n_canes, infected_canes, low_n_above, low_n_disease, n_diff, low_n_na, low_n_later, low_n_healthy ) %>%
		print(n=Inf)