###########################
# RASPBERRY Cane Botrytis #
# Organize 				  #
# Disease Severity		  #
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

library(patchwork) # for wrap_plots

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/raspb_botrytis")


#############
# A. Import #
#############

### upload
	in.r1.sever = read_csv(file="./2_data/Rasp_CaneB_Run1_data_v2 - severity.csv", col_names=T, na="")
	in.r2.sever = read_csv(file="./2_data/run2_v7 - severity.csv", col_names=T, na="")

### check for NAs
	in.r1.sever %>% filter(is.na(n_lesions) | is.na(lesion_length_1) | is.na(lesion_length_2) | is.na(lesion_length_3) | is.na(lesion_length_4) | is.na(lesion_length_5) )
	in.r2.sever %>% filter(is.na(n_lesions) | is.na(`lesion_length_1(cm)`) | is.na(`lesion_length_2(cm)`) | is.na(`lesion_length_3(cm)`) )

### bind
	## remove n_lesions; will be analyzed separately in severity-derived incidence
#	in.r1.sever = in.r1.sever %>% select(-n_lesions)
#	in.r2.sever = in.r2.sever %>% select(-n_lesions)
	
	## convert to long
	in.r1.sever = in.r1.sever %>% gather(key="lesion", value="lesion_temp", -tunnel, -trt, -block, -cane, -n_lesions, -date, -status, -notes)
	in.r2.sever = in.r2.sever %>% gather(key="lesion", value="lesion_temp", -tunnel, -trt, -block, -cane, -n_lesions, -date, -status, -notes)

	## remove text from lesion column (r2 is equivalent to two str_replace statements replacing "lesion_temp_" and "\\(cm\\)")
	in.r1.sever = in.r1.sever %>% mutate(lesion=str_replace(lesion, "lesion_length_", "") )
	in.r2.sever = in.r2.sever %>% mutate(lesion=str_replace_all(lesion, "[lesion_length_\\(cm\\)]", "") )
	
	## prepare for binding
		# add experiment column
		in.r1.sever = in.r1.sever %>% mutate(experiment=case_when(
			(tunnel == "2-row") ~ "A-2-row",
			(tunnel == "3-row") ~ "B-3-row") )
		in.r2.sever = in.r2.sever %>% mutate(experiment="C-3-row")

		# change column type 
		in.r2.sever = in.r2.sever %>% mutate(n_lesions=as.character(n_lesions) )

	## bind
	in.sever = bind_rows(in.r1.sever, in.r2.sever)
	
	## remove tunnel, notes
	in.sever = in.sever %>% select(-tunnel, -notes)
	

#########################
# B. Organize - Lesions #
#########################
# each lesion individually

### checks
	## status to determine how to organize
#	in.sever %>% 
#		group_by(status) %>% 
#		summarize(ct=n(), ct_period=sum(lesion_temp == "."), ct_nolesion=sum(lesion_temp == "no lesion"), ct_lesionlater=sum(lesion_temp == "lesion later") ) %>%
#		print(n=Inf)
#
#	## for NAs
#	in.sever %>% filter(is.na(lesion_temp) )
#	
#	## for 'no lesion' for lesion 1
#	in.sever %>% filter(lesion == 1 & lesion_temp == "no lesion") 
#	in.sever %>% filter(lesion == 1 & lesion_temp == "no lesion") %>% group_by(status) %>% summarize(ct=n() )

### organize based on status
	## lesion_temp column
		# lesion length will be summed within each cane
		# 'no lesion', 'lesion later', 'merged' = 0 lesions are being summed and not averaged at this stage, and needed to contrast with true missing; are not there naturally
		# . = NA because are true missing (are not there artificially, i.e. compromised controls)
	
		# fill with 0s
		data.sever.les = in.sever %>% mutate(lesion_temp=replace(lesion_temp, lesion_temp %in% c("no lesion","lesion later","merged"), 0) )
	
		# fill with NA
		data.sever.les = data.sever.les %>% mutate(lesion_temp=replace(lesion_temp, lesion_temp == '.', NA) )
		data.sever.les = data.sever.les %>% mutate(n_lesions=replace(n_lesions, n_lesions == '.', NA) )
		
			# convert column type
			data.sever.les = data.sever.les %>% mutate(n_lesions=as.integer(n_lesions) )
		
		# check for other text strings
		data.sever.les %>% arrange(lesion_temp) %>% print(n=3)
		data.sever.les %>% arrange(desc(lesion_temp) ) %>% print(n=3)
	
		# update with final status
			# make new status column
			data.sever.les = data.sever.les %>% mutate(status_2=case_when(
				(status %in% c("no data all dates","no data now above twine later","no data now yes later")) ~ "complete",
				(!(status %in% c("no data all dates","no data now above twine later","no data now yes later"))) ~ NA_character_) )
				
			# fill for status == NA, 'lesions revised to previous'
			data.sever.les = data.sever.les %>% mutate(status_2=replace(status_2, is.na(status) | status %in% c("n_lesions revised to previous"), "complete") )
				
	## status column
		# fill values
			# 0s
			data.sever.les = data.sever.les %>% mutate(lesion_temp=replace(lesion_temp, status %in% c("not botrytis"), 0) )
			data.sever.les = data.sever.les %>% mutate(n_lesions=replace(n_lesions, status %in% c("not botrytis"), 0L) ) # L forces to be integer
		
			# NAs
			data.sever.les = data.sever.les %>% mutate(lesion_temp=replace(lesion_temp, status %in% c("compromised control","tag missing","cane broken no data"), NA) )
			data.sever.les = data.sever.les %>% mutate(n_lesions=replace(n_lesions, status %in% c("compromised control","tag missing","cane broken no data"), NA) )
		
		# update with final status
			# update
			data.sever.les = data.sever.les %>% mutate(status_2=replace(status_2, status %in% c("compromised control","not botrytis","tag missing","cane broken no data"), "complete") )
		
	## merged
		# no change - set status to complete
		data.sever.les = data.sever.les %>% mutate(status_2=replace(status_2, status %in% c("merged with ground lesion","merged with ground lesion n_lesions revised to prev"), "complete") )

### convert run 1 data from inches to cm
	## convert column type
	data.sever.les = data.sever.les %>% mutate(lesion_temp=as.numeric(lesion_temp) )

	## calculate
	data.sever.les = data.sever.les %>% mutate(lesion_length=case_when(
		(experiment %in% c("A-2-row","B-3-row")) ~ lesion_temp * 2.54,
		(experiment == "C-3-row") ~ lesion_temp) )
		
	## round
	data.sever.les = data.sever.les %>% mutate(lesion_length=round(lesion_length, digits=2) )
	
	## remove old column
	data.sever.les = data.sever.les %>% select(-lesion_temp)
	
### move above twine to separate column (remove lesion_location above)
	## move
		# lesion_length
		data.sever.les = data.sever.les %>% mutate(above_twine_length=case_when(
			(status == "above twine") ~ lesion_length,
			(status == "above twine tag missing") ~ NA_real_,
			(!(status %in% c("above twine","above twine tag missing")) | is.na(status) ) ~ NA_real_) )

		# n_lesions
		data.sever.les = data.sever.les %>% mutate(above_n_lesions=case_when(
			(status == "above twine") ~ n_lesions,
			(status == "above twine tag missing") ~ NA_integer_,
			(!(status %in% c("above twine","above twine tag missing")) | is.na(status) ) ~ NA_integer_) )

	## replace values
		# lesion_length
		data.sever.les = data.sever.les %>% mutate(lesion_length=replace(lesion_length, status %in% c("above twine"), 0) )
		data.sever.les = data.sever.les %>% mutate(lesion_length=replace(lesion_length, status %in% c("above twine tag missing"), NA) )

		# n_lesions
		data.sever.les = data.sever.les %>% mutate(n_lesions=replace(n_lesions, status %in% c("above twine"), 0) )
		data.sever.les = data.sever.les %>% mutate(n_lesions=replace(n_lesions, status %in% c("above twine tag missing"), NA) )

	## mark as complete
	data.sever.les = data.sever.les %>% mutate(status_2=replace(status_2, status %in% c("above twine","above twine tag missing"), "complete") )
		
### condense status groups
	## combine
	data.sever.les = data.sever.les %>% mutate(status_3=case_when(
		(status %in% c("n_lesions revised to previous","no data all dates","no data now above twine later","not botrytis") | is.na(status) ) ~ "ok",
		(status == "above twine") ~ "no low yes above",
		(status == "no data now yes later") ~ "disease later",
		(status %in% c("above twine tag missing","cane broken no data","compromised control","tag missing")) ~ "missing",
		(status %in% c("merged with ground lesion","merged with ground lesion n_lesions revised to prev")) ~ "merged ground") )

	## checks
		# check 0s, NAs are correct based on status
		data.sever.les %>% 
			group_by(status, status_3, status_2) %>% 
			summarize(
				ct=n(), 
				ct_0=sum(lesion_length == 0, na.rm=TRUE), ct_gt0=sum(lesion_length > 0, na.rm=TRUE), ct_na=sum(is.na(lesion_length)),
				ct_above_0=sum(above_twine_length == 0, na.rm=TRUE), ct_above_gt0=sum(above_twine_length > 0, na.rm=TRUE), ct_above_na=sum(is.na(above_twine_length)) ) %>%
			arrange(status_3, status) %>% 
			print(n=Inf)
		
		# no more than one status_3 per cane each date
		data.sever.les %>% group_by(experiment, trt, block, cane, date) %>% summarize(ct=n(), ct_status2=n_distinct(status_3) ) %>% filter(ct_status2 > 1)

### finalize
	## add days_after_trt column
		# calculate
		data.sever.les = data.sever.les %>% mutate(days_after_trt=case_when(
			(experiment %in% c("A-2-row","B-3-row") ) ~ date - as_date("2018-01-05"),
			(experiment == "C-3-row") ~ date - as_date("2018-09-25") ) )

		# convert to integer
		data.sever.les = data.sever.les %>% mutate(days_after_trt=as.integer(days_after_trt) )

	## change column order
	data.sever.les = data.sever.les %>% select(experiment, trt, block, cane, n_lesions, lesion, date, days_after_trt, status, status_2, status_3, lesion_length, above_twine_length, above_n_lesions)

### export final curated data
	## remove unneeded columns
	data.sever.les.exp = data.sever.les %>% select(-status, -status_2)

	write_csv(data.sever.les.exp, file="./2_data_curated/rasp-caneb_02-2_disease_severity_lesions_final.csv", na=".", append=F, col_names=T)


#######################
# C. Organize - Canes #
#######################
# sum lesions to a single value for each cane

### sum
	data.sever = data.sever.les %>%
		group_by(experiment, trt, block, cane, date, days_after_trt, n_lesions, status, status_3) %>%
		summarize(n_lesions=mean(n_lesions), lesion_cane_sum=sum(lesion_length), above_twine_cane_sum=sum(above_twine_length) ) %>%
		ungroup()
		
### organize
	## order columns
	data.sever = data.sever %>% select(experiment, trt, block, cane, date, days_after_trt, status, status_3, n_lesions, lesion_cane_sum, above_twine_cane_sum)
	
	## sort
	data.sever = data.sever %>% arrange(experiment, trt, block, cane, date)

### check	
	## status
	data.sever %>% 
		group_by(status, status_3) %>% 
		summarize(
			ct=n(), 
			ct_0=sum(lesion_cane_sum == 0, na.rm=TRUE), ct_gt0=sum(lesion_cane_sum > 0, na.rm=TRUE), ct_na=sum(is.na(lesion_cane_sum)),
			ct_above_0=sum(above_twine_cane_sum == 0, na.rm=TRUE), ct_above_gt0=sum(above_twine_cane_sum > 0, na.rm=TRUE), ct_above_na=sum(is.na(above_twine_cane_sum)) ) %>%
		print(n=Inf)

	## status 2
	data.sever %>% 
		group_by(status_3) %>% 
		summarize(
			ct=n(),
			ct_nlesion_0=sum(n_lesions == 0, na.rm=TRUE), ct_nlesion_gt0=sum(n_lesions > 0, na.rm=TRUE), ct_nlesion_na=sum(is.na(n_lesions)),
			ct_length_0=sum(lesion_cane_sum == 0, na.rm=TRUE), ct_length_gt0=sum(lesion_cane_sum > 0, na.rm=TRUE), ct_length_na=sum(is.na(lesion_cane_sum)),
			ct_above_0=sum(above_twine_cane_sum == 0, na.rm=TRUE), ct_above_gt0=sum(above_twine_cane_sum > 0, na.rm=TRUE), ct_above_na=sum(is.na(above_twine_cane_sum)) ) %>%
		print(n=Inf)

	## status_3 and n_lesions; status of status_3 == "ok" & n_lesions == 0
	data.sever %>%
		filter(status_3 %in% c("ok","no low yes above") & n_lesions == 0) %>%
		group_by(status) %>%
		summarize(ct=n() )

	## plots
#	data.sever %>% 
#		group_by(experiment, trt, block, date) %>% 
#		summarize(
#			ct=n(), 
#			ct_0=sum(lesion_cane_sum == 0, na.rm=TRUE), ct_gt0=sum(lesion_cane_sum > 0, na.rm=TRUE), ct_na=sum(is.na(lesion_cane_sum)),
#			ct_above_0=sum(above_twine_cane_sum == 0, na.rm=TRUE), ct_above_gt0=sum(above_twine_cane_sum > 0, na.rm=TRUE), ct_above_na=sum(is.na(above_twine_cane_sum)) ) %>%
#		arrange(experiment, date, trt, block) %>%
#		print(n=Inf)

	## n_lesions are integers
	data.sever %>% distinct(n_lesions)
						
### export final curated data
	## remove unneeded columns
	data.sever = data.sever %>% select(-status)

	write_csv(data.sever, file="./2_data_curated/rasp-caneb_02-2_disease_severity_cane-total_final.csv", na=".", append=F, col_names=T)

