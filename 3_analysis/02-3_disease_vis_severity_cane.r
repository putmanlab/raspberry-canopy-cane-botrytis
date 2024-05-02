###########################
# RASPBERRY Cane Botrytis #
# Visualize				  #
# Disease Severity		  #
# Cane sums				  #
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

## install ggh4x for force_panelsizes to customize facet width
remotes::install_version("ggh4x", version="0.2.1", repos="https://cran.r-project.org/", dependencies=FALSE, upgrade="never")
library(ggh4x)

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/raspb_botrytis")


##############
## A. Import #
##############

### upload
	data.sever = read_csv(file="./2_data_curated/rasp-caneb_02-2_disease_severity_cane-total_final.csv", col_names=T, na="." )

### change column types
	data.sever = data.sever %>% mutate(block=as.character(block), cane=as.character(cane), days_after_trt=as.integer(days_after_trt), n_lesions=as.integer(n_lesions) )


#############################
# B. Cane Total - Summarize #
#############################
	
### by plot (treatment, block) and date (derived from status categories above)
	# low_ = incidence derived from severity; incidence in leaf removal zone (below first twine) on 10 selected canes
		# _rows: number of rows in dataframe
		# _later: no disease (no n_lesion or lesion_length) but disease appears on later date 
		# _healthy: no disease (all dates, above twine, or not botrytis)
		# _disease: disease present
		# _na: cane removed (compromised control, broken, tag missing)
	
	## calculate
	summ.1 = data.sever %>% 
		group_by(experiment, trt, block, date, days_after_trt) %>% 
		summarize(
			low_n_rows		=n_distinct(cane),
			low_n_later		=sum( status_3 == "disease later"),
			low_n_healthy	=sum( status_3 == "ok" & n_lesions == 0),
			low_n_above 	=sum( status_3 == "no low yes above" & n_lesions == 0),
			low_n_disease	=sum( status_3 == "merged ground" |
								 (status_3 == "ok" & n_lesions > 0) ),			
			low_n_na		=sum( status_3 == "missing" & is.na(n_lesions) ),
			low_mean_nlesions=mean(n_lesions, na.rm=TRUE),
			length_mean_plot=mean(lesion_cane_sum, na.rm=TRUE), 
			above_length_mean_plot=mean(above_twine_cane_sum, na.rm=TRUE)  ) %>%
		ungroup()
		
		# check that columns sum to 10
		summ.1 %>% filter(low_n_rows != low_n_later + low_n_healthy + low_n_above + low_n_disease + low_n_na)
		
	## round
	summ.1 = summ.1 %>% mutate(across(c(length_mean_plot, above_length_mean_plot), ~ round(., digits=1) ) )

	## calculate n_canes for incidence % calc (removes missing canes from total rows, but returns NA for compromised controls)
	summ.1 = summ.1 %>% mutate(low_n_canes=case_when(
		(low_n_na == low_n_rows) ~ NA_integer_,
		(low_n_na <  low_n_rows) ~ low_n_rows - low_n_na) )
		
	## fill compromised controls with NAs
	summ.1 = summ.1 %>% mutate(across(c(low_n_later, low_n_healthy, low_n_disease), ~ replace(., low_n_na == low_n_rows, NA) ))
	
	## order columns
	summ.1 = summ.1 %>% select(experiment, trt, block, date, days_after_trt, low_n_later, low_n_above, low_n_healthy, low_n_disease, low_n_canes, low_n_na, low_mean_nlesions, length_mean_plot, above_length_mean_plot)
		
### by treatment, date
	## calculate
	summ.2 = data.sever %>% 
		group_by(experiment, trt, date, days_after_trt) %>% 
		summarize(
			low_n_rows		  	 = n(),
			low_n_pos	 	  	 = sum( status_3 %in% c("ok","disease later","merged ground") & lesion_cane_sum >  0 ),
			low_n_0 		  	 = sum( status_3 %in% c("ok","disease later","merged ground") & lesion_cane_sum == 0 ),
			low_n_above 	  	 = sum( status_3 == "no low yes above"),
			low_n_missing	  	 = sum( status_3 == "missing"),
			low_mean_nlesions 	 = mean(n_lesions, na.rm=TRUE),
			length_mean_trt	  	 = mean(lesion_cane_sum, na.rm=TRUE), 
			above_length_mean_trt= mean(above_twine_cane_sum, na.rm=TRUE)  ) %>%
		ungroup()
		
		# check that columns sum to 10
		summ.2 %>% filter(low_n_rows != low_n_pos + low_n_0 + low_n_above + low_n_missing) %>% print(n=Inf)
		
	## for paper - longest average lesion length for each experiment
	summ.2 %>% group_by(experiment) %>% summarize(length_max=max(length_mean_trt) )
	
	## for paper - quality control summary
		# remove unneeded columns
		summ.2.tbl = summ.2 %>% select(-days_after_trt, -low_mean_nlesions, -length_mean_trt, -above_length_mean_trt)
		
		# gather
		summ.2.tbl = summ.2.tbl %>% gather(key="var_n", value="value_n", -experiment, -trt, -date, -low_n_rows)
		
		# create column converting rating date to integer
		summ.2.tbl = summ.2.tbl %>% mutate(date_alt=case_when(
			(date %in% as_date(c("2018-02-13","2018-10-17")) ) ~ "date1",
			(date %in% as_date(c("2018-03-13","2018-11-07")) ) ~ "date2",
			(date %in% as_date(c("2018-04-11","2018-11-28")) ) ~ "date3") )
		
		# combine columns
		summ.2.tbl = summ.2.tbl %>% unite("var_datealt", c("var_n", "date_alt"), remove=FALSE)
						
		# remove unneeded columns
		summ.2.tbl = summ.2.tbl %>% select(-date, -date_alt, -var_n)
		
		# spread
		summ.2.tbl = summ.2.tbl %>% spread(key=var_datealt, value=value_n)
		
		# reorder columns
		summ.2.tbl = summ.2.tbl %>% select(experiment, trt, low_n_rows, low_n_0_date1, low_n_above_date1, low_n_missing_date1, low_n_0_date2, low_n_above_date2, low_n_missing_date2, low_n_0_date3, low_n_above_date3, low_n_missing_date3)
		
		# show totals, percentage
		summ.2.tbl %>% group_by(experiment) %>% 
			summarize(across(c(low_n_rows, low_n_0_date1, low_n_above_date1, low_n_missing_date1, low_n_0_date2, low_n_above_date2, low_n_missing_date2, low_n_0_date3, low_n_above_date3, low_n_missing_date3), ~ sum(.x) ) )
		summ.2.tbl %>% group_by(experiment) %>% 
			summarize(across(c(low_n_rows, low_n_0_date1, low_n_above_date1, low_n_missing_date1, low_n_0_date2, low_n_above_date2, low_n_missing_date2, low_n_0_date3, low_n_above_date3, low_n_missing_date3), ~ sum(.x) ) ) %>%
			mutate(	  across(c(			   low_n_0_date1, low_n_above_date1, low_n_missing_date1, low_n_0_date2, low_n_above_date2, low_n_missing_date2, low_n_0_date3, low_n_above_date3, low_n_missing_date3), ~ round( (.x/low_n_rows)*100, digits=0) ) )

### by plot (treatment, block) across dates
	summ.3 = data.sever %>% 
		group_by(experiment, trt, block) %>% 
		summarize(length_mean_plot=mean(lesion_cane_sum, na.rm=TRUE)  ) %>%
		ungroup()

### by treatment
	summ.4 = data.sever %>% 
		group_by(experiment, trt) %>% 
		summarize(length_mean_trt=mean(lesion_cane_sum, na.rm=TRUE)  ) %>% 
		ungroup()

### export
	write_csv(summ.1, file="./2_data_curated/rasp-caneb_02-3_disease_severity_plot-mean_final.csv", na=".", append=F, col_names=T)
	write_csv(summ.2.tbl, file="./4_results/02-3_disease_severity_trt-mean_qual-control-summary.csv", na=".", append=F, col_names=T)
	

#############################
# C. Cane Total - Visualize #
#############################

### prepare annotations
	## mean letters
	c2.letters = tibble(
		experiment = c("A-2-row","A-2-row","A-2-row","A-2-row","B-3-row","B-3-row","B-3-row","B-3-row","C-3-row","C-3-row","C-3-row","C-3-row"),
		trt		   = c("blade","control","manual","twine",	   "blade","control","manual","twine",	   "blade","control","manual","twine"),
		letter_trt = c("a","ab","b","ab",					   "b","a","b","b",						   "ab","b","a","a") )

	## join annotation to summary
		# join
		summ.4 = summ.4 %>% left_join(c2.letters, by=c(c("experiment","trt")) )
		
		# combine mean and letter
		summ.4 = summ.4 %>% mutate(label_trt=paste(sprintf("%0.1f", length_mean_trt), " ", letter_trt, sep="") )
		
		# find value to vertically offset text
		summ.3 %>% 
			left_join(summ.4, by=c(c("experiment","trt")) ) %>%
			mutate(length_diff=length_mean_plot-length_mean_trt) %>%
			arrange(desc(length_diff) ) 
			
### prepare
	## change order of treatment factor
	data.sever = data.sever %>% mutate(trt=fct_relevel(trt, c("blade","manual","twine","control")))
	summ.2 = summ.2 %>% mutate(trt=fct_relevel(trt, c("blade","manual","twine","control")))
	summ.3 = summ.3 %>% mutate(trt=fct_relevel(trt, c("blade","manual","twine","control")))
	summ.4 = summ.4 %>% mutate(trt=fct_relevel(trt, c("blade","manual","twine","control")))
	
	## make character vectors to rename facet labels
	facet.lab.exp = c('A-2-row'="Ranch 1 2-row", 'B-3-row'="Ranch 1 3-row", 'C-3-row'="Ranch 2 3-row")
	facet.lab.trt = c('blade'="Blade", 'manual'="Manual", 'twine'="Twine", 'control'="Control")
	
	## get range for axis limits
	data.sever %>% summarize(length_min=min(lesion_cane_sum, na.rm=TRUE), length_max=max(lesion_cane_sum, na.rm=TRUE) )
	summ.3 %>% arrange(desc(length_mean_plot))

### raw - lines over dates
	## draw plot
	plot.c1 = ggplot(data.sever) + 
		geom_line(aes(x=days_after_trt, y=lesion_cane_sum, group=interaction(block, trt, cane)), size=0.10, alpha=0.5) + 
		geom_line(data=summ.2, aes(x=days_after_trt, y=length_mean_trt), linetype="dashed", size=0.50) +
  		facet_grid(cols=vars(trt), rows=vars(experiment), labeller=labeller(experiment=facet.lab.exp, trt=facet.lab.trt) ) +
 		scale_x_continuous(breaks=c(30,50,70,90)) +
		theme_bw() + 
		theme(legend.position="bottom") +
		theme(panel.grid=element_blank(), panel.grid.major.y=element_line(color="light grey", size=0.15), panel.grid.major.x=element_line(color="light grey", size=0.15)) + 
		labs(x="Days After Treatment", y="Total lesion length on each cane (cm)", color="Treatment", linetype="Treatment") +
		guides(color=guide_legend(override.aes=list(size=0.6)))
	
	ggplot2::ggsave(file="./4_results/02-3c1_disease_severity_cane-total.png", device="png", plot=plot.c1, width=6, height=6, units="in", dpi=600)

### means and statistics
	base.size=11
	plot.c2 = ggplot(summ.3) +
		geom_point(aes(x=trt, y=length_mean_plot), shape=1, size=1.5 ) +
		geom_crossbar(data=summ.4, aes(x=trt, y=length_mean_trt, ymin=length_mean_trt, ymax=length_mean_trt), width=0.3, fatten=2, color="red") +
		geom_text(data=summ.4, aes(x=trt, y=length_mean_trt+11.54, label=label_trt), size=3.25, color="red") +
		facet_grid(rows=vars(experiment), labeller=labeller(experiment=facet.lab.exp) ) +
		scale_x_discrete(expand=expansion(add=c(0.5,0.5)) ) +
		scale_y_continuous(limits=c(0,35.14), expand=expansion(mult=c(0.05,0.00)) ) + # 35.14 = max length_mean_plot + vertical text offset
		theme_bw() +
 		theme(axis.title.x=element_text(size=base.size+2, margin=margin(t=8.25)), axis.title.y=element_text(size=base.size+2, margin=margin(r=8.25)), axis.text=element_text(size=base.size) ) +
		theme(strip.text=element_text(size=base.size+1) ) +	
		labs(x="Leaf removal", y="Lesion length (cm)")
			
	ggplot2::ggsave(file="./4_results/02-3c2_disease_severity_cane-total_means.png", device="png", plot=plot.c2, width=3.25, height=5, units="in", dpi=600)

#### combined - raw, plot means over dates (note: not used, too big for paper)
#	## raw lines
#	plot.c3.1 = ggplot(data.sever) + 
#		geom_line(aes(x=days_after_trt, y=lesion_cane_sum, group=interaction(block, trt, cane)), size=0.10, alpha=0.5) + 
#		geom_line(data=summ.2, aes(x=days_after_trt, y=length_mean_trt), linetype="dashed", size=0.50) +
#  		facet_grid(cols=vars(trt), rows=vars(experiment), labeller=labeller(experiment=facet.lab.exp, trt=facet.lab.trt) ) +
# 		scale_x_continuous(breaks=c(30,50,70,90)) +
# 		scale_y_continuous(limits=c(0,110), expand=expansion(mult=c(0.05,0.00)) ) +
#		theme_bw() + 
#		theme(panel.grid=element_blank(), panel.grid.major.y=element_line(color="light grey", size=0.15), panel.grid.major.x=element_line(color="light grey", size=0.15)) + 
#		theme(strip.text.y=element_blank(), strip.background.y=element_blank() ) +
#		theme(plot.tag.position=c(0.05,0.98) ) +
#		labs(x="Days After Treatment", y="Total lesion length on each cane (cm)", color="Treatment", linetype="Treatment", tag="A") +
#		guides(color=guide_legend(override.aes=list(size=0.6)))
#
#	## right panel - plot means across dates
#	plot.c3.2 = ggplot(summ.3) +
#		geom_point(aes(x=trt, y=length_mean_plot), shape=1 ) +
#		geom_crossbar(data=summ.4, aes(x=trt, y=length_mean_trt, ymin=length_mean_trt, ymax=length_mean_trt), width=0.3, fatten=2, color="red") +
#		geom_text(data=summ.4, aes(x=trt, y=length_mean_trt+13.54, label=label_trt), size=2.75, color="red") +
#		facet_grid(rows=vars(experiment), labeller=labeller(experiment=facet.lab.exp) ) +
#		scale_x_discrete(expand=expansion(add=c(0.45,0.45)) ) +
# 		scale_y_continuous(limits=c(0,110), expand=expansion(mult=c(0.05,0.00)) ) +
#		theme_bw() +
#		theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.text.x=element_text(angle=30, hjust=1) ) +
#		theme(plot.tag.position=c(0.04,0.98) ) +
#		labs(x="Leaf removal", tag="B")
#	
#	plot.c3 = wrap_plots(plot.c3.1, plot.c3.2, nrow=1, widths=c(4,1.33) )
#		
#	ggplot2::ggsave(file="./4_results/02-3c3_disease_severity_cane-total_raw_means.png", device="png", plot=plot.c3, width=7, height=6, units="in", dpi=600)


##################################
# D. Prepare - Change in Disease #
##################################
# time course of disease: was increase in severity on last rating date from new lesions or expansion of lesions that started in first rating period?
	# visual examination of plot.c1 above suggests many canes in A,B did not increase on 2nd and 3rd dates, whereas most C canes increased

### prepare
	## add alternate date format (so when spreading or faceting, C is same as A/B)
	data.sever.2 = data.sever %>% mutate(date_alt=case_when(
		(date %in% as_date(c("2018-02-13","2018-10-17")) ) ~ "date1",
		(date %in% as_date(c("2018-03-13","2018-11-07")) ) ~ "date2",
		(date %in% as_date(c("2018-04-11","2018-11-28")) ) ~ "date3") )

	## remove other columns
	data.sever.2 = data.sever.2 %>% select(-date, -days_after_trt, -n_lesions, -status_3, -above_twine_cane_sum)

	## spread
	data.sever.2 = data.sever.2 %>% spread(key=date_alt, value=lesion_cane_sum)

### calculate change in disease
	data.sever.2 = data.sever.2 %>% mutate(
		date2_new = date2 - date1,
		date3_new = date3 - date2)

### assign first rating date that disease appeared
	## check combinations (for setting up conditions in case_when)
		# replace all >0 values with 1, then show
		data.sever.2 %>% 
			mutate(across(c(date1, date2, date3), ~ replace(.x, .x > 0, 1) ) ) %>%
			group_by(date1, date2, date3) %>% 
			summarize(ct=n() ) %>% 
			arrange(desc(date1), desc(date2), desc(date3) )
	
	## assign
	data.sever.2 = data.sever.2 %>% mutate(disease_appear=case_when(
		(is.na(date1) & is.na(date2) & is.na(date3) ) ~ "missing",
		(date1 >  0) ~ "1",
		(date1 == 0 & date2 >  0) ~ "2",
		(date1 == 0 & date2 == 0 & date3 >  0) ~ "3",
		(date1 == 0 & date2 == 0 & date3 == 0) ~ "none",
		(date1 == 0 & date2 == 0 & is.na(date3) ) ~ "missing") )		
			
		# check for missing rows
		data.sever.2 %>% filter(is.na(disease_appear) )

	## checks 
		## NAs
		data.sever.2 %>% filter(is.na(disease_appear) )
		
		## >1 disease_appear per lesion across dates
		data.sever.2 %>% group_by(experiment, trt, block, cane) %>% summarize(ct=n(), ct_diseaseapp=n_distinct(disease_appear) ) %>% filter(ct_diseaseapp > 1)

### clean up
	## remove old columns
	data.sever.2 = data.sever.2 %>% select(-date2, -date3)

	## rename columns
	data.sever.2 = data.sever.2 %>% rename(date2=date2_new, date3=date3_new)
	
	## gather
	data.sever.2 = data.sever.2 %>% gather(key="date_alt", value="lesion_cane_chg", -experiment, -trt, -block, -cane, -disease_appear)
	
	## add dates back in
	data.sever.2 = data.sever.2 %>% mutate(date=case_when(
		(experiment %in% c("A-2-row","B-3-row") & date_alt == "date1") 	 ~ as_date("2018-02-13"),
		(experiment %in% c("A-2-row","B-3-row") & date_alt == "date2") ~ as_date("2018-03-13"),
		(experiment %in% c("A-2-row","B-3-row") & date_alt == "date3") ~ as_date("2018-04-11"),
		(experiment== "C-3-row" & date_alt == "date1") 	  ~ as_date("2018-10-17"),
		(experiment== "C-3-row" & date_alt == "date2") ~ as_date("2018-11-07"),
		(experiment== "C-3-row" & date_alt == "date3") ~ as_date("2018-11-28") ) )
	
	## order columns
	data.sever.2 = data.sever.2 %>% select(experiment, trt, block, cane, disease_appear, date, date_alt, lesion_cane_chg) 
		
	## sort
	data.sever.2 = data.sever.2 %>% arrange(experiment, trt, block, cane, date)

### join back to original
	## join
	data.sever.2 = data.sever %>% left_join(data.sever.2, by=c(c("experiment","trt","block","cane","date")) )

### calculate percent of final lesion length added at each date
	## get severity on final date
		# filter	
		data.sever.fin = data.sever.2 %>% filter(date_alt == "date3")
		
		# remove unneeded columns
		data.sever.fin = data.sever.fin %>% select(-n_lesions, -date, -days_after_trt, -date_alt, -disease_appear, -lesion_cane_chg, -above_twine_cane_sum, -status_3)

		# rename column
		data.sever.fin = data.sever.fin %>% rename(lesion_cane_end=lesion_cane_sum)
	
	## join back
	data.sever.2 = data.sever.2 %>% left_join(data.sever.fin, by=c(c("experiment","trt","block","cane")) )
	
	## calculate percent added each date
	data.sever.2 = data.sever.2 %>% mutate(length_chg_perc_end=round( (lesion_cane_chg / lesion_cane_end) * 100, digits=1) )

	## change column order; omit lesion_length_end
	data.sever.2 = data.sever.2 %>% select(
		experiment, trt, block, cane, date, days_after_trt, date_alt, disease_appear, 
		lesion_cane_sum, lesion_cane_chg, length_chg_perc_end, lesion_cane_end, n_lesions, above_twine_cane_sum, status_3)
	
	## omit 0 for lesions that had not appeared yet
	data.sever.2 = data.sever.2 %>% mutate(length_chg_perc_end=replace(length_chg_perc_end, lesion_cane_sum == 0, NA) )


################
# E. Visualize #
################

### summarize
	## number of canes in each group
		# summarize
		summ.change = data.sever.2 %>%
			filter(length_chg_perc_end >= 0 & length_chg_perc_end <= 100) %>%
			group_by(experiment, date, date_alt, disease_appear) %>%
			summarize(ct=n(), length_chg_perc_end_median=median(length_chg_perc_end, na.rm=TRUE) ) %>%
			ungroup()
			
		# flag for removal
		summ.change = summ.change %>% mutate(flag_keep=case_when(
			(date_alt == "date1" & disease_appear == "1") ~ 1L,
			(date_alt == "date1" & disease_appear %in% c("2","3","missing","none") ) ~ 0L,
			(date_alt == "date2" & disease_appear %in% c("1","2") ) ~ 1L,
			(date_alt == "date2" & disease_appear %in% c(    "3","missing","none") ) ~ 0L,
			(date_alt == "date3" & disease_appear %in% c("1","2","3") ) ~ 1L,
			(date_alt == "date3" & disease_appear %in% c(        "missing","none") ) ~ 0L) )
			
		# remove missing, none
		summ.change = summ.change %>% filter(flag_keep == "1" )

### distribution - points (ignore values <0, >100)	
	## prepare
	facet.lab.exp = c('A-2-row'="Ranch 1 2-row", 'B-3-row'="Ranch 1 3-row", 'C-3-row'="Ranch 2 3-row")
	facet.lab.appear.ab = c("1"="Severity>0 first recorded on\nFeb 13", "2"="Severity>0 first recorded on\nMar 13", "3"="Severity>0\nfirst recorded\non Apr 11")
	facet.lab.appear.c = c("1"="Severity>0 first recorded on\nOct 17", "2"="Severity>0 first recorded on\nNov 7", "3"="Severity>0\nfirst recorded\non Nov 28")

	base.size=9
	## overall % of final - violin
		# A, B
		plot.e1.1 = data.sever.2 %>% filter(
			experiment %in% c("A-2-row","B-3-row") &
			disease_appear %in% c("1","2","3") & 
			(length_chg_perc_end >= 0 & length_chg_perc_end <= 100) ) %>% {
		ggplot(., aes(x=as.character(date), y=length_chg_perc_end) ) +
			geom_violin(scale="width", size=0.3) +
			stat_summary(fun=function(x) quantile(x,0.75), geom="crossbar", size=0.25, width=0.5, color="red") +
			stat_summary(fun=function(x) quantile(x,0.25), geom="crossbar", size=0.25, width=0.5, color="red") +
			stat_summary(fun=function(x) quantile(x,0.50), geom="crossbar", size=0.25, width=0.5, color="red") +
			geom_point(shape=1, color="darkgrey", position=position_jitter(w=0.20, h=0.00) ) +
			geom_text(data={ summ.change %>% filter(experiment %in% c("A-2-row","B-3-row") ) }, aes(x=as.character(date), label=ct), y=110 ) +
			facet_grid(cols=vars(disease_appear), rows=vars(experiment), scales="free_x", labeller=labeller(experiment=facet.lab.exp, disease_appear=facet.lab.appear.ab) ) +
			force_panelsizes(cols=c(3,2,1) ) +
			scale_y_continuous(breaks=c(0,25,50,75,100), expand=expansion(mult=c(0.05,0.20)) ) +
			theme_bw() +
			theme(axis.title.x=element_blank(), axis.title.y=element_text(size=base.size+2), axis.text=element_text(size=base.size) ) +
			theme(strip.text.x=element_text(size=base.size+1), strip.text.y=element_text(size=base.size+1) ) +
			labs(y="% of final lesion length added in rating period")
		}
		
		# C
		plot.e1.2 = data.sever.2 %>% filter(
			experiment == "C-3-row" &
			disease_appear %in% c("1","2","3") & 
			(length_chg_perc_end >= 0 & length_chg_perc_end <= 100) ) %>% {
		ggplot(., aes(x=as.character(date), y=length_chg_perc_end) ) +
			geom_violin(scale="width", size=0.3) +
			stat_summary(fun=function(x) quantile(x,0.75), geom="crossbar", size=0.25, width=0.5, color="red") +
			stat_summary(fun=function(x) quantile(x,0.25), geom="crossbar", size=0.25, width=0.5, color="red") +
			stat_summary(fun=function(x) quantile(x,0.50), geom="crossbar", size=0.25, width=0.5, color="red") +
			geom_point(shape=1, color="darkgrey", position=position_jitter(w=0.20, h=0.00) ) +
			geom_text(data={ summ.change %>% filter(experiment == "C-3-row") }, aes(x=as.character(date), label=ct), y=110 ) +
			facet_grid(cols=vars(disease_appear), rows=vars(experiment), scales="free_x", labeller=labeller(experiment=facet.lab.exp, disease_appear=facet.lab.appear.c) ) +
			force_panelsizes(cols=c(3,2,1) ) +
			scale_y_continuous(breaks=c(0,25,50,75,100), expand=expansion(mult=c(0.05,0.20)) ) +
			theme_bw() +
			theme(axis.title.x=element_text(size=base.size+2, margin=margin(t=8.25, b=-2.75) ), axis.title.y=element_text(size=base.size+2), axis.text=element_text(size=base.size) ) +
			theme(strip.text=element_text(size=base.size+1), strip.text.y=element_text(size=base.size+1) ) +
			labs(x="Rating date", y="% of final lesion length\nadded in rating period")
		}
		
		plot.e1 = wrap_plots(plot.e1.1, plot.e1.2, ncol=1, heights=c(2.075,1) ) 
		
		ggplot2::ggsave(file="./4_results/02-3e1_disease_severity_cane_chg_perc-total_overall.png", device="png", plot=plot.e1, width=7, height=7, units="in", dpi=600)

	## overall raw - violin 
		# A, B
		plot.e2.1 = data.sever.2 %>% filter(
			experiment %in% c("A-2-row","B-3-row") &
			disease_appear %in% c("1","2","3") ) %>% {
		ggplot(., aes(x=as.character(date), y=lesion_cane_chg) ) +
			geom_violin(scale="width", size=0.3) +
			stat_summary(fun=function(x) quantile(x,0.75), geom="crossbar", size=0.25, width=0.5, color="red") +
			stat_summary(fun=function(x) quantile(x,0.25), geom="crossbar", size=0.25, width=0.5, color="red") +
			stat_summary(fun=function(x) quantile(x,0.50), geom="crossbar", size=0.25, width=0.5, color="red") +
			geom_point(shape=1, color="darkgrey", position=position_jitter(w=0.20, h=0.00) ) +
			geom_text(data={ summ.change %>% filter(experiment %in% c("A-2-row","B-3-row") ) }, aes(x=as.character(date), label=ct), y=110 ) +
			facet_grid(cols=vars(disease_appear), rows=vars(experiment), scales="free_x", labeller=labeller(experiment=facet.lab.exp, disease_appear=facet.lab.appear.ab) ) +
			force_panelsizes(cols=c(3,2,1) ) +
#			scale_y_continuous(breaks=c(0,25,50,75,100), expand=expansion(mult=c(0.05,0.20)) ) +
			theme_bw() +
			theme(axis.title.x=element_blank(), axis.title.y=element_text(size=base.size+2), axis.text=element_text(size=base.size) ) +
			theme(strip.text.x=element_text(size=base.size+1), strip.text.y=element_text(size=base.size+1) ) +
			labs(y="Lesion length (cm) added in rating period")
		}
		
		# C
		plot.e2.2 = data.sever.2 %>% filter(
			experiment == "C-3-row" &
			disease_appear %in% c("1","2","3") ) %>% {
		ggplot(., aes(x=as.character(date), y=lesion_cane_chg) ) +
			geom_violin(scale="width", size=0.3) +
			stat_summary(fun=function(x) quantile(x,0.75), geom="crossbar", size=0.25, width=0.5, color="red") +
			stat_summary(fun=function(x) quantile(x,0.25), geom="crossbar", size=0.25, width=0.5, color="red") +
			stat_summary(fun=function(x) quantile(x,0.50), geom="crossbar", size=0.25, width=0.5, color="red") +
			geom_point(shape=1, color="darkgrey", position=position_jitter(w=0.20, h=0.00) ) +
			geom_text(data={ summ.change %>% filter(experiment == "C-3-row") }, aes(x=as.character(date), label=ct), y=110 ) +
			facet_grid(cols=vars(disease_appear), rows=vars(experiment), scales="free_x", labeller=labeller(experiment=facet.lab.exp, disease_appear=facet.lab.appear.c) ) +
			force_panelsizes(cols=c(3,2,1) ) +
#			scale_y_continuous(breaks=c(0,25,50,75,100), expand=expansion(mult=c(0.05,0.20)) ) +
			theme_bw() +
			theme(axis.title.x=element_text(size=base.size+2, margin=margin(t=8.25, b=-2.75) ), axis.title.y=element_text(size=base.size+2), axis.text=element_text(size=base.size) ) +
			theme(strip.text=element_text(size=base.size+1), strip.text.y=element_text(size=base.size+1) ) +
			labs(x="Rating date", y="Lesion length (cm) added in rating period")
		}
		
		plot.e2 = wrap_plots(plot.e2.1, plot.e2.2, ncol=1, heights=c(2.075,1) ) 
		
		ggplot2::ggsave(file="./4_results/z_02-3e2_disease_severity_cane_chg_raw_overall_not-shown.png", device="png", plot=plot.e2, width=7, height=7, units="in", dpi=600)
