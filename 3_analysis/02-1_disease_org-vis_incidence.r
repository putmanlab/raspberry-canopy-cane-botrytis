###########################
# RASPBERRY Cane Botrytis #
# Organize and Visualize  #
# Disease Incidence		  #
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

conflict_prefer("date", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("spread", "tidyr")

setwd("/home/raspb_botrytis")


##########################
# A. Import and Organize #
##########################

### import
	## run 1
	in.r1.canes = read_csv(file="./2_data/Rasp_CaneB_Run1_data - canes.csv", col_names=T, na=".")
	in.r1.infcn = read_csv(file="./2_data/Rasp_CaneB_Run1_data - incidence.csv", col_names=T, na=".")
	
	## run 2
	in.r2.canes = read_csv(file="./2_data/Rasp_CaneB_Run2_data-canes.csv", col_names=T)
	in.r2.infcn = read_csv(file="./2_data/Rasp_CaneB_Run2_data-incidence.csv", col_names=T)
	
### organize
	## run 1
		# join cane counts with infected cane counts
		r1.incid = in.r1.canes %>% select(-date) %>% right_join(in.r1.infcn, by=c("tunnel" = "tunnel", "trt" = "trt", "block" = "block"))
	
		# reorder columns
		r1.incid = r1.incid %>% select(date, tunnel, trt, block, canes, infected_canes)
	
		# calculate % incidence
		r1.incid = r1.incid %>% mutate(incid=round(((infected_canes/canes)*100), digits=1))
		
		# add days_after_trt column
		r1.incid = r1.incid %>% mutate(days_after_trt=date-as_date("2018-01-05"))

	## run 2 
		# remove unneeded columns; east/west will be ignored in initial analyses because first run experiments do not have that info
		in.r2.canes = in.r2.canes %>% select(-date, -east, -west)
		in.r2.infcn = in.r2.infcn %>% select(-date_mm_dd, -east, -west)
		
		# join
		r2.incid = in.r2.infcn %>% left_join(in.r2.canes, by=c("tunnel" = "tunnel", "trt" = "trt", "block" = "block"))
		
		# calculate incidence
		r2.incid = r2.incid %>% mutate(incid=round(((infected_canes/canes)*100), digits=1))					

		# add days_after_trt column
		r2.incid = r2.incid %>% mutate(days_after_trt=date-as_date("2018-09-25"))

### bind runs together 
	## add 'experiment' column to distinguish between experimental runs
		# run 1
		r1.incid = r1.incid %>% mutate(experiment=as.character(NA))

			# fill experiment column
			r1.incid = r1.incid %>% mutate(experiment=replace(experiment, tunnel == "2-row", "A-2-row"))
			r1.incid = r1.incid %>% mutate(experiment=replace(experiment, tunnel == "3-row", "B-3-row"))

		# run 2; add column and fill
		r2.incid = r2.incid %>% mutate(experiment="C-3-row")

	## bind
	data.incid = bind_rows(r1.incid, r2.incid)
			
	## change order of columns and discard tunnel column
	data.incid = data.incid %>% select(experiment, trt, block, date, days_after_trt, canes, infected_canes, incid)
	
	## change days_after_trt to integer
	data.incid = data.incid %>% mutate(days_after_trt=as.integer(days_after_trt))
	
	## sort dataset
	data.incid = data.incid %>% arrange(experiment, trt, block, date)
	
### export final curated data
	write_csv(data.incid, file="./2_data_curated/rasp-caneb_02-1_disease_incidence_final.csv", na=".", append=F, col_names=T)
	

##############################
# B. Visualize and Summarize #
##############################

### summarize
	## calculate numerical mean in data scale
	incid.summ = data.incid %>% group_by(experiment, date, days_after_trt, trt) %>% summarize(incid_mean=round(mean(incid, na.rm=T), digits=1), incid_min=min(incid, na.rm=T), incid_max=max(incid, na.rm=T)) %>% ungroup()

	## change to factor for table order
	incid.summ = incid.summ %>% mutate(trt=fct_relevel(trt, c("control","blade","manual","twine")))
	
	## for table
		# gather summary variables
		incid.summ.tbl = incid.summ %>% select(-days_after_trt) %>% gather(key="measure", value="value", -experiment, -date, -trt)
		
		# reformat table - join columns
		incid.summ.tbl = incid.summ.tbl %>% unite("experiment_date", c("experiment", "date"))
			
		# reformat table - spread
		incid.summ.tbl = incid.summ.tbl %>% spread(key=experiment_date, value=value)
		
		# fix measure column 
			# remove "incid_"
			incid.summ.tbl = incid.summ.tbl %>% mutate(measure=str_replace(measure, "incid_", ""))
		
			# reorder
			incid.summ.tbl = incid.summ.tbl %>% mutate(measure=fct_relevel(measure, c("mean","min","max")))
		
		# arrange trts
		incid.summ.tbl = incid.summ.tbl %>% arrange(measure, trt)
		
		# write files
		write_delim(incid.summ.tbl, file="./4_results/02-1_summary-table_disease_incidence.csv", delim="\t", na="-")

### summarize for text
	## Results - numerical summary - range
	data.incid %>% 
		filter(date %in% as_date(c("2018-04-11","2018-11-28") ) ) %>%
		group_by(experiment, date) %>% 
		summarize(incid_min=min(incid, na.rm=TRUE), incid_max=max(incid, na.rm=TRUE) )
		
	## Discussion - canes per meter
	data.incid %>%
		group_by(experiment, trt, block) %>%
		summarize(canes=mean(canes, na.rm=FALSE) ) %>%
		ungroup() %>%
		mutate(canes_per_m = canes / 4.9) %>%
		group_by(experiment) %>%
		summarize(canes_m_mean=mean(canes_per_m, na.rm=TRUE), canes_m_median=median(canes_per_m, na.rm=TRUE), canes_m_min=min(canes_per_m, na.rm=TRUE), canes_m_max=max(canes_per_m, na.rm=TRUE) )
	
### plot all three experiments using days after treatment
	## make character vectors to rename facet labels
	facet.lab.exp = c('A-2-row'="Ranch 1 2-row", 'B-3-row'="Ranch 1 3-row", 'C-3-row'="Ranch 2 3-row")
	facet.lab.trt = c('blade'="Blade", 'manual'="Manual", 'twine'="Twine", 'control'="Control")

	## change order of treatment factors
	data.incid.t2 = data.incid %>% mutate(trt=fct_relevel(trt, c("blade","manual","twine","control")))

	## make plot
	plot.incid.f = ggplot(data.incid.t2) + 
  		geom_line(aes(x=days_after_trt, y=incid, group=interaction(block,trt)), size=0.17, alpha=0.6) + 
  		geom_line(data=incid.summ, aes(x=days_after_trt, y=incid_mean), linetype="dashed", size=0.5) +
  		facet_grid(experiment ~ trt, labeller=labeller(experiment=facet.lab.exp, trt=facet.lab.trt)) + 
  		theme_bw() +
  		scale_x_continuous(breaks=c(30,50,70,90)) +
    	theme(legend.position="bottom") +
  		theme(panel.grid=element_blank(), panel.grid.major.y=element_line(color="light grey", size=0.15), panel.grid.major.x=element_line(color="light grey", size=0.15)) + 
  		labs(x="Days After Treatment", y="Incidence (% canes with >= 1 lesion)", color="Treatment", linetype="Treatment")
  		
	ggplot2::ggsave(file="./4_results/02-1_disease_incidence_line.png", device="png", plot=plot.incid.f, width=6, height=5, units="in", dpi=960)


