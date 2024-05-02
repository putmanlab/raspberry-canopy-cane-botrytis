###########################
# RASPBERRY Cane Botrytis #
# Organize and Visualize  #
# Receptacles			  #
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
	## receptacle
	in.r1.recep = read_csv(file="./2_data/Rasp_CaneB_Run1_data - receptacle.csv", col_names=T, na=".")
	in.r2.recep = read_csv(file="./2_data/Rasp_CaneB_Run2_data-receptacles.csv", col_names=T, na=".")
	
	## cane counts
	in.r1.canes = read_csv(file="./2_data/Rasp_CaneB_Run1_data - canes.csv", col_names=T, na=".")
	in.r2.canes = read_csv(file="./2_data/Rasp_CaneB_Run2_data-canes.csv", col_names=T)
	
### organize			
	## gather
	r1.recep = in.r1.recep %>% gather(key="cane", value="n_receps", -date, -block, -tunnel, -trt)
	r2.recep = in.r2.recep %>% gather(key="cane", value="n_receps", -date, -block, -tunnel, -trt)
	
	## remove "receps_cane_"
	r1.recep = r1.recep %>% mutate(cane=str_replace(cane, "receps_cane_", ""))
	r2.recep = r2.recep %>% mutate(cane=str_replace(cane, "receps_cane_", ""))
	
	## convert to integer
	r1.recep = r1.recep %>% mutate(cane=as.integer(cane))
	r2.recep = r2.recep %>% mutate(cane=as.integer(cane))
	
	## join cane counts
	r1.recep = r1.recep %>% left_join(in.r1.canes, by=c("tunnel"="tunnel", "trt"="trt", "block"="block"))
	r2.recep = r2.recep %>% left_join(in.r2.canes, by=c("tunnel"="tunnel", "trt"="trt", "block"="block"))
	
	## remove and rename date columns
	r1.recep = r1.recep %>% select(-date.y) %>% rename(date=date.x)
	r2.recep = r2.recep %>% select(-date.y) %>% rename(date=date.x)
	
	## rename canes column; remove east,west cane count column
	r1.recep = r1.recep %>% rename(n_canes=canes)
	r2.recep = r2.recep %>% rename(n_canes=canes) %>% select(-east, -west)
	
	## change block to character
	r1.recep = r1.recep %>% mutate(block=as.character(block))
	r2.recep = r2.recep %>% mutate(block=as.character(block))

### bind runs together 
	## add 'experiment' column to distinguish between experimental runs
		# run 1
		r1.recep = r1.recep %>% mutate(experiment=as.character(NA))

			# fill experiment column
			r1.recep = r1.recep %>% mutate(experiment=replace(experiment, tunnel == "2-row", "A-2-row"))
			r1.recep = r1.recep %>% mutate(experiment=replace(experiment, tunnel == "3-row", "B-3-row"))

		# run 2; add column and fill
		r2.recep = r2.recep %>% mutate(experiment="C-3-row")

	## bind
	data.recep = bind_rows(r1.recep, r2.recep)
			
	## change order of columns and discard tunnel column
	data.recep = data.recep %>% select(experiment, trt, block, cane, n_receps, n_canes)
	
	## sort dataset
	data.recep = data.recep %>% arrange(experiment, trt, block, cane)

### adjust based on plot cane density
	data.recep = data.recep %>% mutate(receps_per_cane=round( (n_receps/n_canes), digits=1) )

### export final curated data
	write_csv(data.recep, file="./2_data_curated/rasp-caneb_02-5_receptacles_final.csv", na=".", append=F, col_names=T)
	

##############################
# B. Visualize and Summarize #
##############################

### summarize
	## calculate numerical mean in data scale
	recep.summ = data.recep %>% group_by(experiment, trt) %>% summarize(receps_mean=round(mean(n_receps, na.rm=T), digits=0), receps_min=min(n_receps, na.rm=T), receps_max=max(n_receps, na.rm=T))
		
	## write files
	write_delim(recep.summ, file="./4_results/02-5_receptacles_summary-table.csv", delim="\t", na="-")

### prepare	
	## make character vectors to rename facet labels
	facet.lab.exp = c('A-2-row'="Ranch 1 2-row", 'B-3-row'="Ranch 1 3-row", 'C-3-row'="Ranch 2 3-row")

	## change order of treatment factors
	data.recep.t = data.recep %>% mutate(trt=fct_relevel(trt, c("blade","manual","twine","control")))

### boxplot (note: not used, violin plot is better)
#	plot.b.1 = ggplot(data.recep.t, aes(y=n_receps, x=trt)) +
#		geom_point(shape=1, color="darkgrey", position=position_jitter(w=0.1, h=0.1)) + 
#		geom_boxplot(outlier.shape=NA, fill=NA) +
#		facet_grid(. ~ experiment, labeller=labeller(experiment=facet.lab.exp)) +
#		scale_x_discrete(labels=c("Blade","Manual","Twine","Control")) +
#		theme_bw() +
#		theme(axis.title=element_text(size=13), axis.text=element_text(size=12), strip.text=element_text(size=13)) +
#		theme(panel.grid=element_blank(), panel.grid.major.y=element_line(color="light grey", size=0.15), panel.grid.major.x=element_line(color="light grey", size=0.15)) +
#		labs(x="Treatment", y="Number of receptacles per cane", color="Treatment")
#
#	ggsave(file="./4_results/02-5b1_receptacle_box.png", device="png", plot=plot.b.1, width=8, height=8, units="in")
	
### violin
	plot.b.2 = ggplot(data.recep.t, aes(y=n_receps, x=trt)) +
		geom_violin(scale="count", draw_quantiles=c(0.25, 0.5, 0.75), size=0.3) +
		geom_point(shape=1, color="darkgrey", position=position_jitter(w=0.15, h=0.15)) + 
		facet_grid(. ~ experiment, labeller=labeller(experiment=facet.lab.exp)) +
		scale_x_discrete(labels=c("Blade","Manual","Twine","Control")) +
		theme_bw() +
		theme(axis.title=element_text(size=13), axis.text=element_text(size=12), strip.text=element_text(size=13)) +
		theme(panel.grid=element_blank(), panel.grid.major.y=element_line(color="light grey", size=0.15), panel.grid.major.x=element_line(color="light grey", size=0.15)) +
		labs(x="Treatment", y="Number of receptacles on each cane", color="Treatment")

	ggsave(file="./4_results/02-5b2_receptacle-raw_treatment.png", device="png", plot=plot.b.2, width=8, height=8, units="in")
		

####################################################
# C. Visualize and Summarize - Cane density - Mean #
####################################################

#### summarize
#	## calculate 
#	data.recep.plot = data.recep %>%
#		group_by(experiment, trt, block, n_canes) %>% 
#		summarize(receps_avg=mean(n_receps, na.rm=TRUE), n_cane_sample=n(), stddev=sd(n_receps, na.rm=TRUE) ) %>%
#		ungroup()
#	
#	## calculate standard error
#	data.recep.plot = data.recep.plot %>% mutate(std_error=round( (stddev/(n_cane_sample^0.5)), digits=1) )
#
#### scatter
#	plot.c.3 = ggplot(data.recep.plot, aes(y=receps_avg, x=n_canes) ) +
#		geom_point(size=1, shape=1) +
#		geom_linerange(aes(ymin=receps_avg-std_error, ymax=receps_avg+std_error) ) +
#		facet_grid(cols=vars(trt), rows=vars(experiment) ) +
#		theme_bw()
#		
#	ggsave(file="./4_results/02-5c3_receptacle-avg_treatment_vs-plot-density_scatter.png", device="png", plot=plot.c.3, width=8, height=8, units="in")

		# results: no consistent trends between plot cane density and number of receptacles