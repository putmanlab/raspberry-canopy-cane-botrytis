###########################
# RASPBERRY Cane Botrytis #
# Design				  #
# Run 2					  #
###########################

## built on Docker putmanlab/field-exp-analysis-docker

library(dplyr)
library(agricolae)

directory="/home/raspb_botrytis"
setwd(directory)

#############################
# A. Run 2 - Camarillo 2018 #
#############################

## set up trial parameters
trt.r2 = c("blade","manual","twine","control")
reps.r2 = 5
units.r2 = reps.r2 * length(trt.r2)

## randomization
	# set random number seed *** !!! USE DIFFERENT SEED EACH TIME !!! ***
	# obtained from blind selection of random number in "A Million Random Digits With 100,000 Normal Deviates" by RAND Corporation
	seed.r2 = 77711
	
	# get randomization
	des.r2 = design.rcbd(trt=trt.r2, r=reps.r2, seed=seed.r2, kinds="Super-Duper", first=TRUE, continue=FALSE)
	
	# change column name
	des.r2.out = des.r2$book %>% rename("treatment"=trt.r2)
	
	# same seed = same randomization, every time; append seed and datetime to filename to prevent accidentally overwriting randomization
	write.table(des.r2.out, file=paste("raspb_caneb_run-2_randomized_", "seed-", seed.r2, "_", format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), ".txt", sep=""), row.names=F, col.names=T, quote=F, sep="\t")