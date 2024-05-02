#######################################
# RASPBERRY Cane Botrytis 			  #
# Define Graph Settings				  #
#######################################

###
### all files
###

# z_05-3_environ_summ-vis_rating_not-shown.r, 06-3_environ_epi-summ_org-vis_rating.r, 
# 07-1_dis-env_signif-effect_vis.r, 09-1_multivar_vis_end.r, 10_environ_summ-experiment_org-vis.r
# 11_weather_org-vis.r

### manual scales 
	# leaf removal treatments
	vec.trt.shape = c(1,0,2,8)
	vec.trt.color = scales::hue_pal()(10)[c(2,5,8,10)]
	
	# rating date/period
	vec.rat.shape = c(5,6,3)
	vec.rat.color = scales::hue_pal()(10)[c(1,3,7)]
	
	# experiment
	vec.exp.shape = c(4,9,10) 
	vec.exp.color = scales::hue_pal()(10)[c(4,6,9)]
	vec.exp.line = c("12223242","solid","22")
	
	# period
	vec.per.shape = c(13,14) 
	vec.per.color = c("black","grey50")

### to show colors
	# scales::show_col(vec.exp.color)
