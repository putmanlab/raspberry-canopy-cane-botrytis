* ********************************************* *
* RASPBERRY Cane Botrytis						*	 
* Environmental Observations      		  		*
* Rating date - Epidemiology Summary Statistics *
* Time of temperature 15 < t < 25	     		*
* ********************************************* *;

*** set variables for folder locations and base filename used for saving output
	** local to lab desktop;
*	%LET base_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\Raspb_botrytis\SAS_raspb;
*	%LET results_path=&base_path.\4_results\environ-epi-rating-temp1525;
*	%LET results_path_img=&base_path.\4_results\environ-epi-rating-temp1525\html_images;

	** to SAS ODA/SAS Studio;
	%LET base_path=/home/u63629950/raspb-botrytis;
	%LET results_path=&base_path./4_results/environ-epi-rating-temp1525;
	%LET results_path_img=&base_path./4_results/environ-epi-rating-temp1525/html_images;

	** both;
	%LET name_base=environ-epi-rating-temp1525_; 

*** load macros for controlling output;
	** local;
*	%include "&base_path.\3_analysis\output_files_macro.sas";
	
	** SAS Studio;
	%include "&base_path./3_analysis/output_files_macro.sas";

*options ls=120 nonumber formdlim=' ' pagesize=52 center;


* ********* *
* A. Import
* ********* *;

** save log to file;
	* local;
*	proc printto new log="&results_path\&name_base.A_sas-log.log"; run; 
	
	* SAS Studio;
	proc printto new log="&results_path/&name_base.A_sas-log.log"; run; 

** import data;
	* local;
*	proc import 
*			datafile="&base_path.\2_data\rasp-caneb_06-3b_environ_summ-epi_rating_SAS.csv"
*			dbms=dlm replace out=environ_epi_rating;
*		delimiter=",";
*		getnames=YES;
*	run;
			
	* SAS ODA/SAS Studio;
	proc import 
			datafile="&base_path./2_data/rasp-caneb_06-3b_environ_summ-epi_rating_SAS.csv"
			dbms=dlm replace out=environ_epi_rating;
		delimiter=",";
		getnames=YES;
	run;

** sort dataset
	* by rating for 'by group' processing in proc glimmix;
	* good practice to sort dataset in same order that variables appear in class statement;
proc sort data=environ_epi_rating;
	by experiment treatment block date_rating;
run;

** check variable types;
proc contents data=environ_epi_rating;
run;

proc printto; run; * direct log back to SAS Log window;


* ******************************** *
* B. Analyze - Identify Base Model *
* ******************************** *;

*** Step 1 ***;
	** OBJ: most simple model;
	
	** Step 1-1 **;
		* OBJ: base model;
		* see Appendix B for code
		* RESULTS: 
		* CONCLUSION: test time factors as fixed effects


*** Step 2 ***;
	** OBJ: test time factor as fixed;
	
	** Step 2-1 **;
		* OBJ: date_rating;
		* see Appendix B for code

	** RESULTS: significant improvement in fit
	** CONCLUSION: test interactive effect


*** Step 3 ***;
	** OBJ: test date_rating as interactive effect;

	** Step 3-1 **;
		* OBJ: treatment*date_rating interaction;
			* note: commented out and not moved to appendix because is earlier version of final model;
*		%output_html_files_on(name_step=B_step-3-1, title_step=B step 3-1); * redirect html, log output to files;
*		proc glimmix data=environ_epi_rating plot=residualpanel method=laplace;
*			class experiment treatment date_rating;
*			by experiment;
*			model temp_time1525 = treatment|date_rating / dist=beta link=logit htype=3;
*			title 'rcb env epidem-summ day temp-time1525 B id base model - step 3 rating date - step 3-1';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
		
	** RESULTS: small decline in fit;
	** CONCLUSION: try random block


*** Step 4 ***;
	** OBJ: test random block;

	** Step 4-1 **;
		* OBJ: random block (from 2-1);
		* see Appendix B for code
		
		* RESULTS: est G matrix error all experiments;
			* cov test > 0.05 all experiments

	** Step 4-2 **;
		* OBJ: random block (from 2-1);
		* see Appendix B for code
		* RESULTS: intercept cov param estimates missing 
			* cov test > 0.05 all experiments

	** Step 4-3 **;
		* OBJ: random block (from 3-1);
		* see Appendix B for code		
		* RESULTS: intercept cov param estimates missing
			* cov test > 0.05 all experiments

	** CONCLUSION: do not include random block


* ****************************** *
* C. Analyze - Repeated Measures *
* ****************************** *;

*** Step 1 ***;
	** OBJ: repeated measures with date_rating;

	** Step 1-1 **;
		* OBJ: autoregressive;
		* see Appendix B for code
		
	** RESULTS: missing covariance parameter estimates
	** CONCLUSIONS: do not use repeated measures	


* *************************** *
* D. Analyze - Fixed effects  *
* *************************** *;

*** Step 1 ***;
	** OBJ: update settings for final analysis;
		* note: treatment|date_rating (B step 3-1) chosen vs "treatment date_rating" (B step 2-1) because raw data suggests relationship may not be constant across rating dates
		
	** Step 1-1 **;
		* OBJ: base model (B step 3-1);
		%output_html_files_on(name_step=D_step-1-1, title_step=D step 1-1); * redirect html, log output to files;
		proc glimmix data=environ_epi_rating plot=residualpanel;
			class experiment treatment date_rating;
			by experiment;
			model temp_time1525 = treatment|date_rating / dist=beta link=logit htype=3;
			title 'rcb env epidem-summ day temp-time1525 D update settings - step 1 update settings - step 1-1 base model';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;	
		
	** RESULTS: 
		* A: all effects, F values are infinity
		* B: all effects, F values are infinity
		* C: treatment, date_rating
	** CONCLUSION: A,B results are spurious, no evidence from raw data that results could be so strongly significant
		* examine C


* **************************************** *
* E. Analyze - Examine significant effects *
* **************************************** *;

*** make dataset;
	data environ_epi_rating_C;
		set environ_epi_rating;
		if experiment in ('A-2-row','B-3-row') then delete;
	run;

*** Step 1 ***;
	** OBJ: examine means;
		
	** Step 1-1 **;
		* OBJ: base model (B step 3-1);
		%output_html_files_on(name_step=E_step-1-1, title_step=E step 1-1); * redirect html, log output to files;
		proc glimmix data=environ_epi_rating_C plot=residualpanel;
			class experiment treatment date_rating;
			by experiment;
			model temp_time1525 = treatment|date_rating / dist=beta link=logit htype=3;
			
			lsmeans treatment / ilink lines adjust=tukey adjdfe=row;

			ods exclude MeanPlot SliceDiffs DiffPlot;
			title 'rcb env epidem-summ day temp-time1525 E examine - step 1 means - step 1-1';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;	

		* RESULTS: blade b, control ab, manual ab, twine a
			* when A,B tried, estimates for all pairwise comparisons are missing
	
	** CONCLUSION: analysis complete



* --------------------APPENDICES-------------------- *;



* ***************************************** *
* Appendix B. Analyze - Identify Base Model *
* ***************************************** *;

*** Step 1 ***;
	** OBJ: most simple model;
	
	** Step 1-1 **;
		* OBJ: base model;
*		%output_html_files_on(name_step=B_step-1-1, title_step=B step 1-1); * redirect html, log output to files;
*		proc glimmix data=environ_epi_rating plot=residualpanel method=laplace;
*			class experiment treatment;
*			by experiment;
*			model temp_time1525 = treatment / dist=beta link=logit htype=3;
*			title 'rcb env epidem-summ day temp-time1525 B id base model - step 1 most simple model - step 1-1';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: 
*		* CONCLUSION: test time factors as fixed effects
*
*
**** Step 2 ***;
*	** OBJ: test time factor as fixed;
*	
*	** Step 2-1 **;
*		* OBJ: date_rating;
*		%output_html_files_on(name_step=B_step-2-1, title_step=B step 2-1); * redirect html, log output to files;
*		proc glimmix data=environ_epi_rating plot=residualpanel method=laplace;
*			class experiment treatment date_rating;
*			by experiment;
*			model temp_time1525 = treatment date_rating / dist=beta link=logit htype=3;
*			title 'rcb env epidem-summ day temp-time1525 B id base model - step 2 time fixed - step 2-1';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** RESULTS: significant improvement in fit
*	** CONCLUSION: test interactive effect
*	
*
**** Step 4 ***;
*	** OBJ: test random block;
*
*	** Step 4-1 **;
*		* OBJ: random block (from 2-1);
*		%output_html_files_on(name_step=B_step-4-1, title_step=B step 4-1); * redirect html, log output to files;
*		proc glimmix data=environ_epi_rating plot=residualpanel method=laplace;
*			class experiment treatment block;
*			by experiment;
*			model temp_time1525 = treatment / dist=beta link=logit htype=3;
*			random intercept / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env epidem-summ day temp-time1525 B id base model - step 4 random block - step 4-1 no block (1-1)';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;	
*		
*		* RESULTS: est G matrix error all experiments;
*			* cov test > 0.05 all experiments
*
*	** Step 4-2 **;
*		* OBJ: random block (from 2-1);
*		%output_html_files_on(name_step=B_step-4-2, title_step=B step 4-2); * redirect html, log output to files;
*		proc glimmix data=environ_epi_rating plot=residualpanel method=laplace;
*			class experiment treatment block date_rating;
*			by experiment;
*			model temp_time1525 = treatment date_rating / dist=beta link=logit htype=3;
*			random intercept / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env epidem-summ day temp-time1525 B id base model - step 4 random block - step 4-2 separate (2-1)';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;	
*		
*		* RESULTS: intercept cov param estimates missing 
*			* cov test > 0.05 all experiments
*
*	** Step 4-3 **;
*		* OBJ: random block (from 3-1);
*		%output_html_files_on(name_step=B_step-4-3, title_step=B step 4-3); * redirect html, log output to files;
*		proc glimmix data=environ_epi_rating plot=residualpanel method=laplace;
*			class experiment treatment block date_rating;
*			by experiment;
*			model temp_time1525 = treatment|date_rating / dist=beta link=logit htype=3;
*			random intercept / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env epidem-summ day temp-time1525 B id base model - step 4 random block - step 4-3 interaction (3-1)';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;	
*		
*		* RESULTS: intercept cov param estimates missing
*			* cov test > 0.05 all experiments
*
*	** CONCLUSION: do not include random block


* *************************************** *
* Appendix C. Analyze - Repeated Measures *
* *************************************** *;

*** Step 1 ***;
	** OBJ: repeated measures with date_rating;

	** Step 1-1 **;
		* OBJ: autoregressive;
*		%output_html_files_on(name_step=C_step-1-1, title_step=C step 1-1); * redirect html, log output to files;
*		proc glimmix data=environ_epi_rating plot=residualpanel method=laplace;
*			class experiment treatment block date_rating;
*			by experiment;
*			model temp_time1525 = treatment / dist=beta link=logit htype=3;
*			random date_rating / type=ar(1) subject=treatment*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env epidem-summ day temp-time1525 C repeated measures - step 1 date_rating - step 1-1 ar(1)';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;	
		
	** RESULTS: missing covariance parameter estimates
	** CONCLUSIONS: do not use repeated measures	
