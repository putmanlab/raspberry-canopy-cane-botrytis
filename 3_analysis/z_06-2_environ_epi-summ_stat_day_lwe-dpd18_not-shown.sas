* *************************************** *
* RASPBERRY Cane Botrytis 		  		  *	 
* Environmental Observations      		  *
* Daily - Epidemiology Summary Statistics *
* Estimated LW - dew point depress 1.8    *
* *************************************** *;

*** set variables for folder locations and base filename used for saving output
	** local to lab desktop;
*	%LET base_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\Raspb_botrytis\SAS_raspb;
*	%LET results_path=&base_path.\4_results\environ-epi-day-dpd18;
*	%LET results_path_img=&base_path.\4_results\environ-epi-day-dpd18\html_images;

	** to SAS ODA/SAS Studio;
	%LET base_path=/home/u63629950/raspb-botrytis;
	%LET results_path=&base_path./4_results/environ-epi-day-dpd18;
	%LET results_path_img=&base_path./4_results/environ-epi-day-dpd18/html_images;

	** both;
	%LET name_base=environ-epi-day-dpd18_; 

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
*			datafile="&base_path.\2_data\rasp-caneb_07b_environ_summ-epi_day_proportion.csv"
*			dbms=dlm replace out=environ_epi_day;
*		delimiter=",";
*		getnames=YES;
*	run;
			
	* SAS ODA/SAS Studio;
	proc import 
			datafile="&base_path./2_data/rasp-caneb_07b_environ_summ-epi_day_proportion.csv"
			dbms=dlm replace out=environ_epi_day;
		delimiter=",";
		getnames=YES;
	run;

** sort dataset
	* by rating for 'by group' processing in proc glimmix;
	* good practice to sort dataset in same order that variables appear in class statement;
proc sort data=environ_epi_day;
	by experiment treatment block date date_rating;
run;

** check variable types;
proc contents data=environ_epi_day;
run;

proc printto; run; * direct log back to SAS Log window;


* ******************************** *
* B. Analyze - Identify Base Model *
* ******************************** *;

*** Step 1 ***;
	** OBJ: most simple model;
	
	** Step 1-1 **;
		* OBJ: base model;
		%output_html_files_on(name_step=B_step-1-1, title_step=B step 1-1); * redirect html, log output to files;
		proc glimmix data=environ_epi_day plot=residualpanel method=laplace;
			class experiment treatment;
			by experiment;
			model lwe_dpd18 = treatment / dist=beta link=logit htype=3;
			title 'rcb env epidem-summ day lwe-dpd18 B id base model - step 1 most simple model - step 1-1 base';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;

		* RESULTS: extremely high pearson/df, no signif
		* CONCLUSION: test time factors as fixed effects


*** Step 2 ***;
	** OBJ: test time factor(s) as fixed;
	
	** Step 2-1 **;
		* OBJ: date;
		%output_html_files_on(name_step=B_step-2-1, title_step=B step 2-1); * redirect html, log output to files;
		proc glimmix data=environ_epi_day plot=residualpanel method=laplace;
			class experiment treatment date;
			by experiment;
			model lwe_dpd18 = treatment date / dist=beta link=logit htype=3;
			title 'rcb env epidem-summ day lwe-dpd18 B id base model - step 2 time fixed - step 2-1 date';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;

	** Step 2-2 **;
		* OBJ: date_rating;
		%output_html_files_on(name_step=B_step-2-2, title_step=B step 2-2); * redirect html, log output to files;
		proc glimmix data=environ_epi_day plot=residualpanel method=laplace;
			class experiment treatment date_rating;
			by experiment;
			model lwe_dpd18 = treatment date_rating / dist=beta link=logit htype=3;
			title 'rcb env epidem-summ day lwe-dpd18 B id base model - step 2 time fixed - step 2-2 date_rating';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS:
		* date: significant improvement in fit
		* date_rating: modest improvement
	** CONCLUSION: test interactive effect


*** Step 3 ***;
	** OBJ: test date_rating as interactive effect;

	** Step 3-1 **;
		* OBJ: treatment*date_rating interaction;
		%output_html_files_on(name_step=B_step-3-1, title_step=B step 3-1); * redirect html, log output to files;
		proc glimmix data=environ_epi_day plot=residualpanel method=laplace;
			class experiment treatment date_rating;
			by experiment;
			model lwe_dpd18 = treatment|date_rating / dist=beta link=logit htype=3;
			title 'rcb env epidem-summ day lwe-dpd18 B id base model - step 3 rating date - step 3-1 treatment*date_rating';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;
		
		* RESULTS: slight decline in fit
		* note: following models tried, but when used to examine means, lsmeans return non-est errors that are only fixed by removing date	
			* model lwe_dpd18 = treatment date_rating*date / dist=beta link=logit htype=3;
			* model lwe_dpd18 = treatment|date_rating date_rating*date / dist=beta link=logit htype=3;
	
	** CONCLUSION: try random block


*** Step 4 ***;
	** OBJ: test random block;

	** Step 4-1 **;
		* OBJ: random block (from 2-2);
		%output_html_files_on(name_step=B_step-4-1, title_step=B step 4-1); * redirect html, log output to files;
		proc glimmix data=environ_epi_day plot=residualpanel method=laplace;
			class experiment treatment block date_rating;
			by experiment;
			model lwe_dpd18 = treatment date_rating / dist=beta link=logit htype=3;
			random intercept / subject=block;
			covtest / wald;
			covtest 'glm' glm;
			title 'rcb env epidem-summ day lwe-dpd18 B id base model - step 4 random block - step 4-1 date_rating only';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;	
		
		* RESULTS: estimated G matrix error all three experiments


* ****************************** *
* C. Analyze - Repeated Measures *
* ****************************** *;

*** Step 1 ***;
	** OBJ: repeated measures with date_rating (from 2-2);

	** Step 1-1 **;
		* OBJ: autoregressive (from 2-2);
		%output_html_files_on(name_step=C_step-1-1, title_step=C step 1-1); * redirect html, log output to files;
		proc glimmix data=environ_epi_day plot=residualpanel method=laplace;
			class experiment treatment block date_rating;
			by experiment;
			model lwe_dpd18 = treatment / dist=beta link=logit htype=3;
			random date_rating / type=ar(1) subject=treatment*block;
			covtest / wald;
			covtest 'glm' glm;
			title 'rcb env epidem-summ day lwe-dpd18 C repeated measures - step 1 date_rating - step 1-1 ar(1)';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;	
		
		* RESULTS: ran normally but treatment effect not significant


* *************************** *
* D. Analyze - Fixed effects  *
* *************************** *;

*** Step 1 ***;
	** OBJ: update settings for final analysis;

	** Step 1-1 **;
		* OBJ: base model (B step 2-2);
		%output_html_files_on(name_step=D_step-1-1, title_step=D step 1-1); * redirect html, log output to files;
		proc glimmix data=environ_epi_day plot=residualpanel;
			class experiment treatment date_rating;
			by experiment;
			model lwe_dpd18 = treatment date_rating / dist=beta link=logit htype=3;
			title 'rcb env epidem-summ day lwe-dpd18 D examine - step 1 update settings - step 1-1 base model';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;	

	** Step 2-1 **;
		* OBJ: repeated measures (C step 1-1);
		%output_html_files_on(name_step=D_step-2-1, title_step=D step 2-1); * redirect html, log output to files;
		proc glimmix data=environ_epi_day plot=residualpanel;
			class experiment treatment block date_rating;
			by experiment;
			model lwe_dpd18 = treatment / dist=beta link=logit htype=3;
			random date_rating / type=ar(1) subject=treatment*block;
			title 'rcb env epidem-summ day lwe-dpd18 D examine - step 1 update settings - step 1-1 repeated measures';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;	
		
	** RESULTS: treatment effect not significant
	** CONCLUSION: analysis complete
