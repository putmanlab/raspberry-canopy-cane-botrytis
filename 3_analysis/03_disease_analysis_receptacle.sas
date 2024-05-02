* *********************** *
* RASPBERRY Cane Botrytis * 
* Receptacle			  *
* *********************** *;

*** set variables for folder locations and base filename used for saving output
	** local to lab desktop;
*	%LET base_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\Raspb_botrytis\SAS_raspb;
*	%LET results_path=&base_path.\4_results\rating-recept;
*	%LET results_path_img=&base_path.\4_results\rating-recept\html_images;

	** to SAS ODA/SAS Studio;
	%LET base_path=/home/u63629950/raspb-botrytis;
	%LET results_path=&base_path./4_results/rating-recept;
	%LET results_path_img=&base_path./4_results/rating-recept/html_images;

	** both;
	%LET name_base=rating-recept_; 

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
*			datafile="&base_path.\2_data\rasp-caneb_02-3_receptacles_final.csv"
*			dbms=dlm replace out=rating_recept;
*		delimiter=",";
*		getnames=YES;
*	run;
			
	* SAS ODA/SAS Studio;
	proc import 
			datafile="&base_path./2_data/rasp-caneb_02-3_receptacles_final.csv"
			dbms=dlm replace out=rating_recept;
		delimiter=",";
		getnames=YES;
	run;

** sort dataset
	* by rating for 'by group' processing in proc glimmix;
	* good practice to sort dataset in same order that variables appear in class statement;
proc sort data=rating_recept;
	by experiment trt block;
run;

** check variable types;
proc contents data=rating_recept;
run;

proc printto; run; * direct log back to SAS Log window;


* ******************************** *
* B. Analyze - Identify Base Model *
* ******************************** *;

*** Step 1 ***;
	** OBJ: most simple model;

	** Step 1-1 **;
		* OBJ: base model;
		* see Appendix B for code;

	** RESULTS: pearson/DF outrageously high
	** CONCLUSION: try other distributions


*** Step 2 ***;
	** OBJ: test other distributions;

	** Step 2-1 **;
		* OBJ: lognormal;
			* note: commented out and not moved to appendix because is early version of final model;
*		%output_html_files_on(name_step=B_step-2-1, title_step=B step 2-1); * redirect html, log output to files;
*		proc glimmix data=rating_recept plot=residualpanel method=laplace;
*			class experiment trt;
*			by experiment;
*			model n_receps = trt / dist=lognormal link=identity htype=3;
*			title 'rcb receptacles B id base model - step 2 other distributions - step 2-1 lognormal';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
			
		* RESULTS: dramatically better fit, huge improvement in pearson/DF to acceptable levels

	** Step 2-2 **;
		* OBJ: negative binomial;
		* see Appendix B for code;
		* RESULTS: huge improvement in pearson/DF to acceptable levels but not as much as step 2-1, 
			* A,B much worse fit than step 1-1 (C similar)

	** Step 2-3 **;
		* OBJ: poisson;
		* see Appendix B for code;
		* RESULTS: much worse fit, huge improvement in pearson/DF but not to acceptable levels

	** CONCLUSION: use lognormal
		* test random block


*** Step 3 ***;
	** OBJ: test random block;

	** Step 3-1 **;
		* OBJ: intercept only;
		%output_html_files_on(name_step=B_step-3-1, title_step=B step 3-1); * redirect html, log output to files;
		proc glimmix data=rating_recept plot=residualpanel method=laplace;
			class experiment trt block;
			by experiment;
			model n_receps = trt / dist=lognormal link=identity htype=3;
			random intercept / subject=block;
			covtest / wald;
			covtest 'glm' glm;			
			title 'rcb receptacles B id base model - step 3 random block - step 3-1 intercept only';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;
			
		* RESULTS: slight improvement vs B step 2-1
			* no significant differences

	** Step 3-2 **;
		* OBJ: intercept, trt;
		* see Appendix B for code;
		* RESULTS: A,B est G matrix error, C no improvement vs C step 3-1

	** CONCLUSION: use 3-1
		* test canes as covariate


*** Step 4 ***;
	** OBJ: test numb of canes as covariate;

	** Step 4-1 **;
		* OBJ: canes covariate;
		* see Appendix B for code;
			
	** RESULTS: no change vs step 3-1
	** CONCLUSION: analysis complete with step 3-1
	


* --------------------APPENDICES-------------------- *;



* ***************************************** *
* Appendix B. Analyze - Identify Base Model *
* ***************************************** *;

**** Step 1 ***;
*	** OBJ: most simple model;
*
*	** Step 1-1 **;
*		* OBJ: base model;
*		%output_html_files_on(name_step=B_step-1-1, title_step=B step 1-1); * redirect html, log output to files;
*		proc glimmix data=rating_recept plot=residualpanel method=laplace;
*			class experiment trt;
*			by experiment;
*			model n_receps = trt / dist=normal link=identity htype=3;
*			title 'rcb receptacles B id base model - step 1 most simple model - step 1-1 base';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** RESULTS: pearson/DF outrageously high
*	** CONCLUSION: try other distributions
*
*
**** Step 2 ***;
*	** OBJ: test other distributions;
*
*	** Step 2-2 **;
*		* OBJ: negative binomial;
*		%output_html_files_on(name_step=B_step-2-2, title_step=B step 2-2); * redirect html, log output to files;
*		proc glimmix data=rating_recept plot=residualpanel method=laplace;
*			class experiment trt;
*			by experiment;
*			model n_receps = trt / dist=negbinomial link=log htype=3;
*			title 'rcb receptacles B id base model - step 2 other distributions - step 2-2 negative binomial';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*		
*		* RESULTS: huge improvement in pearson/DF to acceptable levels but not as much as step 2-1, 
*			* A,B much worse fit than step 1-1 (C similar)
*
*	** Step 2-3 **;
*		* OBJ: poisson;
*		%output_html_files_on(name_step=B_step-2-3, title_step=B step 2-3); * redirect html, log output to files;
*		proc glimmix data=rating_recept plot=residualpanel method=laplace;
*			class experiment trt;
*			by experiment;
*			model n_receps = trt / dist=poisson link=log htype=3;
*			title 'rcb receptacles B id base model - step 2 other distributions - step 2-3 poisson';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: much worse fit, huge improvement in pearson/DF but not to acceptable levels
*
*	** CONCLUSION: use lognormal
*		* test random block
*
*
**** Step 3 ***;
*	** OBJ: test random block;
*
*	** Step 3-2 **;
*		* OBJ: intercept, trt;
*		%output_html_files_on(name_step=B_step-3-2, title_step=B step 3-2); * redirect html, log output to files;
*		proc glimmix data=rating_recept plot=residualpanel method=laplace;
*			class experiment trt block;
*			by experiment;
*			model n_receps = trt / dist=lognormal link=identity htype=3;
*			random intercept trt / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;			
*			title 'rcb receptacles B id base model - step 3 random block - step 3-2 intercept, trt';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*		
*		* RESULTS: A,B est G matrix error, C no improvement vs C step 3-1
*
*	** CONCLUSION: use 3-1
*		* test canes as covariate
*
*
**** Step 4 ***;
*	** OBJ: test numb of canes as covariate;
*
*	** Step 4-1 **;
*		* OBJ: canes covariate;
*		%output_html_files_on(name_step=B_step-4-1, title_step=B step 4-1); * redirect html, log output to files;
*		proc glimmix data=rating_recept plot=residualpanel method=laplace;
*			class experiment trt block;
*			by experiment;
*			model n_receps = trt n_canes / dist=lognormal link=identity htype=3;
*			random intercept / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;			
*			title 'rcb receptacles B id base model - step 4 covariate - step 4-1 test';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*			
*	** RESULTS: no change vs step 3-1
*	** CONCLUSION: analysis complete with step 3-1
