* *********************************** *
* RASPBERRY Cane Botrytis 			  * 
* Environmental Observations	      *
* Rating date - Relative humidity 	  *
* *********************************** *;

*** set variables for folder locations and base filename used for saving output
	** local to lab desktop;
*	%LET base_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\Raspb_botrytis\SAS_raspb;
*	%LET results_path=&base_path.\4_results\environ-rating-rh;
*	%LET results_path_img=&base_path.\4_results\environ-rating-rh\html_images;

	** to SAS ODA/SAS Studio;
	%LET base_path=/home/u63629950/raspb-botrytis;
	%LET results_path=&base_path./4_results/environ-rating-rh;
	%LET results_path_img=&base_path./4_results/environ-rating-rh/html_images;

	** both;
	%LET name_base=environ-rating-rh_; 

*** load macros for controlling output;
	** local;
*	%include "&base_path.\3_analysis\output_files_macro.sas";
	
	** SAS Studio;
	%include "&base_path./3_analysis/output_files_macro.sas";

*options ls=120 nonumber formdlim=' ' pagesize=52 center;


* ----- NOTE ----- *
* not shown - analysis of daily data provides more detail and similar story


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
*			datafile="&base_path.\2_data\rasp-caneb_05i_environ_final-rating.csv"
*			dbms=dlm replace out=environ_rating;
*		delimiter=",";
*		getnames=YES;
*	run;
			
	* SAS ODA/SAS Studio;
	proc import 
			datafile="&base_path./2_data/rasp-caneb_05i_environ_final-rating.csv"
			dbms=dlm replace out=environ_rating;
		delimiter=",";
		getnames=YES;
	run;

** sort dataset
	* by rating for 'by group' processing in proc glimmix;
	* good practice to sort dataset in same order that variables appear in class statement;
proc sort data=environ_rating;
	by experiment treatment block date_rating days_since_last period;
run;

** check variable types;
proc contents data=environ_rating;
run;

proc printto; run; * direct log back to SAS Log window;


* ******************************** *
* B. Analyze - Identify base model *
* ******************************** *;

*** OBJ: identify base model (no repeated measures, no random block)

*** Step 1 ***;
	** OBJ: most simple model;
	
	** Step 1-1 **;
		* OBJ: base model;
		* see Appendix B for code
		* RESULTS: Pearson/DF very high (see model test results spreadsheet)
		* CONCLUSION: try adding date_rating


*** Step 2 ***;
	** OBJ: test date_rating;
	
	** Step 2-1 **;
		* OBJ: date_rating separate;
		* see Appendix B for code
		* RESULTS: some improvement in Pearson/DF, slight improvement in fit

	** Step 2-2 **;
		* OBJ: date_rating interaction;
		* see Appendix B for code
		* RESULTS: minuscule improvement in Pearson/DF, worse fit vs step 2-1
	
	** CONCLUSION: test period
	
	
*** Step 3 ***;
	** OBJ: test period to improve model;

	** Step 3-1 **;
		* OBJ: add period to b interaction;
		* see Appendix B for code
		* RESULTS: drastic improvement in Pearson/DF, improvement in fit vs step 2-2

	** Step 3-2 **;
		* OBJ: add period to a interaction;
		* see Appendix B for code;
		* RESULTS: slightly worse Pearson/df, fit

	** Step 3-3 **;
		* OBJ: add period to a, b interaction;
		* see Appendix B for code;
		* RESULTS: pearson/DF, fit improved vs. step 3-2
		
	** CONCLUSION: test random block


*** Step 4 ***;
	** OBJ: test hour to improve model;
	** note: used in for other data (hourly, daily) but not applicable here


*** Step 5 ***;
	** OBJ: test random block;

	** Step 5-1 **;
		* OBJ: add block to class statement;
		* not run here because did not show difference in other datasets;
		
	** Step 5-2 **;
		* OBJ: add random block; 
			* note: commented out because it the base model that is used for final analysis after being improved upon
*		%output_html_files_on(name_step=B_step-5-2, title_step=B step 5-2); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period period*date_rating / dist=normal link=identity htype=3 ddfm=contain;
*			random intercept / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env hour rh B id base model - step 5 random block - 5-2 add random block';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

		* RESULTS: slight improvement in fit, improved pearson/DF vs step 3-3


* ****************************** *
* C. Analyze - Repeated measures *
* ****************************** *;

*** Step 1 ***;
	** OBJ: test repeated measures, no random block;
	** see Appendix C for code
	** RESULTS: cs, ar(1) are only two models without errors in >1 experiment
	** CONCLUSION: test random block

*** Step 2 ***;
	** OBJ: test repeated measures, no random block;
	** see Appendix C for code
	** RESULTS: estimated G matrix not positive definite both steps, all experiments
	** CONCLUSION: use random block without repeated measures


* ***************************** *
* D. Analyze - Test more models *
* ***************************** *;

*** Step 1 ***;
	** OBJ: test date_rating in three-way interaction;

	** Step 1-1 **;
		* OBJ: no random effects;
		* see Appendix D for code
		
	** Step 1-2 **;
		* OBJ: add random block;
			* note: commented out because model is analyzed with modified settings below;
*		%output_html_files_on(name_step=D_step-1-2, title_step=D step 1-2); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period|date_rating / dist=normal link=identity htype=3 ddfm=contain;
*			random intercept / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env hour rh D test more models - step 1 date_rating interaction - 1-2 add random block';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: slightly worse fit, but random block improves Pearson/DF vs. B step 5-2
	** CONCLUSION: use D step 1-2 to simplify model and examine rating dates, which are of interest
	
	
* ************************** *
* E. Analyze - Fixed effects *
* ************************** *;

*** Step 1 ***;
	** OBJ: analyze fixed effects with method=laplace removed and ddfm=kr2;

	** Step 1-1 **;
		* OBJ: analyze;
			* note: commented out because same analysis is repeated below;
*		%output_html_files_on(name_step=E_step-1-1, title_step=E step 1-1); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period|date_rating / dist=normal link=identity htype=3 ddfm=kr2;
*			random intercept / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env hour rh E analyze fixed effects - step 1 fixed effects - 1-1 no laplace, ddfm=kr2';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: similar overall to D step 1-2
		* A-2-row: treatment*period
		* B-3-row: no sig. effects with treatment
		* C-3-row: weak treatment*date_rating
	** CONCLUSION: examine significant effects
	

* **************************************** *
* F. Analyze - Examine significant effects *
* **************************************** *;

*** Step 1 ***;
	** OBJ: examine interactions;

	** Step 1-1 **;
		* OBJ: treatment x period (for A-2-row);
		%output_html_files_on(name_step=F_step-1-1, title_step=F step 1-1); * redirect html, log output to files;
		proc glimmix data=environ_rating plot=residualpanel;
			class experiment treatment date_rating period;
			by experiment;
			model rh_avg = treatment|period|date_rating / dist=normal link=identity htype=3 ddfm=kr2;
			random intercept / subject=block;
			covtest / wald;
			covtest 'glm' glm;
			
			slice treatment*period / sliceBy=period;
			
			title 'rcb env hour rh F examine significant effects - step 1 interactions - 1-1 trt x period (A-2-row)';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;
		
		* RESULTS: neither slice significant;

	** Step 1-2 **;
		* OBJ: treatment x date_rating (for C-3-row);
			* note: commented out because same results are available in F step 2-2 below;
*		%output_html_files_on(name_step=F_step-1-2, title_step=F step 1-2); * redirect html, log output to files;
*		proc glimmix data=environ_rating_C plot=residualpanel;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period|date_rating / dist=normal link=identity htype=3 ddfm=kr2;
*			random intercept / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			
*			slice treatment*date_rating / sliceBy=date_rating;
*			
*			title 'rcb env hour rh F examine significant effects - step 1 interactions - 1-2 trt x date_rating (C-3-row)';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

		* RESULTS: 10-17, 11-07 not significant, 11-28 significant

*** Step 2 ***;
	** OBJ: examine means;

	** Step 2-2 **;
		* OBJ: treatment x date_rating (for C-3-row);
		%output_html_files_on(name_step=F_step-2-2, title_step=F step 2-2); * redirect html, log output to files;
		proc glimmix data=environ_rating plot=residualpanel;
			class experiment treatment date_rating period;
			by experiment;
			model rh_avg = treatment|period|date_rating / dist=normal link=identity htype=3 ddfm=kr2;
			random intercept / subject=block;
			covtest / wald;
			covtest 'glm' glm;
			
			slice treatment*date_rating / sliceBy=date_rating means ilink linestable adjust=tukey;
			
			title 'rcb env hour rh F examine significant effects - step 2 means - 2-2 trt x date_rating (C-3-row)';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;
		
		* RESULTS: no differences



* --------------------APPENDICES-------------------- *;



* ****************************************** *
* Appendix B - Analyze - Identify base model *
* ****************************************** *;

*** OBJ: identify base model (no repeated measures, no random block)

**** Step 1 ***;
*	** OBJ: most simple model;
*	
*	** Step 1-1 **;
*		* OBJ: base model;
*		%output_html_files_on(name_step=B_step-1-1, title_step=B step 1-1); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace;
*			class experiment treatment;
*			by experiment;
*			model rh_avg = treatment / dist=normal link=identity htype=3 ddfm=contain;
*			title 'rcb env hour rh B id base model - step 1 base (no repeated) - 1-1 base';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: Pearson/DF very high (see model test results spreadsheet)
*		* CONCLUSION: try adding date_rating
*
*
**** Step 2 ***;
*	** OBJ: test date_rating;
*	
*	** Step 2-1 **;
*		* OBJ: date_rating separate;
*		%output_html_files_on(name_step=B_step-2-1, title_step=B step 2-1); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace;
*			class experiment treatment date_rating;
*			by experiment;
*			model rh_avg = treatment date_rating / dist=normal link=identity htype=3 ddfm=contain;
*			title 'rcb env hour rh B id base model - step 2 date_rating (no repeated) - 2-1 separate';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: some improvement in Pearson/DF, slight improvement in fit
*
*	** Step 2-2 **;
*		* OBJ: date_rating interaction;
*		%output_html_files_on(name_step=B_step-2-2, title_step=B step 2-2); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace;
*			class experiment treatment date_rating;
*			by experiment;
*			model rh_avg = treatment|date_rating / dist=normal link=identity htype=3 ddfm=contain;
*			title 'rcb env hour rh B id base model - step 2 date_rating (no repeated) - 2-2 interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: minuscule improvement in Pearson/DF, worse fit vs step 2-1
*	
*	** CONCLUSION: test period
*	
*	
**** Step 3 ***;
*	** OBJ: test period to improve model;
*
*	** Step 3-1 **;
*		* OBJ: add period to b interaction;
*		%output_html_files_on(name_step=B_step-3-1, title_step=B step 3-1); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment period*date_rating / dist=normal link=identity htype=3 ddfm=contain;
*			title 'rcb env hour rh B id base model - step 3 test period - 3-1 add period in b interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: drastic improvement in Pearson/DF, improvement in fit vs step 2-2
*
*	** Step 3-2 **;
*		* OBJ: add period to a interaction;
*		%output_html_files_on(name_step=B_step-3-2, title_step=B step 3-2); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period date_rating / dist=normal link=identity htype=3 ddfm=contain;
*			title 'rcb env hour rh B id base model - step 3 test period - 3-2 add period in a interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: slightly worse Pearson/df, fit
*
*	** Step 3-3 **;
*		* OBJ: add period to a, b interaction;
*		%output_html_files_on(name_step=B_step-3-3, title_step=B step 3-3); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period period*date_rating / dist=normal link=identity htype=3 ddfm=contain;
*			title 'rcb env hour rh B id base model - step 3 test period - 3-3 add period in a, b interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: pearson/DF, fit improved vs. step 3-2
*		
*	** CONCLUSION: try adding hour
*
*
**** Step 4 ***;
*	** OBJ: test hour to improve model;
*	** note: used in for other data (hourly, daily) but not applicable here


* **************************************** *
* Appendix C - Analyze - Repeated measures *
* **************************************** *;

**** Step 1 ***;
*	** OBJ: test repeated measures, no random block;
*
*	** Step 1-1 **;
*		* OBJ: unstructured;
*		%output_html_files_on(name_step=C_step-1-1, title_step=C step 1-1); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace inititer=100;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period  / dist=normal link=identity htype=3 ddfm=contain;
*			random date_rating / type=un subject=treatment*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env hour rh C repeated measure simple - step 1 no block - 1-1 unstructured';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-2 **;
*		* OBJ: compound symmetry;
*		%output_html_files_on(name_step=C_step-1-2, title_step=C step 1-2); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace inititer=100;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period  / dist=normal link=identity htype=3 ddfm=contain;
*			random date_rating / type=cs subject=treatment*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env hour rh C repeated measure simple - step 1 no block - 1-2 compound symmetry';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-3 **;
*		* OBJ: compound symmetry heterogeneous;
*		%output_html_files_on(name_step=C_step-1-3, title_step=C step 1-3); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace inititer=100;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period  / dist=normal link=identity htype=3 ddfm=contain;
*			random date_rating / type=csh subject=treatment*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env hour rh C repeated measure simple - step 1 no block - 1-3 compound symmetry heterogeneous';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-4 **;
*		* OBJ: autoregressive;
*		%output_html_files_on(name_step=C_step-1-4, title_step=C step 1-4); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace inititer=100;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period  / dist=normal link=identity htype=3 ddfm=contain;
*			random date_rating / type=ar(1) subject=treatment*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env hour rh C repeated measure simple - step 1 no block - 1-4 autoregressive';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-5 **;
*		* OBJ: autoregressive heterogeneous;
*		%output_html_files_on(name_step=C_step-1-5, title_step=C step 1-5); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace inititer=100;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period  / dist=normal link=identity htype=3 ddfm=contain;
*			random date_rating / type=arh(1) subject=treatment*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env hour rh C repeated measure simple - step 1 no block - 1-5 autoregressive heterogeneous';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-6 **;
*		* OBJ: Toeplitz;
*		%output_html_files_on(name_step=C_step-1-6, title_step=C step 1-6); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace inititer=100;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period  / dist=normal link=identity htype=3 ddfm=contain;
*			random date_rating / type=toep subject=treatment*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env hour rh C repeated measure simple - step 1 no block - 1-6 Toeplitz';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-7 **;
*		* OBJ: Toeplitz heterogeneous;
*		%output_html_files_on(name_step=C_step-1-7, title_step=C step 1-7); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace inititer=100;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period  / dist=normal link=identity htype=3 ddfm=contain;
*			random date_rating / type=toeph subject=treatment*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env hour rh C repeated measure simple - step 1 no block - 1-7 Toeplitz heterogeneous';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-8 **;
*		* OBJ: antedependence;
*		%output_html_files_on(name_step=C_step-1-8, title_step=C step 1-8); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace inititer=100;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period  / dist=normal link=identity htype=3 ddfm=contain;
*			random date_rating / type=ante(1) subject=treatment*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env hour rh C repeated measure simple - step 1 no block - 1-8 antedependence';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** RESULTS: cs, ar(1) are only two models without errors in >1 experiment
*	** CONCLUSION: test random block
*
**** Step 2 ***;
*	** OBJ: test repeated measures, no random block;
*
*	** Step 2-1 **;
*		* OBJ: compound symmetry;
*		%output_html_files_on(name_step=C_step-2-1, title_step=C step 2-1); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period  / dist=normal link=identity htype=3 ddfm=contain;
*			random intercept / subject=block;
*			random date_rating / type=cs subject=treatment*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env hour rh C repeated measure simple - step 2 random block - 2-1 compound symmetry';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 2-2 **;
*		* OBJ: autoregressive;
*		%output_html_files_on(name_step=C_step-2-2, title_step=C step 2-2); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period  / dist=normal link=identity htype=3 ddfm=contain;
*			random intercept / subject=block;
*			random date_rating / type=ar(1) subject=treatment*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env hour rh C repeated measure simple - step 2 random block - 2-2 autoregressive';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** RESULTS: estimated G matrix not positive definite both steps, all experiments
*	
*	** CONCLUSION: use random block without repeated measures


* *************************************** *
* Appendix D - Analyze - Test more models *
* *************************************** *;

**** Step 1 ***;
*	** OBJ: test date_rating in three-way interaction;
*
*	** Step 1-1 **;
*		* OBJ: no random effects;
*		%output_html_files_on(name_step=D_step-1-1, title_step=D step 1-1); * redirect html, log output to files;
*		proc glimmix data=environ_rating plot=residualpanel method=laplace;
*			class experiment treatment date_rating period;
*			by experiment;
*			model rh_avg = treatment|period|date_rating / dist=normal link=identity htype=3 ddfm=contain;
*			title 'rcb env hour rh D test more models - step 1 date_rating interaction - 1-1 no random effects';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
