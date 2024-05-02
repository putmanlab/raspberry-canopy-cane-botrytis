* ******************************* *
* RASPBERRY Cane Botrytis 		  * 
* Environmental Observations      *
* Daily - Temperature			  *
* ******************************* *;

*** set variables for folder locations and base filename used for saving output
	** local to lab desktop;
*	%LET base_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\Raspb_botrytis\SAS_raspb;
*	%LET results_path=&base_path.\4_results\environ-day-temp;
*	%LET results_path_img=&base_path.\4_results\environ-day-temp\html_images;

	** to SAS ODA/SAS Studio;
	%LET base_path=/home/u63629950/raspb-botrytis;
	%LET results_path=&base_path./4_results/environ-day-temp;
	%LET results_path_img=&base_path./4_results/environ-day-temp/html_images;

	** both;
	%LET name_base=environ-day-temp_; 

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
*			datafile="&base_path.\2_data\rasp-caneb_04f_environ_final-day.csv"
*			dbms=dlm replace out=environ_day;
*		delimiter=",";
*		getnames=YES;
*	run;
			
	* SAS ODA/SAS Studio;
	proc import 
			datafile="&base_path./2_data/rasp-caneb_04f_environ_final-day.csv"
			dbms=dlm replace out=environ_day;
		delimiter=",";
		getnames=YES;
	run;

** sort dataset
	* by rating for 'by group' processing in proc glimmix;
	* good practice to sort dataset in same order that variables appear in class statement;
proc sort data=environ_day;
	by experiment treatment block date_rating week day_of_week date period;
run;

** check variable types;
proc contents data=environ_day;
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
		* RESULTS: pearson/df too high
		* CONCLUSION: test time factors as fixed effects


*** Step 2 ***;
	** OBJ: test time factor(s) as fixed;
	
	** Step 2-1 **;
		* OBJ: date;
		* see Appendix B for code;

	** Step 2-2 **;
		* OBJ: week, day of week components;
		* see Appendix B for code;

	** RESULTS: identical
	** CONCLUSION: use 2-1 to simplify analysis


*** Step 3 ***;
	** OBJ: test period;

	** Step 3-1 **;
		* OBJ: add period to b interaction;
		* see Appendix B for code;

	** Step 3-2 **;
		* OBJ: add period to a interaction;
		* see Appendix B for code;

	** Step 3-3 **;
		* OBJ: add period to a,b interaction;
		* see Appendix B for code;

	** RESULTS: 3-2 fit is worse but pearson/DF is acceptable

	** CONCLUSION: use 3-2


*** Step 4 ***;
	** OBJ: test date_rating as fixed effect (to 3-2);

	** Step 4-1 **;
		* OBJ: add date_rating to a interaction;
		* not run due to missing fixed effect estimates for relative humidity

	** Step 4-2 **;
		* OBJ: add date_rating to a,b interaction;
		* see Appendix B for code;
		* RESULTS: much worse fit, pearson/DF increased but still acceptable
		
	** CONCLUSION: do not use, makes fit worse


*** Step 5 ***;
	** OBJ: condense to one interaction;

	** Step 5-1 **;
		* OBJ: with date;
		* see Appendix B for code;
		* RESULTS: fit, pearson/DF significantly improved vs. 3-2, but fit slightly worse vs 3-3

	** Step 5-2 **;
		* OBJ: with date_rating;
		* not run
	
	** CONCLUSION: test random block with 5-1
		

*** Step 6 ***;
	** OBJ: test random block (from 5-1);

	** Step 6-1 **;
		* OBJ: random block;
			* note: commented out and not moved to appendix because is earlier version of final model;
*		%output_html_files_on(name_step=B_step-6-1, title_step=B step 6-1); * redirect html, log output to files;
*		proc glimmix data=environ_day plot=residualpanel method=laplace;
*			class experiment treatment block date period;
*			by experiment;
*			model temp_avg = treatment|period|date / dist=normal link=identity htype=3;
*			random intercept / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env day temp B id base model - step 6 random block - step 6-1 random block';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;	
		
		* RESULTS: worse fit but improved pearson/DF
	

* ****************************** *
* C. Analyze - Repeated Measures *
* ****************************** *;

*** Step 1 ***;
	** OBJ: base repeated measures, no random block;

	** Step 1-1 **;
		* OBJ: compound symmetry;
		* see Appendix C for code;

	** Step 1-2 **;
		* OBJ: autoregressive;
		* see Appendix C for code;

	** RESULTS: fit much higher, Pearson/DF also increased but within acceptable range
	
	** CONCLUSION: do not use repeated measures
	

* *************************** *
* D. Analyze - Fixed effects  *
* *************************** *;

*** Step 1 ***;
	** OBJ: update B step 6-1 settings for final analysis;

	** Step 1-1 **;
		* OBJ: remove laplace;
			* note: commented out because same results available in E step 2-1;
*		%output_html_files_on(name_step=D_step-1-1, title_step=D step 1-1); * redirect html, log output to files;
*		proc glimmix data=environ_day plot=residualpanel;
*			class experiment treatment block date period;
*			by experiment;
*			model temp_avg = treatment|period|date / dist=normal link=identity htype=3;
*			random intercept / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env day temp D examine - step 1 update settings - step 1-1 remove laplace';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;	
		
	** RESULTS: 
		* A: treatment*period
		* B: treatment*period
		* C: treatment*period, treatment*date
	** CONCLUSION: examine interactions	


* ********************************* *
* E. Analyze - Examine interaction  *
* ********************************* *;

*** Step 1 ***;
	** OBJ: examine interaction;

	** Step 1-1 **;
		* OBJ: examine;
			* note: commented out because same results available in step 2-1;
*		%output_html_files_on(name_step=E_step-1-1, title_step=E step 1-1); * redirect html, log output to files;
*		proc glimmix data=environ_day plot=residualpanel;
*			class experiment treatment block date period;
*			by experiment;
*			model temp_avg = treatment|period|date / dist=normal link=identity htype=3;
*			random intercept / subject=block;
*
*			slice treatment*period / sliceBy=period;
*
*			title 'rcb env day temp E examine - step 1 examine interaction - step 1-1 slice';
*			ods exclude DiffPlot;
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;	
		
		* RESULTS:
			* A: day, night
			* B: day, night
			* C: day, night

*** Step 2 ***;
	** OBJ: examine means;

	** Step 2-1 **;
		* OBJ: examine;
		%output_html_files_on(name_step=E_step-2-1, title_step=E step 2-1); * redirect html, log output to files;
		proc glimmix data=environ_day plot=residualpanel;
			class experiment treatment block date period;
			by experiment;
			model temp_avg = treatment|period|date / dist=normal link=identity htype=3;
			random intercept / subject=block;

			slice treatment*period / sliceBy=period means ilink linestable adjust=tukey;

			title 'rcb env day temp E examine - step 2 examine means - step 2-1 means';
			ods exclude MeanPlot SliceDiffs DiffPlot;
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;	
		
		* RESULTS:
			* A day: control a, manual b, twine b, blade c
			* A night: twine a, blade ab, manual b, control c
			* C night: manual a, blade a, control b, twine b



* --------------------APPENDICES-------------------- *;



* ******************************** *
* B. Analyze - Identify Base Model *
* ******************************** *;

*** Step 1 ***;
	** OBJ: most simple model;
	
	** Step 1-1 **;
		* OBJ: base model;
*		%output_html_files_on(name_step=B_step-1-1, title_step=B step 1-1); * redirect html, log output to files;
*		proc glimmix data=environ_day plot=residualpanel method=laplace;
*			class experiment treatment;
*			by experiment;
*			model temp_avg = treatment / dist=normal link=identity htype=3;
*			title 'rcb env day temp B id base model - step 1 most simple model - step 1-1 base';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: pearson/df too high
*		* CONCLUSION: test time factors as fixed effects
*
*
**** Step 2 ***;
*	** OBJ: test time factor(s) as fixed;
*	
*	** Step 2-1 **;
*		* OBJ: date;
*		%output_html_files_on(name_step=B_step-2-1, title_step=B step 2-1); * redirect html, log output to files;
*		proc glimmix data=environ_day plot=residualpanel method=laplace;
*			class experiment treatment date;
*			by experiment;
*			model temp_avg = treatment date / dist=normal link=identity htype=3;
*			title 'rcb env day temp B id base model - step 2 time fixed - step 2-1 date';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 2-2 **;
*		* OBJ: week, day of week components;
*		%output_html_files_on(name_step=B_step-2-2, title_step=B step 2-2); * redirect html, log output to files;
*		proc glimmix data=environ_day plot=residualpanel method=laplace;
*			class experiment treatment week day_of_week;
*			by experiment;
*			model temp_avg = treatment week*day_of_week / dist=normal link=identity htype=3;
*			title 'rcb env day temp B id base model - step 2 time fixed - step 2-2 week*day_of_week';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** RESULTS: identical
*	** CONCLUSION: use 2-1 to simplify analysis
*
*
**** Step 3 ***;
*	** OBJ: test period;
*
*	** Step 3-1 **;
*		* OBJ: add period to b interaction;
*		%output_html_files_on(name_step=B_step-3-1, title_step=B step 3-1); * redirect html, log output to files;
*		proc glimmix data=environ_day plot=residualpanel method=laplace;
*			class experiment treatment date period;
*			by experiment;
*			model temp_avg = treatment date*period / dist=normal link=identity htype=3;
*			title 'rcb env day temp B id base model - step 3 period - step 3-1 add period to b interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 3-2 **;
*		* OBJ: add period to a interaction;
*		%output_html_files_on(name_step=B_step-3-2, title_step=B step 3-2); * redirect html, log output to files;
*		proc glimmix data=environ_day plot=residualpanel method=laplace;
*			class experiment treatment date period;
*			by experiment;
*			model temp_avg = treatment|period date / dist=normal link=identity htype=3;
*			title 'rcb env day temp B id base model - step 3 period - step 3-2 add period to a interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 3-3 **;
*		* OBJ: add period to a,b interaction;
*		%output_html_files_on(name_step=B_step-3-3, title_step=B step 3-3); * redirect html, log output to files;
*		proc glimmix data=environ_day plot=residualpanel method=laplace;
*			class experiment treatment date period;
*			by experiment;
*			model temp_avg = treatment|period date*period / dist=normal link=identity htype=3;
*			title 'rcb env day temp B id base model - step 3 period - step 3-3 add period to a,b interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** RESULTS: 3-2 fit is worse but pearson/DF is acceptable
*
*	** CONCLUSION: use 3-2
*
*
**** Step 4 ***;
*	** OBJ: test date_rating as fixed effect (to 3-2);
*
*	** Step 4-1 **;
*		* OBJ: add date_rating to a interaction;
*		* not run due to missing fixed effect estimates for relative humidity
*
*	** Step 4-2 **;
*		* OBJ: add date_rating to a,b interaction;
*		%output_html_files_on(name_step=B_step-4-2, title_step=B step 4-2); * redirect html, log output to files;
*		proc glimmix data=environ_day plot=residualpanel method=laplace;
*			class experiment treatment date_rating date period;
*			by experiment;
*			model temp_avg = treatment|date_rating|period date_rating*date / dist=normal link=identity htype=3;
*			title 'rcb env day temp B id base model - step 4 rating date - step 4-2 add rating date to a,b interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*		
*		* RESULTS: much worse fit, pearson/DF increased but still acceptable
*		
*	** CONCLUSION: do not use, makes fit worse
*
*
**** Step 5 ***;
*	** OBJ: condense to one interaction;
*
*	** Step 5-1 **;
*		* OBJ: with date;
*		%output_html_files_on(name_step=B_step-5-1, title_step=B step 5-1); * redirect html, log output to files;
*		proc glimmix data=environ_day plot=residualpanel method=laplace;
*			class experiment treatment date period;
*			by experiment;
*			model temp_avg = treatment|period|date / dist=normal link=identity htype=3;
*			title 'rcb env day temp B id base model - step 5 one interaction - step 5-1 date ';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
		
		* RESULTS: fit, pearson/DF significantly improved vs. 3-2, but fit slightly worse vs 3-3

	** Step 5-2 **;
		* OBJ: with date_rating;
		* not run
	
	** CONCLUSION: test random block with 5-1
		

* *************************************** *
* Appendix C. Analyze - Repeated Measures *
* *************************************** *;

*** Step 1 ***;
	** OBJ: base repeated measures, no random block;

	** Step 1-1 **;
		* OBJ: compound symmetry;
*		%output_html_files_on(name_step=C_step-1-1, title_step=C step 1-1); * redirect html, log output to files;
*		proc glimmix data=environ_day plot=residualpanel method=laplace;
*			class experiment treatment block date period;
*			by experiment;
*			model temp_avg = treatment|period / dist=normal link=identity htype=3;
*			random date / type=cs subject=treatment*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env day temp C repeated measures - step 1 no random block - step 1-1 compound symmetry';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-2 **;
*		* OBJ: autoregressive;
*		%output_html_files_on(name_step=C_step-1-2, title_step=C step 1-2); * redirect html, log output to files;
*		proc glimmix data=environ_day plot=residualpanel method=laplace;
*			class experiment treatment block date period;
*			by experiment;
*			model temp_avg = treatment|period / dist=normal link=identity htype=3;
*			random date / type=ar(1) subject=treatment*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env day temp C repeated measures - step 1 no random block - step 1-2 autoregressive';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: fit much higher, Pearson/DF also increased but within acceptable range
	
	** CONCLUSION: do not use repeated measures


