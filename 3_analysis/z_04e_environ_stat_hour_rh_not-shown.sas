* ******************************* *
* RASPBERRY Cane Botrytis 		  * 
* Environmental Observations      *
* Hourly - Relative humidity 	  *
* ******************************* *;

*** set variables for folder locations and base filename used for saving output
	** local to lab desktop;
*	%LET base_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\Raspb_botrytis\SAS_raspb;
*	%LET results_path=&base_path.\4_results\environ-hour-rh;
*	%LET results_path_img=&base_path.\4_results\environ-hour-rh\html_images;

	** to SAS ODA/SAS Studio;
	%LET base_path=/home/u63629950/raspb-botrytis;
	%LET results_path=&base_path./4_results/environ-hour-rh;
	%LET results_path_img=&base_path./4_results/environ-hour-rh/html_images;

	** both;
	%LET name_base=environ-hour-rh_; 

*** load macros for controlling output;
	** local;
*	%include "&base_path.\3_analysis\output_files_macro.sas";
	
	** SAS Studio;
	%include "&base_path./3_analysis/output_files_macro.sas";

*options ls=120 nonumber formdlim=' ' pagesize=52 center;


* ----- NOTE ----- *
* analysis of hourly data not completed
* magnitude of differences in raw averages of daily data is small, there is no need to find differences in hourly data


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
*			datafile="&base_path.\2_data\rasp-caneb_05e_environ_final-hour.csv"
*			dbms=dlm replace out=environ_hr;
*		delimiter=",";
*		getnames=YES;
*	run;
			
	* SAS ODA/SAS Studio;
	proc import 
			datafile="&base_path./2_data/rasp-caneb_05e_environ_final-hour.csv"
			dbms=dlm replace out=environ_hr;
		delimiter=",";
		getnames=YES;
	run;

** sort dataset
	* by rating for 'by group' processing in proc glimmix;
	* good practice to sort dataset in same order that variables appear in class statement;
proc sort data=environ_hr;
	by experiment treatment block datetime week day_of_week hour period date_adj date_rating;
run;

** check variable types;
proc contents data=environ_hr;
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
		* RESULTS: very high Pearson/DF (see model testing results spreadsheet)
		* CONCLUSION: add time as fixed effect


*** Step 2 ***;
	** OBJ: test datetime vs components;
	
	** Step 2-1 **;
		* OBJ: datetime;
		* see Appendix B for code
		* RESULTS: significant improvement vs step 1-1 (see spreadsheet)

	** Step 2-2 **;
		* OBJ: week*day_of_week*hour;
		* see Appendix B for code
		* RESULTS: identical to step 2-1
	
	** CONCLUSION: Pearson/DF still a bit too high
	
	
*** Step 3 ***;
	** OBJ: test period to improve model;

	** Step 3-1 **;
		* OBJ: add period to b interaction;
		* see Appendix B for code
		* RESULTS: identical to step 2-2

	** Step 3-2 **;
		* OBJ: add period to a interaction;
		* see Appendix B for code
		* RESULTS: very slight improvement vs Step 3-1, but period results missing

	** Step 3-3 **;
		* OBJ: add period to a, b interaction;
		* see Appendix B for code
		* RESULTS: same as 3-2 except period result present
		
	** CONCLUSION: use 3-3, try adding hour


*** Step 4 ***;
	** OBJ: test hour to improve model;

	** Step 4-1 **;
		* OBJ: add week to a interaction;
		* see Appendix B for code
		* RESULTS: very slight improvement vs 3-3
	
	** CONCLUSION: 
		* improvement too slight to retain, would be complicated to analyze
		* test random block
		

*** Step 5 ***;
	** OBJ: test random block;

	** Step 5-1 **;
		* OBJ: add block to class statement;
		* see Appendix B for code
		* RESULTS: no change vs 4-1

	** Step 5-2 **;
		* OBJ: add random block;
		* see Appendix B for code
		* RESULTS: stopped, still running after 2 hr

	** CONCLUSION: no random block


*** Step 6 ***;
	** OBJ: add date_rating to step 4-1;

	** Step 6-1 **;
		* OBJ: add rating date to a interaction;
		* see Appendix B for code
		* RESULTS: date_rating fixed effect estimates missing
		
	** Step 6-2 **;
		* OBJ: add rating date to b interaction;
		* see Appendix B for code
		* RESULTS: identical to step 5-1

	** Step 6-3 **;
		* OBJ: add rating date to b interaction;
		%output_html_files_on(name_step=B_step-6-3, title_step=B step 6-3); * redirect html, log output to files;
		proc glimmix data=environ_hr plot=residualpanel method=laplace;
			class experiment treatment date_rating week day_of_week period hour;
			by experiment;
			model rh_avg = treatment|date_rating|period date_rating*week*day_of_week*period*hour / dist=normal link=identity htype=3 ddfm=bw;
			title 'rcb env hour rh B id base model - step 6 rating date - 6-3 add to a,b interaction';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;
	
		* RESULTS: identical to step 6-1

	** CONCLUSION: use 6-3


* ****************************** *
* C. Analyze - Repeated measures *
* ****************************** *;

*** Step 1 ***;
	** OBJ: test repeated measures, no random block;
	** not run due to model size, dispersion/fit problems with daily data

*	** Step 1-1 **;
*		* OBJ: compound symmetry;
*		%output_html_files_on(name_step=B_step-1-1, title_step=B step 1-1); * redirect html, log output to files;
*		proc glimmix data=environ_hr plot=residualpanel method=laplace;
*			class experiment treatment week day_of_week period hour;
*			by experiment;
*			model rh_avg = treatment|period / dist=normal link=identity htype=3 ddfm=bw;
*			random week*day_of_week*period*hour / type=cs subject=treatment*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env hour rh C repeated measure simple - step 1 repeated measures - 1-1 compound symmetry';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: 
*
*	** Step 1-2 **;
*		* OBJ: autoregressive;
*		%output_html_files_on(name_step=B_step-1-2, title_step=B step 1-2); * redirect html, log output to files;
*		proc glimmix data=environ_hr plot=residualpanel method=laplace;
*			class experiment treatment week day_of_week period hour;
*			by experiment;
*			model rh_avg = treatment|period / dist=normal link=identity htype=3 ddfm=bw;
*			random week*day_of_week*period*hour / type=ar(1) subject=treatment*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env hour rh B repeated measure simple - step 6 repeated measures - 1-2 autoregressive';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: 


* ************************* *
* D. Analyze - Other models *
* ************************* *;

** NOTE: NOT RUN - possible model if do future testing

*** Step 1 ***;
	** OBJ: simplified model with date;

*	** Step 1-1 **;
*		* OBJ: date;
*		%output_html_files_on(name_step=B_step-1-1, title_step=B step 1-1); * redirect html, log output to files;
*		proc glimmix data=environ_hr plot=residualpanel method=laplace;
*			class experiment treatment date_rating week day_of_week period hour;
*			by experiment;
*			model rh_avg = treatment|period|date date*period*hour / dist=normal link=identity htype=3 ddfm=bw;
*			title 'rcb env hour rh D other models - step 1 simplified - 1-1 date';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

	

* --------------------APPENDICES-------------------- *;



* ***************************************** *
* Appendix B. Analyze - Identify base model *
* ***************************************** *;

*** OBJ: identify base model (no repeated measures, no random block)

*** Step 1 ***;
	** OBJ: most simple model;
	
*	** Step 1-1 **;
*		* OBJ: base model;
*		%output_html_files_on(name_step=B_step-1-1, title_step=B step 1-1); * redirect html, log output to files;
*		proc glimmix data=environ_hr plot=residualpanel method=laplace;
*			class experiment treatment;
*			by experiment;
*			model rh_avg = treatment / dist=normal link=identity htype=3 ddfm=bw;
*			title 'rcb env hour rh B id base model - step 1 base (no repeated) - 1-1 base';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: very high Pearson/DF (see model testing results spreadsheet)
*		* CONCLUSION: add time as fixed effect


*** Step 2 ***;
	** OBJ: test datetime vs components;
	
*	** Step 2-1 **;
*		* OBJ: datetime;
*		%output_html_files_on(name_step=B_step-2-1, title_step=B step 2-1); * redirect html, log output to files;
*		proc glimmix data=environ_hr plot=residualpanel method=laplace;
*			class experiment treatment datetime;
*			by experiment;
*			model rh_avg = treatment datetime / dist=normal link=identity htype=3 ddfm=bw;
*			title 'rcb env hour rh B id base model - step 2 datetime (no repeated) - 2-1 datetime';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: significant improvement vs step 1-1 (see spreadsheet)
*
*	** Step 2-2 **;
*		* OBJ: week*day_of_week*hour;
*		%output_html_files_on(name_step=B_step-2-2, title_step=B step 2-2); * redirect html, log output to files;
*		proc glimmix data=environ_hr plot=residualpanel method=laplace;
*			class experiment treatment week day_of_week hour;
*			by experiment;
*			model rh_avg = treatment week*day_of_week*hour / dist=normal link=identity htype=3 ddfm=bw;
*			title 'rcb env hour rh B id base model - step 2 datetime (no repeated) - 2-2 week, day of week, hour';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

		* RESULTS: identical to step 2-1
	
	** CONCLUSION: Pearson/DF still a bit too high
	
	
*** Step 3 ***;
	** OBJ: test period to improve model;

*	** Step 3-1 **;
*		* OBJ: add period to b interaction;
*		%output_html_files_on(name_step=B_step-3-1, title_step=B step 3-1); * redirect html, log output to files;
*		proc glimmix data=environ_hr plot=residualpanel method=laplace;
*			class experiment treatment week day_of_week period hour;
*			by experiment;
*			model rh_avg = treatment week*day_of_week*period*hour / dist=normal link=identity htype=3 ddfm=bw;
*			title 'rcb env hour rh B id base model - step 3 test period - 3-1 add period in b interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: identical to step 2-2
*
*	** Step 3-2 **;
*		* OBJ: add period to a interaction;
*		%output_html_files_on(name_step=B_step-3-2, title_step=B step 3-2); * redirect html, log output to files;
*		proc glimmix data=environ_hr plot=residualpanel method=laplace;
*			class experiment treatment week day_of_week period hour;
*			by experiment;
*			model rh_avg = treatment|period week*day_of_week*hour / dist=normal link=identity htype=3 ddfm=bw;
*			title 'rcb env hour rh B id base model - step 3 test period - 3-2 add period in a interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: very slight improvement vs Step 3-1, but period results missing
*
*	** Step 3-3 **;
*		* OBJ: add period to a, b interaction;
*		%output_html_files_on(name_step=B_step-3-3, title_step=B step 3-3); * redirect html, log output to files;
*		proc glimmix data=environ_hr plot=residualpanel method=laplace;
*			class experiment treatment week day_of_week period hour;
*			by experiment;
*			model rh_avg = treatment|period week*day_of_week*period*hour / dist=normal link=identity htype=3 ddfm=bw;
*			title 'rcb env hour rh B id base model - step 3 test period - 3-3 add period in a, b interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

		* RESULTS: same as 3-2 except period result present
		
	** CONCLUSION: use 3-3, try adding hour


*** Step 4 ***;
	** OBJ: test hour to improve model;

*	** Step 4-1 **;
*		* OBJ: add week to a interaction;
*		%output_html_files_on(name_step=B_step-4-1, title_step=B step 4-1); * redirect html, log output to files;
*		proc glimmix data=environ_hr plot=residualpanel method=laplace;
*			class experiment treatment week day_of_week period hour;
*			by experiment;
*			model rh_avg = treatment|period|hour week*day_of_week*period*hour / dist=normal link=identity htype=3 ddfm=bw;
*			title 'rcb env hour rh B id base model - step 4 test hour - 4-1 add hour to a interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

		* RESULTS: very slight improvement vs 3-3
	
	** CONCLUSION: 
		* improvement too slight to retain, would be complicated to analyze
		* test random block
		

*** Step 5 ***;
	** OBJ: test random block;

*	** Step 5-1 **;
*		* OBJ: add block to class statement;
*		%output_html_files_on(name_step=B_step-5-1, title_step=B step 5-1); * redirect html, log output to files;
*		proc glimmix data=environ_hr plot=residualpanel method=laplace;
*			class experiment treatment block week day_of_week period hour;
*			by experiment;
*			model rh_avg = treatment|period week*day_of_week*period*hour / dist=normal link=identity htype=3 ddfm=bw;
*			title 'rcb env hour rh B id base model - step 5 random block - 5-1 add block to class statement';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: no change vs 4-1

	** Step 5-2 **;
		* OBJ: add random block;
*		%output_html_files_on(name_step=B_step-5-2, title_step=B step 5-2); * redirect html, log output to files;
*		proc glimmix data=environ_hr plot=residualpanel method=laplace;
*			class experiment treatment block week day_of_week period hour;
*			by experiment;
*			model rh_avg = treatment|period week*day_of_week*period*hour / dist=normal link=identity htype=3 ddfm=bw;
*			random intercept / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb env hour rh B id base model - step 5 random block - 5-2 add random block';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

		* RESULTS: stopped, still running after 2 hr

	** CONCLUSION: no random block


*** Step 6 ***;
	** OBJ: add date_rating to step 4-1;

*	** Step 6-1 **;
*		* OBJ: add rating date to a interaction;
*		%output_html_files_on(name_step=B_step-6-1, title_step=B step 6-1); * redirect html, log output to files;
*		proc glimmix data=environ_hr plot=residualpanel method=laplace;
*			class experiment treatment date_rating week day_of_week period hour;
*			by experiment;
*			model rh_avg = treatment|date_rating|period week*day_of_week*period*hour / dist=normal link=identity htype=3 ddfm=bw;
*			title 'rcb env hour rh B id base model - step 6 rating date - 6-1 add to a interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: date_rating fixed effect estimates missing
*		
*	** Step 6-2 **;
*		* OBJ: add rating date to b interaction;
*		%output_html_files_on(name_step=B_step-6-2, title_step=B step 6-2); * redirect html, log output to files;
*		proc glimmix data=environ_hr plot=residualpanel method=laplace;
*			class experiment treatment date_rating week day_of_week period hour;
*			by experiment;
*			model rh_avg = treatment|period date_rating*week*day_of_week*period*hour / dist=normal link=identity htype=3 ddfm=bw;
*			title 'rcb env hour rh B id base model - step 6 rating date - 6-2 add to b interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*		
*		* RESULTS: identical to step 5-1

