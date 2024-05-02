* ************************** *
* RASPBERRY Cane Botrytis 	 * 
* Disease					 *
* Severity - Total each cane *
* ************************** *;

*** set variables for folder locations and base filename used for saving output
	** local to lab desktop;
*	%LET base_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\Raspb_botrytis\SAS_raspb;
*	%LET results_path=&base_path.\4_results\rating-severity-cane;
*	%LET results_path_img=&base_path.\4_results\rating-severity-cane\html_images;

	** to SAS ODA/SAS Studio;
	%LET base_path=/home/u63629950/raspb-botrytis;
	%LET results_path=&base_path./4_results/rating-severity-cane;
	%LET results_path_img=&base_path./4_results/rating-severity-cane/html_images;

	** both;
	%LET name_base=rating-severity-cane_; 

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
*			datafile="&base_path.\2_data\rasp-caneb_02-2_disease_severity_cane-total_final.csv"
*			dbms=dlm replace out=rating_sever_cane;
*		delimiter=",";
*		getnames=YES;
*	run;
			
	* SAS ODA/SAS Studio;
	proc import 
			datafile="&base_path./2_data/rasp-caneb_02-2_disease_severity_cane-total_final.csv"
			dbms=dlm replace out=rating_sever_cane;
		delimiter=",";
		getnames=YES;
	run;

** sort dataset
	* by rating for 'by group' processing in proc glimmix;
	* good practice to sort dataset in same order that variables appear in class statement;
proc sort data=rating_sever_cane;
	by experiment trt block date;
run;

** check variable types;
proc contents data=rating_sever_cane;
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
		
	** RESULTS: pearson/DF absurdly high all three experiments
	** CONCLUSION: add rating date as fixed effect


*** Step 2 ***;
	** OBJ: test time factor;

	** Step 2-1 **;
		* OBJ: test time factor as separate fixed effect;
		* see Appendix B for code;
		* RESULTS: some improvement but pearson/DF still absurdly high
		
	** Step 2-2 **;
		* OBJ: test time factor as separate fixed effect;
		* see Appendix B for code;

	** RESULTS: slight but insufficient improvement in pearson/DF vs B step 2-1
	** CONCLUSION: test other distributions


*** Step 3 ***;
	** OBJ: try other distributions (from B step 2-2);

	** Step 3-1 **;
		* OBJ: lognormal;
			* note: commented out and not moved to appendix because is earlier version of final model;
*		%output_html_files_on(name_step=B_step-3-1, title_step=B step 3-1); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel method=laplace;
*			class experiment trt date;
*			by experiment;
*			model lesion_cane_sum = trt|date / dist=lognormal link=identity htype=3;
*			title 'rcb severity cane total B id base model - step 3 test other distributions - step 3-1 lognormal';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

	** Step 3-2 **;
		* OBJ: exponential;
		* see Appendix B for code;

	** RESULTS: drastic improvement
		* lognormal has much better fit and acceptable pearson/DF compared to exponential
	** CONCLUSION: use lognormal;
			

* ****************************** *
* C. Analyze - Repeated Measures *
* ****************************** *;

*** Step 1 ***;
	** OBJ: base, no random block;

	** Step 1-1 **;
		* OBJ: unstructured;
		* see Appendix C for code;
		* RESULTS: error all experiments;

	** Step 1-2 **;
		* OBJ: compound symmetry;
		* see Appendix C for code;
		* RESULTS: ran normally, but residual cov parm estimate missing, A covtest P-value > 0.05

	** Step 1-3 **;
		* OBJ: compound symmetry heterogeneous;
		* see Appendix C for code;
		* RESULTS: ran normally but all pearson/DF suspiciously 0 and missing Var(1) cov parm estimate;
		
	** Step 1-4 **;
		* OBJ: autoregressive;
		* see Appendix C for code;
		* RESULTS: pearson/DF suspiciously 0 for all experiments, missing residual cov parm estimate;

	** Step 1-5 **;
		* OBJ: autoregressive heterogeneous;
		* see Appendix C for code;
		* RESULTS: pearson/DF suspiciously 0 for all experiments, missing residual cov parm estimate;

	** Step 1-6 **;
		* OBJ: Toeplitz;
		* see Appendix C for code;
		* RESULTS: A/B missing residual cov parm estimates, C opt routine error
		
	** Step 1-7 **;
		* OBJ: Toeplitz heterogeneous;
		* see Appendix C for code;
		* RESULTS: est G matrix error B/C;

	** Step 1-8 **;
		* OBJ: antedependence;
		* see Appendix C for code;
		* RESULTS: A/C errors

	** RESULTS: no model ran normally without any errors
		* cs: looked most normal but residual estimates missing, possibly indicating overfit
	** CONCLUSION: try random block
	
	
*** Step 2 ***;
	** OBJ: add random block;

	** Step 2-1 **;
		* OBJ: add to C step 1-2;
		* see Appendix C for code;
		* RESULTS: A,B missing est G matrix error, C missing cov parm estimates
		
	** CONCLUSION: made worse, do not use repeated measures
		* try random block without repeated measures


* ************************************ *
* D. Analyze - Identify Base Model - 2 *
* ************************************ *;

*** Step 1 ***;
	** OBJ: test random block (from B step 3-1);

	** Step 1-1 **;
		* OBJ: add block to test time factor as separate fixed effect;
		* see Appendix D for code;
		
	** Step 1-2 **;
		* OBJ: test time factor as separate fixed effect;
		* see Appendix D for code;

	** RESULTS: est G matrix error A,B
	** CONCLUSION: do not use random block
		* use B step 3-1


* ************************* *
* E. Analyze - Main Effects *
* ************************* *;

*** Step 1 ***;
	** OBJ: analyze fixed effects with method=laplace removed and ddfm=kr2 (from B step 3-1);

	** Step 1-1 **;
		* OBJ: analyze;
			* note: commented out and not moved to appendix because is final model, results are available in final section;
*		%output_html_files_on(name_step=E_step-1-1, title_step=E step 1-1); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel;
*			class experiment trt date;
*			by experiment;
*			model lesion_cane_sum = trt|date / dist=lognormal link=identity htype=3 ddfm=kr2;
*			title 'rcb severity cane total E analyze fixed effects - step 1 fixed effects - step 1-1 -laplace, +ddfm=kr2';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: trt all experiments
	** CONCLUSION: examine means


* **************************************** *
* F. Analyze - Examine significant effects *
* **************************************** *;

*** Step 1 ***;
	** OBJ: examine means;

	** Step 1-1 **;
		* OBJ: analyze;
		%output_html_files_on(name_step=F_step-1-1, title_step=F step 1-1); * redirect html, log output to files;
		proc glimmix data=rating_sever_cane plot=residualpanel;
			class experiment trt date;
			by experiment;
			model lesion_cane_sum = trt|date / dist=lognormal link=identity htype=3 ddfm=kr2;
			
			lsmeans trt / lines ilink adjust=tukey adjdfe=row;

			title 'rcb severity cane total F analyze signif effects - step 1 means - step 1-1 examine';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS:
		* A: blade a, control ab, manual b, twine ab
		* B: blade b, control a, manual b, twine b
		* C: blade ab, control b, manual a, twine a
	** CONCLUSION: analysis complete



* --------------------APPENDICES-------------------- *;



* ***************************************** *
* Appendix B. Analyze - Identify Base Model *
* ***************************************** *;

*** Step 1 ***;
	** OBJ: most simple model;

*	** Step 1-1 **;
*		* OBJ: base model;
*		%output_html_files_on(name_step=B_step-1-1, title_step=B step 1-1); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel method=laplace;
*			class experiment trt;
*			by experiment;
*			model lesion_cane_sum = trt / dist=normal link=identity htype=3;
*			title 'rcb severity cane total B id base model - step 1 most simple model - step 1-1 base';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** RESULTS: pearson/DF absurdly high all three experiments
*	** CONCLUSION: add rating date as fixed effect
*
*
**** Step 2 ***;
*	** OBJ: test time factor;
*
*	** Step 2-1 **;
*		* OBJ: test time factor as separate fixed effect;
*		%output_html_files_on(name_step=B_step-2-1, title_step=B step 2-1); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel method=laplace;
*			class experiment trt date;
*			by experiment;
*			model lesion_cane_sum = trt date / dist=normal link=identity htype=3;
*			title 'rcb severity cane total B id base model - step 2 test time factor - step 2-1 rating date seperate';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: some improvement but pearson/DF still absurdly high
*		
*	** Step 2-2 **;
*		* OBJ: test time factor as separate fixed effect;
*		%output_html_files_on(name_step=B_step-2-2, title_step=B step 2-2); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel method=laplace;
*			class experiment trt date;
*			by experiment;
*			model lesion_cane_sum = trt|date / dist=normal link=identity htype=3;
*			title 'rcb severity cane total B id base model - step 2 test time factor - step 2-2 rating date interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** RESULTS: slight but insufficient improvement in pearson/DF vs B step 2-1
*	** CONCLUSION: test repeated measures
*
*
**** Step 3 ***;
*	** OBJ: try other distributions (from B step 2-2);
*
*	** Step 3-2 **;
*		* OBJ: exponential;
*		%output_html_files_on(name_step=B_step-3-2, title_step=B step 3-2); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel method=laplace;
*			class experiment trt date;
*			by experiment;
*			model lesion_cane_sum = trt|date / dist=exponential link=log htype=3;
*			title 'rcb severity cane total B id base model - step 3 test other distributions - step 3-2 exponential';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** RESULTS: drastic improvement
*		* lognormal has much better fit and acceptable pearson/DF compared to exponential
*	** CONCLUSION: use lognormal;


* *************************************** *
* Appendix C. Analyze - Repeated Measures *
* *************************************** *;

*** Step 1 ***;
	** OBJ: base, no random block;

	** Step 1-1 **;
		* OBJ: unstructured;
*		%output_html_files_on(name_step=C_step-1-1, title_step=C step 1-1); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel method=laplace;
*			class experiment trt date block cane;
*			by experiment;
*			model lesion_cane_sum = trt / dist=lognormal link=identity htype=3;
*			random date / type=un subject=trt*block*cane;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb severity cane total C repeated measures - step 1 no random block - step 1-1 unstructured';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*		
*		* RESULTS: error all experiments;
*
*	** Step 1-2 **;
*		* OBJ: compound symmetry;
*		%output_html_files_on(name_step=C_step-1-2, title_step=C step 1-2); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel method=laplace;
*			class experiment trt date block cane;
*			by experiment;
*			model lesion_cane_sum = trt / dist=lognormal link=identity htype=3;
*			random date / type=cs subject=trt*block*cane;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb severity cane total C repeated measures - step 1 no random block - step 1-2 compound symmetry';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*		
*		* RESULTS: ran normally, but residual cov parm estimate missing, A covtest P-value > 0.05
*
*	** Step 1-3 **;
*		* OBJ: compound symmetry heterogeneous;
*		%output_html_files_on(name_step=C_step-1-3, title_step=C step 1-3); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel method=laplace;
*			class experiment trt date block cane;
*			by experiment;
*			model lesion_cane_sum = trt / dist=lognormal link=identity htype=3;
*			random date / type=csh subject=trt*block*cane;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb severity cane total C repeated measures - step 1 no random block - step 1-3 compound symmetry heterogeneous';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: ran normally but all pearson/DF suspiciously 0 and missing Var(1) cov parm estimate;
*		
*	** Step 1-4 **;
*		* OBJ: autoregressive;
*		%output_html_files_on(name_step=C_step-1-4, title_step=C step 1-4); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel method=laplace;
*			class experiment trt date block cane;
*			by experiment;
*			model lesion_cane_sum = trt / dist=lognormal link=identity htype=3;
*			random date / type=ar(1) subject=trt*block*cane;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb severity cane total C repeated measures - step 1 no random block - step 1-4 autoregressive';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*		
*		* RESULTS: pearson/DF suspiciously 0 for all experiments, missing residual cov parm estimate;
*
*	** Step 1-5 **;
*		* OBJ: autoregressive heterogeneous;
*		%output_html_files_on(name_step=C_step-1-5, title_step=C step 1-5); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel method=laplace;
*			class experiment trt date block cane;
*			by experiment;
*			model lesion_cane_sum = trt / dist=lognormal link=identity htype=3;
*			random date / type=arh(1) subject=trt*block*cane;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb severity cane total C repeated measures - step 1 no random block - step 1-5 autoregressive heterogeneous';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*		
*		* RESULTS: pearson/DF suspiciously 0 for all experiments, missing residual cov parm estimate;
*
*	** Step 1-6 **;
*		* OBJ: Toeplitz;
*		%output_html_files_on(name_step=C_step-1-6, title_step=C step 1-6); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel method=laplace;
*			class experiment trt date block cane;
*			by experiment;
*			model lesion_cane_sum = trt / dist=lognormal link=identity htype=3;
*			random date / type=toep subject=trt*block*cane;
*			covtest / wald;
*			covtest 'glm' glm;
*		title 'rcb severity cane total C repeated measures - step 1 no random block - step 1-6 Toeplitz';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*		
*		* RESULTS: A/B missing residual cov parm estimates, C opt routine error
*		
*	** Step 1-7 **;
*		* OBJ: Toeplitz heterogeneous;
*		%output_html_files_on(name_step=C_step-1-7, title_step=C step 1-7); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel method=laplace;
*			class experiment trt date block cane;
*			by experiment;
*			model lesion_cane_sum = trt / dist=lognormal link=identity htype=3;
*			random date / type=toeph subject=trt*block*cane;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb severity cane total C repeated measures - step 1 no random block - step 1-7 Toeplitz heterogeneous';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*		
*		* RESULTS: est G matrix error B/C;
*
*	** Step 1-8 **;
*		* OBJ: antedependence;
*		%output_html_files_on(name_step=C_step-1-8, title_step=C step 1-8); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel method=laplace;
*			class experiment trt date block cane;
*			by experiment;
*			model lesion_cane_sum = trt / dist=lognormal link=identity htype=3;
*			random date / type=ante(1) subject=trt*block*cane;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb severity cane total C repeated measures - step 1 no random block - step 1-8 antedependence';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*		
*		* RESULTS: A/C errors
*
*	** RESULTS: no model ran normally without any errors
*		* cs: looked most normal but residual estimates missing, possibly indicating overfit
*	** CONCLUSION: try random block
*	
*	
**** Step 2 ***;
*	** OBJ: add random block;
*
*	** Step 2-1 **;
*		* OBJ: add to C step 1-2;
*		%output_html_files_on(name_step=C_step-2-1, title_step=C step 2-1); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel method=laplace inititer=250;
*			class experiment trt date block cane;
*			by experiment;
*			model lesion_cane_sum = trt / dist=lognormal link=identity htype=3;
*			random date / type=cs subject=trt*block*cane;
*			random intercept / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb severity cane total C repeated measures - step 2 with random block - step 2-1 from C step 1-2';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*		* RESULTS: A,B missing est G matrix error, C missing cov parm estimates
*		
*	** CONCLUSION: made worse, do not use repeated measures
*		* try random block without repeated measures


* ********************************************* *
* Appendix D. Analyze - Identify Base Model - 2 *
* ********************************************* *;

*** Step 1 ***;
	** OBJ: test random block (from B step 3-1);

	** Step 1-1 **;
		* OBJ: add block to test time factor as separate fixed effect;
*		%output_html_files_on(name_step=D_step-1-1, title_step=D step 1-1); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel method=laplace;
*			class experiment trt block date;
*			by experiment;
*			model lesion_cane_sum = trt date / dist=lognormal link=identity htype=3;
*			random intercept / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb severity cane total D id base model 2 - step 1 random block - step 1-1 rating date seperate';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*		
*	** Step 1-2 **;
*		* OBJ: test time factor as separate fixed effect;
*		%output_html_files_on(name_step=D_step-1-2, title_step=D step 1-2); * redirect html, log output to files;
*		proc glimmix data=rating_sever_cane plot=residualpanel method=laplace;
*			class experiment trt block date;
*			by experiment;
*			model lesion_cane_sum = trt|date / dist=lognormal link=identity htype=3;
*			random intercept / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb severity cane total D id base model 2 - step 1 random block - step 1-2 rating date interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** RESULTS: est G matrix error A,B
*	** CONCLUSION: do not use random block
*		* use B step 3-1
