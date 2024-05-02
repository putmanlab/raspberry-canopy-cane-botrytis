* *********************** *
* RASPBERRY Cane Botrytis * 
* Disease 				  *
* Incidence       		  *
* *********************** *;

*** set variables for folder locations and base filename used for saving output
	** local to lab desktop;
*	%LET base_path=C:\Users\Alex Putman\GoogleDrive\UCR_VSPLab\Raspb_botrytis\SAS_raspb;
*	%LET results_path=&base_path.\4_results\rating-incid;
*	%LET results_path_img=&base_path.\4_results\rating-incid\html_images;

	** to SAS ODA/SAS Studio;
	%LET base_path=/home/u63629950/raspb-botrytis;
	%LET results_path=&base_path./4_results/rating-incid;
	%LET results_path_img=&base_path./4_results/rating-incid/html_images;

	** both;
	%LET name_base=rating-incid_; 

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
*			datafile="&base_path.\2_data\rasp-caneb_02-1_disease_incidence_final.csv"
*			dbms=dlm replace out=rating_incid;
*		delimiter=",";
*		getnames=YES;
*	run;
			
	* SAS ODA/SAS Studio;
	proc import 
			datafile="&base_path./2_data/rasp-caneb_02-1_disease_incidence_final.csv"
			dbms=dlm replace out=rating_incid;
		delimiter=",";
		getnames=YES;
	run;

** sort dataset
	* by rating for 'by group' processing in proc glimmix;
	* good practice to sort dataset in same order that variables appear in class statement;
proc sort data=rating_incid;
	by experiment trt block date;
run;

** check variable types;
proc contents data=rating_incid;
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
		
	** RESULTS: pearson/DF very high all three experiments
	** CONCLUSION: add rating date as fixed effect


*** Step 2 ***;
	** OBJ: test time factor;

	** Step 2-1 **;
		* OBJ: test time factor as separate fixed effect;
		* see Appendix B for code

	** Step 2-2 **;
		* OBJ: test time factor as separate fixed effect;
		* see Appendix B for code

	** RESULTS: significant improvement in fit, drastic (A,B) or good (C) improvement in pearson/DF
	** CONCLUSION: test repeated measures
	

* ****************************** *
* C. Analyze - Repeated Measures *
* ****************************** *;

*** Step 1 ***;
	** OBJ: base, no random block;

	** Step 1-1 **;
		* OBJ: unstructured;
		* see Appendix C for code

	** Step 1-2 **;
		* OBJ: compound symmetry;
		* see Appendix C for code

	** Step 1-3 **;
		* OBJ: compound symmetry heterogeneous;
		* see Appendix C for code

	** Step 1-4 **;
		* OBJ: autoregressive;
		* see Appendix C for code

	** Step 1-5 **;
		* OBJ: autoregressive heterogeneous;
		* see Appendix C for code

	** Step 1-6 **;
		* OBJ: Toeplitz;
		* see Appendix C for code

	** Step 1-7 **;
		* OBJ: Toeplitz heterogeneous;
		* see Appendix C for code

	** Step 1-8 **;
		* OBJ: antedependence;
		* see Appendix C for code

	** RESULTS: 
		* A: arh(1) = best fit among repeated measures (higher than B step 2-1) and good pearson/DF
		* B: arh(1) = best fit among repeated measures (lower than B step 2-1) and good pearson/DF
		* C: ar(1) = similar fit to other two repeated measures structures without errors (lower than B step 2-1), good pearson/DF
	** CONCLUSION: try random block
	
	
*** Step 2 ***;
	** OBJ: add random block;

	** Step 2-1 **;
		* OBJ: autoregressive;
		* see Appendix C for code

	** Step 2-1 **;
		* OBJ: autoregressive heterogeneous;
		* see Appendix C for code

	** RESULTS: errors both steps, all experiments
	** CONCLUSION: no random block with repeated measures
		* try random block without repeated measures
		

* ************************************ *
* D. Analyze - Identify Base Model - 2 *
* ************************************ *;

*** Step 1 ***;
	** OBJ: test random block (from B step 2);

	** Step 1-1 **;
		* OBJ: add block to test time factor as separate fixed effect;
		* see Appendix D for code
		* RESULTS: slight improvement in fit, good improvement in pearson/DF vs B step 2-1

	** Step 1-2 **;
		* OBJ: test time factor as separate fixed effect;
		* see Appendix D for code
		* RESULTS: slight improvement in fit, good improvement in pearson/DF vs B step 2-2
			* pearson/DF acceptable for all experiments

	** CONCLUSION: use D step 1-2
	

* ************************* *
* E. Analyze - Main Effects *
* ************************* *;

*** Step 1 ***;
	** OBJ: analyze fixed effects with method=laplace removed and ddfm=kr2;

	** Step 1-1 **;
		* OBJ: analyze;
		%output_html_files_on(name_step=E_step-1-1, title_step=E step 1-1); * redirect html, log output to files;
		proc glimmix data=rating_incid plot=residualpanel;
			class experiment trt block date;
			by experiment;
			model infected_canes / canes = trt|date / dist=binomial link=logit htype=3 ddfm=kr2;
			random intercept trt / subject=block;
			title 'rcb incidence E analyze fixed effects - step 1 fixed effects - step 1-1 -laplace, +ddfm=kr2';
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: 
		* A: trt*date
		* B: date
		* C: trt*date
	** CONCLUSION: examine interactions


* **************************************** *
* F. Analyze - Examine significant effects *
* **************************************** *;

*** make dataset;
	data rating_incid_AC;
		set rating_incid;
		if experiment in ('B-3-row') then delete;
	run;

*** Step 1 ***;
	** OBJ: examine interactions;

	** Step 1-1 **;
		* OBJ: analyze;
			* note: commented out, not moved to appendix because although is model that was used, identical output repeated below;
*		%output_html_files_on(name_step=F_step-1-1, title_step=F step 1-1); * redirect html, log output to files;
*		proc glimmix data=rating_incid_AC;
*			class experiment trt block date;
*			by experiment;
*			model infected_canes / canes = trt|date / dist=binomial link=logit htype=3 ddfm=kr2;
*			random intercept trt / subject=block;
*			
*			slice trt*date / sliceBy=date;
*			
*			title 'rcb incidence F examine significant effects - step 1 interactions - step 1-1 examine A, C';
*			ods exclude DiffPlot;
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: 
		* A: 2018-04-11 only
		* C: 2018-10-17, weak 2018-11-07
	** CONCLUSION: examine means


*** Step 2 ***;
	** OBJ: examine means;

	** Step 2-1 **;
		* OBJ: analyze;
			* note: commented out, not moved to appendix to show motivation for using contrasts;
*		%output_html_files_on(name_step=F_step-2-1, title_step=F step 2-1); * redirect html, log output to files;
*		proc glimmix data=rating_incid_AC;
*			class experiment trt block date;
*			by experiment;
*			model infected_canes / canes = trt|date / dist=binomial link=logit htype=3 ddfm=kr2;
*			random intercept trt / subject=block;
*			
*			slice trt*date / sliceBy=date means ilink linestable adjust=tukey;
*			
*			title 'rcb incidence F examine significant effects - step 2 means - step 2-1 examine A, C';
*			ods exclude MeanPlot SliceDiffs DiffPlot;
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: 
		* A 04-11: blade ab, control a, manual ab, twine b
		* C
			* 10-17: no differences
			* 11-07: no differences
	** CONCLUSION: try single df contrasts


*** Step 3 ***;
	** OBJ: examine means with single df contrasts;

	** make datasets;
	data rating_incid_A;
		set rating_incid;
		if experiment in ('B-3-row','C-3-row') then delete;
	run;

	data rating_incid_C;
		set rating_incid;
		if experiment in ('A-2-row','B-3-row') then delete;
	run;

	** Step 3-1 **;
		* OBJ: contrast statements - exp A;
		%output_html_files_on(name_step=F_step-3-1, title_step=F step 3-1); * redirect html, log output to files;
		proc glimmix data=rating_incid_A;
			class experiment trt block date;
			by experiment;
			model infected_canes / canes = trt|date / dist=binomial link=logit htype=3 ddfm=kr2;
			random intercept trt / subject=block;
			
			slice trt*date / sliceBy=date means;
			
			estimate '2018-04-11: blade vs control'
				trt		1 -1 0 0
				trt*date 0 0 1 0 0 -1 0 0 0 0 0 0 / e;

			estimate '2018-04-11: manual vs control'
				trt		0 -1 1 0
				trt*date 0 0 0 0 0 -1 0 0 1 0 0 0 / e;

			estimate '2018-04-11: twine vs control'
				trt		0 -1 0 1
				trt*date 0 0 0 0 0 -1 0 0 0 0 0 1 / e;
			
			title 'rcb incidence F examine significant effects - step 3 mean contrasts - step 3-1 experiment A';
			ods exclude MeanPlot SliceDiffs DiffPlot;
			ods exclude Coef; * removes large table of coefficients, remove this statement (show table) for initial run to confirm coefficients are correct;
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;

	** Step 3-2 **;
		* OBJ: contrast statements - exp C;
		%output_html_files_on(name_step=F_step-3-2, title_step=F step 3-2); * redirect html, log output to files;
		proc glimmix data=rating_incid_C;
			class experiment trt block date;
			by experiment;
			model infected_canes / canes = trt|date / dist=binomial link=logit htype=3 ddfm=kr2;
			random intercept trt / subject=block;
			
			slice trt*date / sliceBy=date means;
			
			estimate '2018-10-17: blade vs control'
				trt		1 -1 0 0
				trt*date 1 0 0 -1 0 0 0 0 0 0 0 0 / e;

			estimate '2018-10-17: manual vs control'
				trt		0 -1 1 0
				trt*date 0 0 0 -1 0 0 1 0 0 0 0 0 / e;

			estimate '2018-10-17: twine vs control'
				trt		0 -1 0 1
				trt*date 0 0 0 -1 0 0 0 0 0 1 0 0 / e;

			estimate '2018-11-07: blade vs control'
				trt		1 -1 0 0
				trt*date 0 1 0 0 -1 0 0 0 0 0 0 0 / e;

			estimate '2018-11-07: manual vs control'
				trt		0 -1 1 0
				trt*date 0 0 0 0 -1 0 0 1 0 0 0 0 / e;

			estimate '2018-11-07: twine vs control'
				trt		0 -1 0 1
				trt*date 0 0 0 0 -1 0 0 0 0 0 1 0 / e;
			
			title 'rcb incidence F examine significant effects - step 3 mean contrasts - step 3-2 experiment C';
			ods exclude MeanPlot SliceDiffs DiffPlot;
			ods exclude Coef; * removes large table of coefficients, remove this statement (show table) for initial run to confirm coefficients are correct;
		run;
		%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS:
		* A 04-11: blade, weak manual, twine
		* C
			* 10-17: manual, twine
			* 11-07: manual
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
*		proc glimmix data=rating_incid plot=residualpanel method=laplace;
*			class experiment trt;
*			by experiment;
*			model infected_canes / canes = trt / dist=binomial link=logit htype=3;
*			title 'rcb incidence B id base model - step 1 most simple model - step 1-1 base';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: pearson/DF very high all three experiments
	** CONCLUSION: add rating date as fixed effect

*** Step 2 ***;
	** OBJ: test time factor;

*	** Step 2-1 **;
*		* OBJ: test time factor as separate fixed effect;
*		%output_html_files_on(name_step=B_step-2-1, title_step=B step 2-1); * redirect html, log output to files;
*		proc glimmix data=rating_incid plot=residualpanel method=laplace;
*			class experiment trt date;
*			by experiment;
*			model infected_canes / canes = trt date / dist=binomial link=logit htype=3;
*			title 'rcb incidence B id base model - step 2 test time factor - step 2-1 rating date seperate';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 2-2 **;
*		* OBJ: test time factor as separate fixed effect;
*		%output_html_files_on(name_step=B_step-2-2, title_step=B step 2-2); * redirect html, log output to files;
*		proc glimmix data=rating_incid plot=residualpanel method=laplace;
*			class experiment trt date;
*			by experiment;
*			model infected_canes / canes = trt|date / dist=binomial link=logit htype=3;
*			title 'rcb incidence B id base model - step 2 test time factor - step 2-2 rating date interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: significant improvement in fit, drastic (A,B) or good (C) improvement in pearson/DF
	** CONCLUSION: test repeated measures


* *************************************** *
* Appendix C. Analyze - Repeated Measures *
* *************************************** *;

*** Step 1 ***;
	** OBJ: base, no random block;

*	** Step 1-1 **;
*		* OBJ: unstructured;
*		%output_html_files_on(name_step=C_step-1-1, title_step=C step 1-1); * redirect html, log output to files;
*		proc glimmix data=rating_incid plot=residualpanel method=laplace;
*			class experiment trt date block;
*			by experiment;
*			model infected_canes / canes = trt / dist=binomial link=logit htype=3;
*			random date / type=un subject=trt*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb incidence C repeated measures - step 1 no random block - step 1-1 unstructured';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-2 **;
*		* OBJ: compound symmetry;
*		%output_html_files_on(name_step=C_step-1-2, title_step=C step 1-2); * redirect html, log output to files;
*		proc glimmix data=rating_incid plot=residualpanel method=laplace;
*			class experiment trt date block;
*			by experiment;
*			model infected_canes / canes = trt / dist=binomial link=logit htype=3;
*			random date / type=cs subject=trt*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb incidence C repeated measures - step 1 no random block - step 1-2 compound symmetry';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-3 **;
*		* OBJ: compound symmetry heterogeneous;
*		%output_html_files_on(name_step=C_step-1-3, title_step=C step 1-3); * redirect html, log output to files;
*		proc glimmix data=rating_incid plot=residualpanel method=laplace;
*			class experiment trt date block;
*			by experiment;
*			model infected_canes / canes = trt / dist=binomial link=logit htype=3;
*			random date / type=csh subject=trt*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb incidence C repeated measures - step 1 no random block - step 1-3 compound symmetry heterogeneous';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-4 **;
*		* OBJ: autoregressive;
*		%output_html_files_on(name_step=C_step-1-4, title_step=C step 1-4); * redirect html, log output to files;
*		proc glimmix data=rating_incid plot=residualpanel method=laplace;
*			class experiment trt date block;
*			by experiment;
*			model infected_canes / canes = trt / dist=binomial link=logit htype=3;
*			random date / type=ar(1) subject=trt*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb incidence C repeated measures - step 1 no random block - step 1-4 autoregressive';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-5 **;
*		* OBJ: autoregressive heterogeneous;
*		%output_html_files_on(name_step=C_step-1-5, title_step=C step 1-5); * redirect html, log output to files;
*		proc glimmix data=rating_incid plot=residualpanel method=laplace;
*			class experiment trt date block;
*			by experiment;
*			model infected_canes / canes = trt / dist=binomial link=logit htype=3;
*			random date / type=arh(1) subject=trt*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb incidence C repeated measures - step 1 no random block - step 1-5 autoregressive heterogeneous';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-6 **;
*		* OBJ: Toeplitz;
*		%output_html_files_on(name_step=C_step-1-6, title_step=C step 1-6); * redirect html, log output to files;
*		proc glimmix data=rating_incid plot=residualpanel method=laplace;
*			class experiment trt date block;
*			by experiment;
*			model infected_canes / canes = trt / dist=binomial link=logit htype=3;
*			random date / type=toep subject=trt*block;
*			covtest / wald;
*			covtest 'glm' glm;
*		title 'rcb incidence C repeated measures - step 1 no random block - step 1-6 Toeplitz';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-7 **;
*		* OBJ: Toeplitz heterogeneous;
*		%output_html_files_on(name_step=C_step-1-7, title_step=C step 1-7); * redirect html, log output to files;
*		proc glimmix data=rating_incid plot=residualpanel method=laplace;
*			class experiment trt date block;
*			by experiment;
*			model infected_canes / canes = trt / dist=binomial link=logit htype=3;
*			random date / type=toeph subject=trt*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb incidence C repeated measures - step 1 no random block - step 1-7 Toeplitz heterogeneous';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 1-8 **;
*		* OBJ: antedependence;
*		%output_html_files_on(name_step=C_step-1-8, title_step=C step 1-8); * redirect html, log output to files;
*		proc glimmix data=rating_incid plot=residualpanel method=laplace;
*			class experiment trt date block;
*			by experiment;
*			model infected_canes / canes = trt / dist=binomial link=logit htype=3;
*			random date / type=ante(1) subject=trt*block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb incidence C repeated measures - step 1 no random block - step 1-8 antedependence';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: 
		* A: arh(1) = best fit among repeated measures (higher than B step 2-1) and good pearson/DF
		* B: arh(1) = best fit among repeated measures (lower than B step 2-1) and good pearson/DF
		* C: ar(1) = similar fit to other two repeated measures structures without errors (lower than B step 2-1), good pearson/DF
	** CONCLUSION: try random block
	
	
*** Step 2 ***;
	** OBJ: add random block;

*	** Step 2-1 **;
*		* OBJ: autoregressive;
*		%output_html_files_on(name_step=C_step-2-1, title_step=C step 2-1); * redirect html, log output to files;
*		proc glimmix data=rating_incid plot=residualpanel method=laplace inititer=250;
*			class experiment trt date block;
*			by experiment;
*			model infected_canes / canes = trt / dist=binomial link=logit htype=3;
*			random date / type=ar(1) subject=trt*block;
*			random intercept trt / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb incidence C repeated measures - step 2 with random block - step 2-1 autoregressive';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*
*	** Step 2-2 **;
*		* OBJ: autoregressive heterogeneous;
*		%output_html_files_on(name_step=C_step-2-2, title_step=C step 2-2); * redirect html, log output to files;
*		proc glimmix data=rating_incid plot=residualpanel method=laplace inititer=250;
*			class experiment trt date block;
*			by experiment;
*			model infected_canes / canes = trt / dist=binomial link=logit htype=3;
*			random date / type=arh(1) subject=trt*block;
*			random intercept trt / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb incidence C repeated measures - step 2 with random block - step 2-2 autoregressive heterogeneous';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

	** RESULTS: errors both steps, all experiments
	** CONCLUSION: no random block with repeated measures
		* try random block without repeated measures


* ********************************************* *
* Appendix D. Analyze - Identify Base Model - 2 *
* ********************************************* *;

*** Step 1 ***;
	** OBJ: test random block (from B step 2);

*	** Step 1-1 **;
*		* OBJ: add block to test time factor as separate fixed effect;
*		%output_html_files_on(name_step=D_step-1-1, title_step=D step 1-1); * redirect html, log output to files;
*		proc glimmix data=rating_incid plot=residualpanel method=laplace;
*			class experiment trt block date;
*			by experiment;
*			model infected_canes / canes = trt date / dist=binomial link=logit htype=3;
*			random intercept trt / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb incidence D id base model 2 - step 1 random block - step 1-1 rating date seperate';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;
*		
*		* RESULTS: slight improvement in fit, good improvement in pearson/DF vs B step 2-1
*
*	** Step 1-2 **;
*		* OBJ: test time factor as separate fixed effect;
*		%output_html_files_on(name_step=D_step-1-2, title_step=D step 1-2); * redirect html, log output to files;
*		proc glimmix data=rating_incid plot=residualpanel method=laplace;
*			class experiment trt block date;
*			by experiment;
*			model infected_canes / canes = trt|date / dist=binomial link=logit htype=3;
*			random intercept trt / subject=block;
*			covtest / wald;
*			covtest 'glm' glm;
*			title 'rcb incidence D id base model 2 - step 1 random block - step 1-2 rating date interaction';
*		run;
*		%output_html_files_off(); * turn off redirecting html, log output to files;

		* RESULTS: slight improvement in fit, good improvement in pearson/DF vs B step 2-2
			* pearson/DF acceptable for all experiments

	** CONCLUSION: use D step 1-2

