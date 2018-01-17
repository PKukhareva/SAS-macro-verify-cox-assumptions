*  MACRO:        check_cox_assumptions_all
*
*  DESCRIPTION:  Checks assumptions for Cox PH model 
*  
*  SOURCE: 		 CSCC, UNC at Chapel Hill
*
*  PROGRAMMER:   Polina Kukhareva
*
*  DATE:         11/01/2012
*
*  HISTORY:      check_cox_assumptions_all -- Kukhareva -- 11/01/2012
*  
*  LANGUAGE:     SAS VERSION 9.3
*
*  INPUT:        SAS data set containing survival data
*
*  OUTPUT:       RTF file with graphs and lst file with model run
*******************************************************************;

/* 
Input:
Specify parameters as following:
_NUMBER= User defined number of model which will appear in the title;
_DATA= Name of the data set containing survival data;
_PREDICTORS= List of predictors variables which can have some extra variables;
_CLASS= List of the variables included in a class statement. Can spesify reference categories in parantesis.; 
_OUTCOME= Name of the outcome variable, event coded as 1, censored coded as  0; 
_FU= Follow up time;
_STOP= The last variable from predictor list user wants to include in the model; 
_EXCLUDE= List of variables from predictor list which shouldn't be in the model statement, if no variables need to be excluded put 'NOTHING';
_CHECK= List of variables for which user wants to produce diagnostics;
_RQ=Characters to be included in the name of output RTF file
_INTERACTIONS=List of interactions of interest, e.g. sex*age ,
_FOOTNOTE=Footnote

Example:
%check_cox_assumptions_all (_NUMBER=1, _DATA=rq.simcox, _PREDICTORS=&predictors, 
_CLASS=diabetes(ref="0") smoking(ref="0") sex(ref="0") treatment(ref="0") activity(ref="1"), _OUTCOME=CV_event, _FU=CV_time, _STOP=activity,
_EXCLUDE=sex, _RQ=job1, _INTERACTIONS=sex*age, _FOOTNOTE=%str(&sysdate, &systime -- produced by macro check_cox_assumptions))

Output:
For each continuous predictors this check_cox_assumptions_all macro provides:
1.plot of martingale residuals from a model that excludes the variable being checked by the variable to check linearity assumption
2.plot of dummy variables with a smoothing function to check linearity to check linearity assumption
3.Log-negative-log survival curves with dummy variables to check proportional hazard assumption
4.Schoenfeld residuals plots  to check proportional hazard assumption
5.Check for confounding (comparing crude and adjusted models)
6.Check for interactions with all other predictors

For each categorical predictors  check_cox_assumptions_all macro provides:
1.Log-negative-log survival curves to check proportional hazard assumption
2.Schoenfeld residuals plots to check proportional hazard assumption
3.Check for confounding (comparing crude and adjusted models)
4.Check for interactions with all other predictors

*/

%macro check_cox_assumptions_all (_NUMBER= /*User defined NUMBER of model which will appear in the title, e.g. 1*/,
                                  _DATA=   /*Name of the data set containing survival data, e.g. rq.simcox*/,
                                  _PREDICTORS=/*List of predictors variables which can have some extra variables, e.g. age sex treatment*/, 
                                  _CLASS=/*List of the variables included in a class statement, e.g. sex(ref="0") treatment(ref="0")*/, 
                                  _OUTCOME=/*Name of the outcome variable, event coded as 1, censored coded as 0, e.g. CV_event*/, 
                                  _FU=/*Follow up time, e.g. CV_time*/, 
                                  _STOP=_NOT_SPECIFIED/*The last variable from predictor list user wants to include in the model, e.g. sex*/, 
                                  _EXCLUDE=_NOTHING/*List of variables from predictor list which shouldn't be in the model statement, e.g. age*/, 
                                  _CHECK=_ALL/*List of variables for which user wants to produce diagnostics, e.g. sex*/, 
                                  _RQ=cox_assumptions/*Characters to be included in the name of output RTF file, e.g. cox_assumptions*/,
								  _INTERACTIONS=_ALL /*List of interactions of interest, e.g. sex*age */,
								  _FOOTNOTE=%str(&sysdate, &systime -- produced by macro check_cox_assumptions) /*Footnote*/) /minoperator;


%macro check_cox_assumptions (NUMBER=, Data=, predictors=, class=, OUTCOME=, FU=, STOP=, EXCLUDE=, CHECK=)/minoperator;
/* Creating lists of predictor variables */

ods rtf exclude all;
	proc sql;
		select distinct predictor into :predictors_WITHOUT_check separated by ' '
			from predictors_&NUMBER  
			where left(predictor)^="&CHECK" 
			order by order; 
	quit;
    %put predictors_WITHOUT_check=&predictors_WITHOUT_check;

ods output ClassLevelInfo=ClassLevelInfo_&NUMBER;
	proc phreg data=&data;
 		where &FU>0;
		 class &class;
		model &FU*&outcome (0)= &predictors_used /ties=exact rl;
	run;
ods output close;

/* data step to delete reference categories*/
%if %sysfunc(exist(ClassLevelInfo_&NUMBER)) %then %do;
	%if (%eval(%sysfunc(countw(%str(&class))))=0) %then %do; 
	%let class_variables=_EMPTY_LIST_; 
	proc sql;
		create table schoenfeld as
		select *, compress(catx('',predictor,'_')," -$%^()&#@","c") as schoenfeld
		from Predictors_&NUMBER
		order by order;
	quit; 
	%end;

	%else %do;
	data ClassLevelInfo_&NUMBER;
	length class $30 value$30;
	set ClassLevelInfo_&NUMBER;
			 class=UPCASE(CLASS);
			 if ^missing(class) then call symput ('new',class);
			 else class=symget('new');
			 if sum(of X:)=0 then delete;
	run;
	proc sql;
		select distinct class into :class_variables separated by ' '
		from ClassLevelInfo_&NUMBER; 
	quit;
	proc sql;
		create table schoenfeld
		as select predictor, order, value, compress(catx('',predictor,'_',left(value))," -$%^()&#@","c") as schoenfeld
		from  Predictors_&NUMBER left join ClassLevelInfo_&NUMBER on left(class)=left(predictor)
		order by order, schoenfeld; 
	quit;
	%end;

	proc sql;
		select schoenfeld into :schoenfeld separated by ' '
		from schoenfeld; 
	quit;
%end;
%else %do; %let class_variables=_EMPTY_LIST_; %let schoenfeld=%str(); %end;
%put predictors=&predictors_used; 
%put class_variables=&class_variables; 
%put schoenfeld = &schoenfeld;
ods rtf select all;
/* End of: Creating lists of variables */

/* block # 1 */
%if (&CHECK in &class_variables)=0 %then %do;
%if %eval(%sysfunc(countw(&PREDICTORS))) > 1 %then %do;
title2 height=12pt  'Verify the linearity assumption of continuous predictor variables';
title3 height=10pt  "Assess the Linearity Assumption of &CHECK by Plotting Martingale Residuals";
ods rtf startpage=NOW; 
ods rtf exclude all;

	proc phreg data=&data;
		where &FU>0;
		class &class;
		model &FU*&outcome (0)= &predictors_WITHOUT_check /ties=exact rl;
		id &CHECK;
		output out=mart resmart=mart;
	run;

ods rtf select all; 

	proc sgplot data=mart;
		scatter y=mart x=&CHECK/ name='Martingale';
		loess y=mart x=&CHECK/SMOOTH=.8 DEGREE=1 INTERPOLATION= CUBIC ;
		keylegend 'Martingale'/ title=' ' Position=bottom;
	run;

	ODS TEXT='To use the Martingale residuals to assess the linearity assumption, first fit a
	model that excludes the predictor variable of interest. Then plot the
	Martingale residuals by the values of the excluded predictor variable of
	interest. Finally, fit a smoothed line (that is, a loess smooth) in the plot. The
	shape of the smoothed line provides an estimate of the functional form of the
	predictor variable in the model (Therneau, Grambsch, and Fleming 1990).';
%end;
title3 height=10pt  "Assess the Linearity Assumption of &CHECK by Plotting Dummy Estimates";
ods rtf startpage=NOW;
ods rtf exclude all;

		proc rank data=&data groups=5 out=rank&CHECK;
			var &CHECK;
			ranks group&CHECK;
		run;

		data rank&CHECK;
			set rank&CHECK;
	  			d0=(group&CHECK=0);
	  			d1=(group&CHECK=1);
	  			d2=(group&CHECK=2);
	  			d3=(group&CHECK=3);
	  			d4=(group&CHECK=4);
			run;

ods output parameterestimates=parms&CHECK;
		proc phreg data=rank&CHECK;
 			where &FU>0;
			 class &class;
			model &FU*&outcome (0)=  
				%let cnt=0;
      				%do  %until (%qscan(&predictors,&cnt, %str( ))=%str(&stop));
          				%let var=%qscan(&predictors,%eval(&cnt+1),%str( ));
              			%if not (&var in %str(&CHECK)) %then %do; &var %end;
           				%let cnt=%eval(&cnt+1);
       			%end; d4 d3 d2  d1 d0 /ties=exact rl;
		run;
ods output close;

		data parms&CHECK;
			set parms&CHECK;
				where parameter in ('d0', 'd1', 'd2', 'd3', 'd4');
				num_parm=input(substr(parameter, 2, 1),1.);
		run;


ods rtf select all; 
		proc means data=rank&CHECK min max maxdec=2;
			class group&CHECK;
			var &CHECK;
		run;


		proc sgplot data=parms&CHECK;
			scatter y=estimate x=num_parm;
			loess y=estimate x=num_parm/SMOOTH=.8 DEGREE=1 INTERPOLATION= CUBIC ;
		run;

	ODS TEXT ="One method to check for linearity is to replace the predictor variable of
	interest with several dummy variables. Then fit the model with the new dummy variables and plot the parameter estimates of the dummy variables, with a point at zero representing the reference group.
	If the correct scale is linear in the log hazard, then a smoothed line is fairly linear. If the smoothed line departs substantially from a linear trend, then its
	form is used to decide how to model the predictor variable (Hosmer and Lemeshow 1999).";

title2 height=12pt  'Verify the proportional hazards assumption of predictor variables';
title3 height=10pt  "Assess the PH Assumption of &CHECK by Plotting LLS";

ods rtf startpage=NOW; 
ods rtf exclude all;

%end; 

/* end of: block # 1 */

/* block # 2 */

/* proportional hazards assumption of continuous predictor variables */

%if (&CHECK in &class_variables)=0 %then %do;
ods graphics on;

ods rtf select LogNegLogSurvivalPlot;


		proc lifetest data=rank&CHECK outsurv=&CHECK._lifetest NOTABLE method=km plots=lls;
    		where &fu>0;
    		time &fu*&outcome(0);
			strata group&CHECK;
		run;

ODS TEXT ="One method to assess the proportional hazards assumption is to plot the
log-negative-log of the Kaplan-Meier estimates of the survival function versus 
the log of time. If the hazard ratios are constant, the plot has parallel lines.";

title3 height=10pt  "Assess the PH Assumption of &CHECK by Plotting Schoenfeld Residuals";
ods rtf startpage=NOW;
ods rtf exclude all;
ods graphics off;
%end; 

/* proportional hazards assumption of categorical predictor variables */

%if &CHECK in &class_variables %then %do;
title2 height=12pt  'Verify the proportional hazards assumption of predictor variables';
title3 height=10pt  "Assess the PH Assumption of &CHECK by Plotting LLS";
ods rtf startpage=NOW; 
ods rtf exclude all;
ods graphics on;
ods rtf select LogNegLogSurvivalPlot;


		proc lifetest data=&data outsurv=&CHECK._lifetest NOTABLE method=km plots=lls;
    		where &fu>0;
    		time &fu*&outcome(0);
			strata &CHECK;
		run;

ODS TEXT ="One method to assess the proportional hazards assumption is to plot the
log-negative-log of the Kaplan-Meier estimates of the survival function versus
the log of time. If the hazard ratios are constant, the plot has parallel lines.";

ods graphics off;

title3 height=10pt  "Assess the PH Assumption of &CHECK by Plotting Schoenfeld Residuals";
ods rtf startpage=NOW;
ods rtf exclude all;
%end; 
/* proportional hazards assumption of categorical and continuous predictor variables */


		proc phreg data=&data noprint;
 			where &FU>0;
			 class &class;
			model &FU*&outcome (0)=  &predictors_used /ties=exact rl;
			output out=ressch_&CHECK ressch=&schoenfeld;
		run;
ods rtf select all; 
/* Producing as many plots as categories in check variable */
	   %let cntsch=0;
	%do  i=1 %to %sysfunc(countw(&schoenfeld));
        %let category=%qscan(&schoenfeld, &i,%str( ));
        %if %upcase(%substr(&category,1, %sysfunc(length(&CHECK))))=%upcase(&CHECK) %then %do;
		proc sgplot data=ressch_&CHECK;
			scatter y=&category x=&fu/ name="Schoenfeld_&CHECK";
			loess y=&category x=&fu/SMOOTH=.8 DEGREE=1 INTERPOLATION= CUBIC ;
			keylegend "Schoenfeld_&CHECK"/ title=' ' Position=bottom;
		run;

ODS TEXT="Because Schoenfeld residuals are based on the effects of the predictor 
variables that are assumed to be independent of time, a plot of the residuals 
by time could be used to visually assess whether the effect of the predictor 
variable changes over the period of follow-up. A smoothed line fit to the plot of
the residuals should have an intercept and a slope of approximately 0 (Hosmer and 
Lemeshow 1999)."; 
		%end;
	%end;


/* end of: block # 2 */



/* block # 3 */
%if %eval(%sysfunc(countw(&PREDICTORS))) > 1 %then %do;
%if %qscan(&_INTERACTIONS, 1) = _ALL %then %do;
title2 height=12pt  'Assess the model for interactions';
title3 height=10pt  "Check for interactions &CHECK";

ods rtf startpage=NOW;
ods rtf exclude all;

	data &CHECK._interactions;
	run;

	%do  int_count=1 %to %sysfunc(countw(&predictors_WITHOUT_check));
   		%let var_for_interaction=%qscan(&predictors_WITHOUT_check, &int_count,%str( ));
   		%let INTERACTION=&CHECK.*&var_for_interaction;
				%if (%eval(&int_count) > %eval(%sysfunc(countw(&predictors_used)))) %then %do; 
			%put ERROR CHECK SPECIFICATION OF PARAMETERS;
				%return;  %end;
 
ods output ParameterEstimates = interaction_&CHECK;
		proc phreg data=&data;
			where &FU>0;
			 class &class;
			model &FU*&outcome (0)=  &predictors_used &INTERACTION /ties=exact;
		run;
ods output close;

		data &CHECK._interactions;
			length parameter $30.;
			set &CHECK._interactions interaction_&CHECK (where=( parameter contains '*'));
		run;
	%end;

	data &CHECK._interactions;
		set &CHECK._interactions;
			if missing(parameter) then delete;
			if ProbChiSq < 0.05 then flag='*';
	run;

ods rtf select all; 
	proc print data=&CHECK._interactions noobs label;
		var parameter label estimate stderr chisq ProbChiSq flag;
	run;

ODS TEXT="Results of a partial likelihood ratio test that compares the 
model with only the main effects to the model with the main 
effects and all two-way interactions. P-values less than 0.05 are marked";
%end;
%end;
/* end of: block # 3 */

/* block # 4 */
%if %eval(%sysfunc(countw(&PREDICTORS))) > 1 %then %do;
title2 height=12pt  'Assess the model for confounders';
title3 height=10pt  "Check for Confounders adjusting for &CHECK";
ods rtf startpage=NOW;
ods rtf exclude all;

ods output ParameterEstimates=&CHECK._adjusted;
		proc phreg data=&data;
			where &FU>0;
			 class &class;
			model &FU*&outcome (0)= &predictors_used /ties=exact rl;
		run;
ods output close;
ods output ParameterEstimates=&CHECK._crude;
		proc phreg data=&data;
			where &FU>0;
			 class &class;
			model &FU*&outcome (0)= &predictors_WITHOUT_check /ties=exact rl;
			id &CHECK;
		run;
ods output close;

ods rtf select all; 

	proc sort data=&CHECK._adjusted;
		by parameter %if &class_variables ne  _EMPTY_LIST_  %then %do; classval0 %end;;
	run;

	proc sort data=&CHECK._crude;
		by parameter %if &class_variables ne  _EMPTY_LIST_  %then %do; classval0 %end;;
	run;

	data table_&CHECK;
		merge &CHECK._adjusted(rename=(estimate=estimate1 stderr=stderr1 hazardratio=hazardratio1 HRLOWERCL=HRLOWERCL1 HRupperCL=HRupperCL1)) &CHECK._crude;
			by parameter %if &class_variables ne  _EMPTY_LIST_  %then %do; classval0 %end;;
			drop  HRLowerCL  HRUpperCL  ProbChiSq DF chisq;
			if parameter="&CHECK" then delete;
			change=(estimate-estimate1)/estimate;
			if abs(change)>0.2 and (abs(estimate)>0.01 or abs(estimate1)>0.01) then flag='*'; 
			HR=    put(hazardratio,5.2)||' ('||put(HRLOWERCL,5.2)||'-'||put(HRupperCL,5.2)||')';
			HR1=    put(hazardratio1,5.2)||' ('||put(HRLOWERCL1,5.2)||'-'||put(HRupperCL1,5.2)||')';
			EST=put(estimate,5.2)||' ('||put(stderr,5.2)||')';
			EST1=put(estimate1,5.2)||' ('||put(stderr1,5.2)||')';
			label est1='Adjusted Estimate (Error)' est='Crude Estimate (Error)' HR='Crude Hazard Ratio (95% CI)' HR1='Adjusted Hazard Ratio (95% CI)';
			if upcase(parameter) =upcase("&CHECK") then delete;
		run;

	proc print data=table_&CHECK noobs label;
 		format change percent.;
		var parameter label est hr est1  hr1 change flag;
 	run;

ODS TEXT="Estimating change between Parameter Estimates for Other variables varies significantly for model without (crude) or with (adjusted) 
variable of interest. Variables for which parameter estimates varies by more than 20% are flaged.
Variables are flaged only if at least one of the estimates is larger then 0.01.";
/* end of: block # 4 */
ods rtf startpage=NOW;
ods rtf exclude all;
	proc datasets lib=work memtype=data;
		delete table_&CHECK &CHECK._ADJUSTED &CHECK._CRUDE &CHECK._INTERACTIONS INTERACTION_&CHECK ClassLevelInfo_&NUMBER
							&CHECK._LIFETEST	MART PARMS&CHECK	RANK&CHECK RESSCH_&CHECK SCHOENFELD;
	run; quit;
ods rtf select all;

%end;
%mend check_cox_assumptions;
options nodate mprint pageno=1 mergenoby=warn ;
ods rtf file="&_RQ._model_&_NUMBER..rtf" style=statistical;
title1 height=13pt "Model &_NUMBER.: modeling &_fu.*&_outcome.(0) ";
ods rtf startpage=NO;
footnote1 J=right height=7pt &_FOOTNOTE;
ods rtf exclude all;
ods listing close;

%if &_Stop=_NOT_SPECIFIED %then %do; %let _Stop=%qscan(%UPCASE(&_PREDICTORS),-1, %str( )); %end;

%let exclude=%str(%UPCASE(&_EXCLUDE));

	data predictors_&_NUMBER;
		%let cnt=0;
      	format predictor $30.;
			%do  %until (%qscan(%UPCASE(&_PREDICTORS),&cnt, %str( ))=%str(%UPCASE(&_stop)));

				%if (%eval(&cnt) = %eval(%sysfunc(countw(&_PREDICTORS)))) %then %do; 
						%put ERROR: CHECK SPECIFICATION OF PARAMETERS; 
					%return;  %end;

   				%let var=%qscan(%UPCASE(&_PREDICTORS),%eval(&cnt+1),%str( ));

  				%if  ((&var in &exclude)=0) %then %do; 
					order=&cnt;
					predictor="&var";
					output; 
				%end;

    			%let cnt=%eval(&cnt+1);
			%end;
	run;

	proc sql;
		select distinct predictor into :predictors_used separated by ' '
		from predictors_&_NUMBER 
		order by order; 
	quit;

	%put predictors_used=&predictors_used;

	ods listing;
	proc phreg data=&_DATA;
 		where &_FU>0;
        class &_CLASS ;
		model &_FU*&_outcome (0)= &predictors_used /ties=exact rl;
	run;
	ods listing close;
%if &_check =_ALL %then %do;
	%do  all_count=1 %to %sysfunc(countw(&predictors_used));
   		%let CHECK_VAR=%qscan(&predictors_used, &all_count,%str( ));

			%check_cox_assumptions (NUMBER=%UNQUOTE(%UPCASE(&_NUMBER)), Data=&_DATA, predictors=%UPCASE(&_PREDICTORS), class=%UNQUOTE(&_CLASS), OUTCOME=&_OUTCOME, FU=&_FU, 
							STOP=%UPCASE(&_STOP), EXCLUDE=%UPCASE(&_EXCLUDE), CHECK=%UNQUOTE(%UPCASE(&CHECK_VAR)))
	%end;
%end;
%else %do;
	%do  all_count=1 %to %sysfunc(countw(&_CHECK));
   		%let CHECK_VAR=%qscan(&_CHECK, &all_count,%str( ));

			%check_cox_assumptions (NUMBER=%UNQUOTE(%UPCASE(&_NUMBER)), Data=&_DATA, predictors=%UPCASE(&_PREDICTORS), class=%UNQUOTE(&_CLASS), OUTCOME=&_OUTCOME, FU=&_FU, 
							STOP=%UPCASE(&_STOP), EXCLUDE=%UPCASE(&_EXCLUDE), CHECK=%UNQUOTE(%UPCASE(&CHECK_VAR)))
	%end; 
%end;

%if %qscan(&_INTERACTIONS, 1) NE _ALL %then %do;

title2 height=12pt  'Assess the model for interactions';
	data _interactions;
	run;
ods rtf startpage=NOW; 
ods rtf exclude all;

	%do  int_count=1 %to %sysfunc(countw(&_INTERACTIONS,%str( )));
   		%let INTERACTION=%qscan(&_INTERACTIONS, &int_count,%str( ));

ods output ParameterEstimates = interactions_&int_count;
		proc phreg data=&_DATA;
			where &_FU>0;
			class &_CLASS;
			model &_FU*&_outcome (0)=  &predictors_used &INTERACTION /ties=exact;
		run;
ods output close;

		data _interactions;
			length effect $30.;
			set _interactions interactions_&int_count (where=( parameter contains '*'));
		run;
	%end;


	data _interactions;
		set _interactions;
			if missing(parameter) then delete;
			if ProbChiSq < 0.05 then flag='*';
	run;

ods rtf select all; 
	proc print data=_interactions noobs label;
		var parameter label estimate stderr chisq ProbChiSq flag;
 	run;

ODS TEXT="Results of a partial likelihood ratio test that compares the 
model with only the main effects to the model with the main 
effects and all two-way interactions. P-values less than 0.05 are marked";

ods rtf startpage=NOW; 
ods rtf exclude all;

proc datasets lib=work memtype=data;
		delete _interactions predictors_&_NUMBER;
	quit;run; 

%end;

ods rtf close;
footnote;
title; 

%mend check_cox_assumptions_all;

