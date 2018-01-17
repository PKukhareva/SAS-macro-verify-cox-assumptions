%let odsdir=J:\FAVORIT\Sc\UCCPVK\2013_SAS_Verify_cox_assumptions;
libname rq "&odsdir";
options nodate mprint ps=58 ls=175 fmtsearch=(work, FLIB.stand802) pageno=1 mergenoby=warn formchar='|----|+|---+=|-/\<>*' source2 mprint mlogic symbolgen;

%include 'J:\FAVORIT\Sc\UCCPVK\2013_SAS_Verify_cox_assumptions\verify_cox_assumptions_all.sas';

%let predictors=age systolic_bp diastolic_bp ldl bmi diabetes smoking sex treatment activity;
%let class=diabetes(ref="0") smoking(ref="0") sex(ref="0") treatment(ref="0") activity(ref="average");

%check_cox_assumptions_all (_Number=1, _Data=rq.simcox, _predictors=&predictors, _class=&class, _OUTCOME=CV_event, 
_FU=CV_time, _CHECK=AGE ACTIVITY,_STOP=activity, _EXCLUDE=systolic_bp diastolic_bp ldl bmi diabetes smoking sex)
%check_cox_assumptions_all (_Number=2, _Data=rq.simcox, _predictors=&predictors, _OUTCOME=CV_event, _FU=CV_time, _class=&class, _STOP=activity)
%check_cox_assumptions_all (_Number=3, _Data=rq.simcox, _predictors=&predictors, _class=&class, _OUTCOME=CV_event, _FU=CV_time, _CHECK=AGE, _EXCLUDE=sex)
%check_cox_assumptions_all (_Number=4, _Data=rq.simcox, _predictors=&predictors, _class=&class, _OUTCOME=CV_event, _FU=CV_time, _CHECK=AGE, _INTERACTIONS=age*Systolic_bp systolic_bp*diastolic_bp)
%check_cox_assumptions_all (_Number=5, _Data=rq.simcox, _predictors=&predictors, _OUTCOME=CV_event, _FU=CV_time, _STOP=systolic_bp)
%check_cox_assumptions_all (_Number=6, _Data=rq.simcox, _predictors=&predictors, _class=&class, _OUTCOME=CV_event, _FU=CV_time, _CHECK=AGE)
%check_cox_assumptions_all (_Number=7, _Data=rq.simcox, _predictors=AGE, _class=&class, _OUTCOME=CV_event, _FU=CV_time)
%check_cox_assumptions_all (_Number=8, _Data=rq.simcox, _predictors=diabetes, _class=&class, _OUTCOME=CV_event, _FU=CV_time)
