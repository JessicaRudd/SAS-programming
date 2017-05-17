Data model;
set bigdata.diabetes_model_ns;
run;

proc contents data=model;
run;

proc freq data=model;
tables HLTHPLN1 MARITAL SEX _DRDXAR1 _EDUCAG _INCOMG _PAINDX1 _RFBMI5 _RFCHOL
 _RFHYPE5 _RFSMOK3 race alldiab_bin _age_g;
run;

%let predicts_ng = HLTHPLN1 _HCVU651 _DRDXAR1 _AGE_G _EDUCAG _INCOMG SEX
race _PAINDX1 _RFBMI5 MARITAL _RFSMOK3 _RFHYPE5 _RFCHOL;

%let predicts_all = centr_degree_out centr_eigen_wt centr_influence1_wt centr_influence2_wt HLTHPLN1
_HCVU651 _DRDXAR1 _AGE_G centr_close_wt  _EDUCAG _INCOMG SEX
race centr_cluster _PAINDX1 _RFBMI5 community_1 MARITAL _RFSMOK3 _RFHYPE5 _RFCHOL ;

/* SVM with polynomial kernel, various C */
proc hpsvm data=model method=activeset;
input &predicts_ng  /level=nominal order=asc;
input _FRUTSUM _VEGESUM/level=interval;
partition  fraction(validate=.2 seed=12345);
KERNEL POLYNOM / DEGREE=2;
penalty c= .2 .5 1 1.5 2;
select cv=random fold=5 seed=34567;
target alldiab_bin;
output outclass=bigdata.outclass outfit=bigdata.outfit outest=bigdata.outest;
run;

proc svmscore data=model out=score
inclass=outclass infit=outfit inest=outest;
run;

/*SVM with linear kernel*/
proc hpsvm data=model method=activeset;
input &predicts_ng  /level=nominal order=asc;
input _FRUTSUM _VEGESUM/level=interval;
partition  fraction(validate=.2 seed=12345);
KERNEL linear;
penalty c= .2 .5 1 1.5 2;
select cv=random fold=5 seed=34567;
target alldiab_bin;
output outclass=bigdata.outclass_lin outfit=bigdata.outfit_lin outest=bigdata.outest_lin;
run;

/*SVM with rbf kernel*/
proc hpsvm data=model method=activeset;
input &predicts_ng  /level=nominal order=asc;
input _FRUTSUM _VEGESUM/level=interval;
partition  fraction(validate=.2 seed=12345);
KERNEL rbf/k_par=1;
penalty c= .2 .5 1 1.5 2;
select cv=random fold=5 seed=34567;
target alldiab_bin;
output outclass=bigdata.outclass_lin outfit=bigdata.outfit_lin outest=bigdata.outest_lin;
run;


******************* With Graph Attributes *************;
/*SVM with rbf kernel - with graph vars*/
proc hpsvm data=model method=activeset;
input &predicts_all  /level=nominal order=asc;
input _FRUTSUM _VEGESUM/level=interval;
partition  fraction(validate=.2 seed=12345);
KERNEL rbf/k_par=1;
penalty c= .2 .5 1 1.5 2;
select cv=random fold=5 seed=34567;
target alldiab_bin;
output outclass=bigdata.outclass_lin outfit=bigdata.outfit_lin outest=bigdata.outest_lin;
run;

/* SVM with polynomial kernel, various C */
proc hpsvm data=model method=activeset;
input &predicts_all  /level=nominal order=asc;
input _FRUTSUM _VEGESUM/level=interval;
partition  fraction(validate=.2 seed=12345);
KERNEL POLYNOM / DEGREE=2;
penalty c= .2 .5 1 1.5 2;
select cv=random fold=5 seed=34567;
target alldiab_bin;
output outclass=bigdata.outclass outfit=bigdata.outfit outest=bigdata.outest;
run;

/*SVM with linear kernel*/
proc hpsvm data=model method=activeset;
input &predicts_all  /level=nominal order=asc;
input _FRUTSUM _VEGESUM/level=interval;
partition  fraction(validate=.2 seed=12345);
KERNEL linear;
penalty c= .2 .5 1 1.5 2;
select cv=random fold=5 seed=34567;
target alldiab_bin;
output outclass=bigdata.outclass_lin outfit=bigdata.outfit_lin outest=bigdata.outest_lin;
run;


/*Logistic Regression */
proc sort data=bigdata.diabetes_model_ns;
by alldiab_bin;
run;
Proc surveyselect data=bigdata.diabetes_model_ns samprate=.8 out=samp seed=1234567 outall;
strata alldiab_bin;
run;

Proc freq data=samp;
tables alldiab_bin*selected;
run;

Data train valid;
set samp;
if selected then output train;
else output valid;
run;

proc freq data=train;
tables alldiab_bin;
run;
 
proc reg data=train;
model alldiab_bin= &predicts / vif; run;

Proc logistic data=train outest=betas outmodel=scoringdata desc plots(maxpoints=none);
model alldiab_bin=&predicts_all  /selection=stepwise slentry=0.05 slstay=0.05
CTABLE pprob=(0 to 1 by 0.05)
LACKFIT RISKLIMITS;
output out=output p=predicted;
score data=valid out=scored_values;
run;

/*score validation*/
proc logistic inmodel=scoringdata;
score data=valid out=score;
run;

/* Classifcation table */
Proc freq data=score ;
tables 	I_alldiab_bin*F_alldiab_bin/nocol norow;
run;

/* K-S Statistic */
ods graphics on / outputfmt=png;
	proc npar1way data=output wilcoxon edf;
	class alldiab_bin;
	var predicted;
	run;
