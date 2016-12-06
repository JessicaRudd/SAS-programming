/*
/ Program   : KID_NEURO_SmallSubset
/ Version   : 1.0 
/ Author    : Jessica M. Rudd 
/ Date      : 23 March 2016
/ Contact   : icj2@cdc.gov
/ Purpose   : Create subset data from all years of KID data for cases of acute flaccid myelitis 
				based on subset of ICD-9 codes (n=12) proposed for surveillance
				and perform weighted analysis of data to produce national estimates.
/ Notes     : 
/ Usage     : 
/ 
/=============================================================================== 
/ PARAMETERS: 
/-------name------- -------------------------description------------------------ 
  KID										Study Year
/=============================================================================== 
/ AMENDMENT HISTORY: 
/ init --date-- mod-id ----------------------description------------------------
 
/=============================================================================*/
LIBNAME KID '\\cdc\project\NCIRD_DVD_EB_DATA_1\hcup\kid\data';
LIBNAME CC '\\cdc\project\NCIRD_DVD_EB_DATA_1\hcup\kid\data\cost_to_charge_ratio_files';
LIBNAME FORMATS '\\cdc.gov\private\L317\icj2\NCHS\HCUP\KID\Data\Formats';
LIBNAME RUDD '\\cdc.gov\private\L317\icj2\NCHS\HCUP\KID\Data';

proc format;
VALUE AGEGRP 1='LT 1' 2='1-4' 3='5-9' 4='10-20';
VALUE SEX 1='MALE' 2='FEMALE';
VALUE RACE 1='WHITE' 2='BLACK' 3='HISPANIC' 4='ASIAN/PI' 5='AI/AN' 6='OTHER';
VALUE REGION 1='NORTHEAST' 2='MIDWEST' 3='SOUTH' 4='WEST';
VALUE PAY 1='MEDICARE' 2='MEDICAID' 3='PRIVATE INSURANCE' 4='SELF PAY' 5='NO CHARGE' 6='OTHER' ;
VALUE ZIP 1='0-25th' 2='26th-50th' 3='51st-75th' 4='76th-100th';
run;

%LET DENOM=100000; *** Denominator for rates is 100,000 person visits***;

options obs=max bufno=750 bufsize=24k;

%Macro loop;
 %LOCAL I;
 %DO I = 2003 %TO 2009 %by 3; /* Update here when new datasets become available*/

options fmtsearch=(formats.formats);
DATA KID_&I._CORE ;
	SET KID.KID_&I._CORE  (read=kidread);
RUN;

PROC SORT DATA=KID_&I._CORE;
BY HOSPID;
RUN;

DATA KID_&I._HOSPITAL (keep=hospid region);
	SET KID.KID_&I._HOSPITAL (read=kidread rename=(hosp_region=region));
RUN;

PROC SORT DATA=KID_&I._HOSPITAL;
BY HOSPID;
RUN;

DATA KID_&I._SEVERITY;
	SET KID.KID_&I._SEVERITY;
RUN;

PROC SORT DATA=KID_&I._SEVERITY;
BY HOSPID;
RUN;

DATA KID_&I._CORE(compress=binary);
MERGE KID_&I._CORE KID_&I._HOSPITAL KID_&I._SEVERITY;
BY HOSPID;
length kid_stratum 4; 
run;

%END;
%MEND LOOP;
%LOOP;
QUIT;

/* Read in 2012 data*/

options fmtsearch=(formats.formats);
DATA KID_2012_CORE  ;
	SET KID.KID_2012_CORE (read=kidread rename=(hosp_kid=hospid));
RUN;

PROC SORT DATA=KID_2012_CORE;
BY HOSPID;
RUN;

DATA KID_2012_HOSPITAL (keep=hospid region);
	SET KID.KID_2012_HOSPITAL (read=kidread rename=(hosp_region=region hosp_kid=hospid));
RUN;

PROC SORT DATA=KID_2012_HOSPITAL;
BY HOSPid;
RUN;

DATA KID_2012_SEVERITY;
	SET KID.KID_2012_SEVERITY (read=kidread rename=(hosp_kid=hospid));
RUN;

PROC SORT DATA=KID_2012_SEVERITY;
BY HOSPID;
RUN;

DATA KID_2012_CORE (compress=binary);
MERGE KID_2012_CORE KID_2012_HOSPITAL KID_2012_SEVERITY;
BY HOSPid;
run;


options obs=max bufno=750 bufsize=24k;
%macro icd_unspec;                         
**** any-listed AFM (with unspecified cause of myelitis)****;
array icdcodes (15) $ dx1-dx15;
do i=1 to 15;                                                               
 	if substr(icdcodes(i),1,5) in('04590','32352','32382','32342','33529','34489') then do;
  	afm_a=1;
  	if afm_a=1 and i=1 then afm_f=1;
end;
	if substr(icdcodes(i),1,4) in('3239','3358','3359','3441','3442','3449') then do;
  	afm_a=1;
  	if afm_a=1 and i=1 then afm_f=1;
end;
	
end;
%mend icd_unspec;

%macro pr_unspec;   
array prcodes (15) $ pr1-pr15;
	do i=1 to 15;
		if substr (prcodes(i),1,4) in ('8891','8893') then do;
		MRI_a=1;
		if MRI_a=1 and i=1 then MRI_f=1;
	end;
		if substr (prcodes(i),1,4) in ('0331') then do;
		LP_a=1;
		if LP_a=1 and i=1 then LP_f=1;
	end;
		if substr (prcodes(i),1,4) in ('9308') then do;
		EMG_a=1;
		if EMG_a=1 and i=1 then EMG_f=1;
	end;
end;
%mend pr_unspec;


%macro loop;
%local i;
%do i=2003 %to 2012 %by 3;
data kid_afm_unspec_&i (compress=binary);
set kid_&i._core ; 
%icd_unspec;
if age gt 0;
/*if (afm_a=1) and age gt 0;*/
drop i;
run;

%end;
%mend loop;
%loop;


data unspec_afm_kid (compress=binary);
set kid_afm_unspec_2003 kid_afm_unspec_2006 kid_afm_unspec_2009 kid_afm_unspec_2012;
%pr_unspec;
if afm_a=. then afm_a=0;
if afm_f=. then afm_f=0;
if MRI_a=. then MRI_a=0;
if MRI_f=. then MRI_f=0;
if LP_a=. then LP_a=0;
if LP_f=. then LP_f=0;
if EMG_a=. then EMG_a=0;
if EMG_f=. then EMG_f=0;
IF AGE in (1,2,3,4) THEN AGEGRP=2;  *1-5;
IF AGE GT 4 AND AGE LE 9 THEN AGEGRP=3;
IF AGE GE 10 AND AGE LE 20 THEN AGEGRP=4;
/*REDEFINES SEX TO HAVE MALE=1, FEMALE=2*/
IF FEMALE=. THEN SEX=(int(2*ranuni(35346)) + 1);
IF FEMALE=0 THEN SEX=1;
ELSE IF FEMALE=1 THEN SEX=2;
IF YEAR=2003 THEN COMPYR=1;
ELSE IF YEAR=2006 THEN COMPYR=2;
ELSE IF YEAR=2009 THEN COMPYR=3;
ELSE IF YEAR=2012 THEN COMPYR=4;
FORMAT YEAR 5. AGEGRP AGEGRP. SEX SEX. REGION REGION. RACE RACE. pay1 pay. zipinc_qrtl zip.;
run;

proc freq data=unspec_afm_kid;
tables year agegrp sex region race pay1 zipinc_qrtl female CM_NEURO CM_PARA;
run;

Data rudd.afm_kid_small (compress=binary);
set unspec_afm_kid;
run;

ods rtf file="\\cdc.gov\private\L317\icj2\NCHS\HCUP\KID\Documentation\AFM_SMALL_by_agegrp_&sysdate..rtf" bodytitle sasdate style=rtf;
title 'table of AFM (with 12 codes) by year and agegrp';
proc freq data=unspec_afm_kid;
table year*agegrp;
weight discwt;
where afm_a=1;
run;

title 'AFM as a primary diagnosis';
proc freq data=unspec_afm_kid;
table afm_f;
weight discwt;
where afm_a=1;
run;

title 'MRI as a procedure for any listed AFM';
proc freq data=unspec_afm_kid;
table MRI_a;
weight discwt;
where afm_a=1;
run;

title 'LP as a procedure for any listed AFM';
proc freq data=unspec_afm_kid;
table LP_a;
weight discwt;
where afm_a=1;
run;

title 'EMG as a procedure for any listed AFM';
proc freq data=unspec_afm_kid;
table EMG_a;
weight discwt;
where afm_a=1;
run;

title 'AFM with MRI';
proc freq data=unspec_afm_kid;
table MRI_a*(AFM_f AFM_a);
weight discwt;
run;

title 'AFM with LP';
proc freq data=unspec_afm_kid;
table LP_f*(AFM_f AFM_a);
weight discwt;
run;

title 'AFM with EMG';
proc freq data=unspec_afm_kid;
table EMG_f*(AFM_f AFM_a);
weight discwt;
run;

title 'AFM as a primary diagnosis with MRI, LP, or EMG';
proc freq data=unspec_afm_kid;
table afm_f;
weight discwt;
where MRI_a=1 or LP_a=1 or EMG_a=1;
run;

ods rtf close;

ods rtf file="\\cdc.gov\private\L317\icj2\NCHS\HCUP\KID\Documentation\AFM_SMALL_dx1_freqs_JMR_&sysdate..rtf" bodytitle sasdate style=rtf;

title '1st Listed DX for 1st Listed AFM Hospitalizations Ages 1-20 yrs - WEIGHTED';
PROC FREQ data=unspec_afm_kid;
table dx1 * year;
weight discwt;
where afm_f=1;
run;

title '1st Listed DX for 1st listed AFM Hospitalizations Ages 1-20 yrs - UNWEIGHTED';
PROC FREQ data=unspec_afm_kid;
table dx1 * year;
where afm_f=1;
run;

ods rtf close;

***************************************************************
****** READS IN THE US CENSUS DATA FOR RATE CALCULATIONS ******
***************************************************************;

LIBNAME CEN '\\cdc\project\NCIRD_DVD_EB_DATA_1\census\bridged_race\data';

DATA CEN;                                                                       
  SET CEN.br90_12_B (KEEP=YEAR SEX AGE AGE5A POP STATE RACE);                       
IF YEAR IN (2003,2006,2009,2012);                                                      
DIED=2; 
IF AGE5A=0 or age = 0 THEN AGEGRP=1; *LT 1;
ELSE IF AGE5A=1 or age in (1,2,3,4)THEN AGEGRP=2;  *1-4;
ELSE IF AGE5A=2 OR AGE IN (5,6,7,8,9)THEN AGEGRP=3; *5-9;
ELSE IF AGE5A IN (3,4) OR AGE IN (10,11,12,13,14,15,16,17,18,19,20)THEN AGEGRP=4;
ELSE AGEGRP=.;

****ENTER NEW AGE DEFINITION ABOVE THIS LINE****;  ***STEP_15***;               
                                                      
                                          
LABEL POP='AGE GROUP CENSUS';                                                  
  IF STATE IN(23,33,50,25,44,09,36,34,42) THEN REGION=1;                        
  IF STATE IN (39,18,17,26,55,27,19,29,38,46,31,20) THEN REGION=2;              
  IF STATE IN(10,24,11,51,54,37,45,13,12,21,47,01,28,05,22,40,48)               
    THEN REGION=3;                                                              
  IF STATE IN(30,16,56,08,35,04,49,32,53,41,06,02,15) THEN REGION=4;            
IF AGEGRP=. OR AGEGRP=1 OR AGEGRP GT 4 THEN DELETE;
RENAME POP=POP2; 
FORMAT AGEGRP AGEGRP. SEX SEX. RACE RACE. REGION REGION.; 
RUN;                                                                            
     

PROC SUMMARY NWAY MISSING;                                                      
  CLASS YEAR SEX AGE AGE5A AGEGRP REGION RACE;                                
  VAR POP2;                                                                     
OUTPUT OUT=CENSUS SUM=;                                                         
RUN;  

Data Rudd.census;
set cen;
run;

*** Create AMF variable with MRI, LP, EMG procedures ***;
Data afm_small_proc (compress=binary);
set unspec_afm_kid;
*** Any-listed AMF ***;
if afm_a=1 and (mri_a=1 or lp_a=1 or emg_a=1) then afm_proc=1;
else afm_proc=0;
if afm_a=1 and mri_a=1 then afm_mri=1;
else afm_mri=0;
if afm_a=1 and LP_a=1 then afm_lp=1;
else afm_lp=0;
if afm_a=1 and EMG_a=1 then afm_emg=1;
else afm_emg=0;
*** First-listed AFM ***;
if afm_f=1 and (mri_a=1 or lp_a=1 or emg_a=1) then afm_f_pr=1;
else afm_f_pr=0;
if afm_f=1 and mri_a=1 then afm_f_m=1;
else afm_f_m=0;
if afm_f=1 and LP_a=1 then afm_f_l=1;
else afm_f_l=0;
if afm_f=1 and EMG_a=1 then afm_f_e=1;
else afm_f_e=0;
run;

proc freq data=afm_small_proc;
tables afm_f_pr*(cm_neuro cm_para);
weight discwt;
run;

proc freq data=afm_small_proc;
tables afm_proc afm_mri afm_lp afm_emg afm_f_pr afm_f_m afm_f_l afm_f_e;
weight discwt;
run;

Data Rudd.afm_small_proc;
set afm_small_proc;
run;

*************************************************************************
***** THIS SECTION CREATES OUTPUT DATASETS THAT HAVE THE ESTIMATES ******
***** AND SES BY DEMOGRAPHIC CHARACTERISTICS AND TIME PERIOD       ******
***** TO BE USED FOR RATE CALCULATIONS                             ******
*************************************************************************;
*SEX, AGEGRP, YEAR, REGION;

Data afm_small_proc;
set rudd.afm_small_proc;
run;

PROC SORT DATA=afm_small_proc;
BY KID_STRATUM HOSPID;
RUN;

%macro pr_spinal;   
array prcodes (15) $ pr1-pr15;
	do i=1 to 15;
		if substr (prcodes(i),1,4) in ('8893') then do;
		spinal=1;
		if spinal=1 and i=1 then spinal_f=1;
	end;
end;
%mend pr_spinal;

Data afm_spinal_mri;
set afm_small_proc;
%pr_spinal;
if afm_f=1 and spinal=1 then afm_spinal=1; else afm_spinal=0;
run;

ods rtf file="\\cdc.gov\private\L317\icj2\NCHS\HCUP\KID\Documentation\AFM_spinalMRI_&sysdate..rtf" bodytitle sasdate style=rtf;
proc freq data=afm_spinal_mri;
table year*agegrp*afm_spinal;
title "Unweighted Hospitalizations of 1st Coded AFM w/ Spinal MRI";
run;

proc freq data=afm_spinal_mri;
table year*agegrp*afm_spinal;
weight discwt;
title "Weighted Hospitalizations of 1st Coded AFM w/ Spinal MRI";
run;
ods rtf close;

proc freq data=afm_small_proc;
table year*(agegrp race region sex zipinc_qrtl died female amonth);
run;

proc print data=afm_small_proc;
where amonth=.;
run;


proc freq data=afm_small_proc;
table year*(age agegrp);
run;

proc freq data=afm_small_proc;
tables agegrp race region sex zipinc_qrtl died;
weight discwt;
where afm_f_pr=1;
run;

proc univariate data=afm_small_proc;
var npr los totchg age;
weight discwt;
where afm_f_pr=1;
run;

proc surveymeans data=afm_small_proc;
class agegrp race region sex zipinc_qrtl died;
cluster hospid;
strata kid_stratum;
var agegrp race region sex zipinc_qrtl died npr los totchg age;
weight discwt;
run;

***Create new survey weights in order to run domain analysis with medians and percentiles ***;
***Justification for this method found:    Graubard BI, Korn EL.  Survey inference for subpopulations.  American Journal of Epidemiology 1996;
Data afm_pr;
set afm_small_proc;
if afm_f_pr=1 /*then discwt_domain=discwt*/;
/*else discwt_domain=0.0000000000001;*/
run;

Data nonAfm (keep=kid_stratum hospid weight);
set afm_small_proc;
if afm_f_pr=0;
weight=0.000000001;
run;

proc sort data=nonAFM nodupkey;
by kid_stratum hospid;
run;

Data afm_pr_domain;
set afm_pr nonAFM;
run;


proc freq data=afm_pr_domain;
tables npr*afm_f_pr;
run;

ods rtf file="\\cdc.gov\private\L317\icj2\NCHS\HCUP\KID\Documentation\AFM_proc_demographics_&sysdate..rtf" bodytitle sasdate style=rtf;
title 'Weighted median length of stay (LOS) for AFM with MRI/LP/EMG hospitalizations';
proc surveymeans data=afm_pr_domain quartiles;
cluster hospid;
strata kid_stratum;
var npr los totchg age;
weight discwt;
run;

title 'Weighted frequencies of AFM hospitalizations';
proc surveyfreq data=afm_pr_domain;
tables agegrp race region sex zipinc_qrtl died amonth cm_para cm_neuro year*amonth;
cluster hospid;
strata kid_stratum;
weight discwt;
run;
ods rtf close;

ods rtf file="\\cdc.gov\private\L317\icj2\NCHS\HCUP\KID\Documentation\AFM_SMALL_dx1_freqs_JMR_&sysdate..rtf" bodytitle sasdate style=rtf;

title '1st Listed DX for 1st Listed AFM Hospitalizations Ages 1-20 yrs - WEIGHTED';
PROC FREQ data=afm_small_proc;
table dx1 * agegrp;
weight discwt;
where afm_f_pr=1;
format DX1 $I9DXF.;
run;

title '1st Listed DX for 1st listed AFM Hospitalizations Ages 1-20 yrs - UNWEIGHTED';
PROC FREQ data=afm_small_proc;
table dx1 * agegrp;
where afm_f_pr=1;
format DX1 $I9DXF.;
run;

ods rtf close;

*** AFM RATES FOR 1ST LISTED AFM ***;
*************************************************************
***Determine rate of primary-coded AFP with MRI or LP or EMG***;
options mprint;
DATA CAT;                                                                       
INPUT CAT $ @@;                                                    
CARDS;                                                                          
AFM_f_pr
;
RUN;

DATA _NULL_;
SET CAT END=LAST;
CALL SYMPUT('CAT'||LEFT(PUT(_N_,3.)),CAT);
IF LAST THEN CALL SYMPUT('COUNT',_N_);
RUN; 

%MACRO LOOP;
%LOCAL I;
%DO I=1 %TO &COUNT;
%let ti=%cmpres(&&cat&i);
ODS PDF FILE="\\cdc.gov\private\L317\icj2\NCHS\HCUP\KID\Documentation\&ti._small_rates_&sysdate..PDF";

TITLE "Primary Coded AFM Inpatient Visits, with MRI or LP or EMG, CATEGORY=&TI, US, 2003-2012, KID";
PROC CROSSTAB DATA=afm_small_proc FILETYPE=SAS DESIGN=WR;
  WEIGHT DISCWT;
  SUBPOPN &TI=1;
  NEST kid_stratum HOSPID/ MISSUNIT;
  CLASS YEAR AGEGRP REGION SEX RACE AMONTH/INCLUDE=MISSING;
  TABLES YEAR *(AGEGRP REGION SEX RACE AMONTH);
  SETENV LEFTMGN=1;*COLWIDTH=10 DECWIDTH=0;   
  RTITLE "Primary Coded AFM Inpatient Visits, with MRI or LP or EMG, CATEGORY=&TI, US [KID], 2003-2012";
  PRINT NSUM WSUM COLPER ROWPER SEWGT SECOL SETOT;
  OUTPUT NSUM="UNWEIGHTED CASES" WSUM="WEIGHTED CASES" SEWGT="STANDARD ERROR"/FILENAME="WORK.VAR97"
  FILETYPE=SAS REPLACE;
  FORMAT AGEGRP AGEGRP. SEX SEX. REGION REGION. RACE RACE.; 
run;
quit;
************************
****** TOTAL RATE  *****
************************;

DATA TOTHOSPS;
SET VAR97;
IF YEAR=0 AND SEX=0 /*AND AGEGRP=-2 AND REGION=-2 AND RACE=-2 AND AMONTH=0*/;
KEEP WSUM SEWGT;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
%MACRO RATECI;
  RATE=(WSUM/POP2)*&DENOM; 
   raterse1=(RATE*(SEWGT/WSUM));                                            
lci1=(RATE-(1.96*raterse1));        
uci1=(RATE+(1.96*raterse1));                                                 
LABEL RATE="HOSPS/&DENOM" WSUM ='HOSPS' LCI1='LOWER 95% CI' UCI1='UPPER 95% CI';
FORMAT RATE LCI1 UCI1 10.1 WSUM SEWGT COMMA10. POP2 COMMA12.;
%MEND RATECI;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
TITLE2 "TOTAL RATE";
RUN;

**********************
****** BY YEAR   *****
**********************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR>0 AND region=-2 AND AGEGRP=-2 AND SEX=0 /*AND RACE=-2 AND AMONTH=0*/;
KEEP YEAR WSUM SEWGT;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT YEAR, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY YEAR;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY YEAR;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
TITLE2 "RATE BY YEAR";
RUN;

**********************
****** BY AGE GROUP   *****
**********************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR=0 /*AND region=-2 */AND AGEGRP >0 /*AND SEX=0 AND RACE=-2*/;
KEEP AGEGRP WSUM SEWGT;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT AGEGRP, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY AGEGRP;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY AGEGRP;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
TITLE2 "RATE BY AGE GROUP";
RUN;


**********************
****** BY SEX   *****
**********************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR=0 AND SEX >0 ;
KEEP SEX WSUM SEWGT;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT SEX, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY SEX;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY SEX;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
TITLE2 "RATE BY SEX";
RUN;

**********************
****** BY REGION   *****
**********************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR=0 AND region >0 ;
KEEP REGION WSUM SEWGT;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT REGION, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY REGION;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY REGION;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
TITLE2 "RATE BY REGION";
RUN;

**********************
****** BY RACE   *****
**********************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR=0 AND RACE >0;
KEEP RACE WSUM SEWGT;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT RACE, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY RACE;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY RACE;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
TITLE2 "RATE BY RACE";
RUN;

**********************
****** BY YEAR AND AGE GROUP   *****
**********************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR>0 AND AGEGRP > 0 ;
KEEP YEAR AGEGRP WSUM SEWGT;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT YEAR, AGEGRP, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY YEAR, AGEGRP;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY YEAR AGEGRP;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
TITLE2 "RATE BY YEAR AND AGE GROUP";
RUN;

**********************
****** BY YEAR AND SEX   *****
**********************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR>0 AND SEX > 0 ;
KEEP YEAR SEX WSUM SEWGT;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT YEAR, SEX, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY YEAR, SEX;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY YEAR SEX;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
TITLE2 "RATE BY YEAR AND SEX";
RUN;

**********************
****** BY YEAR AND RACE   *****
**********************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR>0 AND RACE > 0;
KEEP YEAR RACE WSUM SEWGT;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT YEAR, RACE, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY YEAR, RACE;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY YEAR RACE;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
TITLE2 "RATE BY YEAR AND RACE";
RUN;

**********************
****** BY YEAR AND REGION   *****
**********************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR>0 AND region > 0 ;
KEEP YEAR REGION WSUM SEWGT;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT YEAR, REGION, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY YEAR, REGION;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY YEAR REGION;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
TITLE2 "RATE BY YEAR AND REGION";
RUN;

**********************
****** BY MONTH   *****
**********************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR=0 AND AMONTH >0;
KEEP AMONTH WSUM SEWGT;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT SUM(POP2)/12 AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS;

DATA ALL;
MERGE TOTCEN TOTHOSPS ;
IF POP2=. THEN POP2=27575294;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
TITLE2 "RATE BY MONTH";
RUN;

**********************
****** BY YEAR AND MONTH   *****
**********************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR>0 AND AMONTH > 0 ;
KEEP YEAR amonth WSUM SEWGT;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT YEAR, SUM(POP2)/12 AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY YEAR;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY YEAR ;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
TITLE2 "RATE BY YEAR AND MONTH";
RUN;
ODS PDF CLOSE;
%END;
%MEND LOOP;
%LOOP;
 

***Determine top AFM diagnoses associated with 1st listed AFM discharges - AC's way;
data all_dxs;
set afm_small_proc (keep=year recnum age agegrp dx1-dx15 discwt afm_f afm_f_pr);
array aa (15) $ dx1-dx15;
do i=1 to 15;
if aa(i) in ('04590','32352','32382','32342','33529','34489','3239','3358','3359','3441','3442','3449')
and afm_f_pr=1 then do;
  dx=aa(i);
  output;
end;
end;
FORMAT DX1-DX15 $I9DXF.;
run;
proc sql;
create table all_dxs_sum as
select /*year,*/agegrp,dx, sum(discwt) as dx_n
from all_dxs
group by /*year,*/agegrp, dx;
quit;
proc freq data=all_dxs_sum noprint;
table agegrp*dx/**year*//out=dxs_allages;
weight dx_n;
*where agegrp=2;
run;
proc sort data=dxs_allages;
by agegrp /*year*/ count;
run;
*********************************************************************
**** total hosps by age grp for calculating percentages by total ****
**** hosps by age group and year                                 ****
*********************************************************************;
/*%macro loop;
%do i=2003 %to 2012 %by 3;*/
proc summary data=afm_small_proc nway;
class agegrp;
var afm_f_pr;
weight discwt;
output out=all_afm/*_&i*/ (drop=_type_ _freq_) sum=denom/*_&i*/;
/*where year=&i;*/
run;
/*%end;
%mend loop;
%loop;
*/

%macro loop;
/*%do i=2003 %to 2012 %by 3;*/
  %do j=2 %to 4;
proc sort data=dxs_allages out=dxs_&j;
by /*year*/ descending count;
where /*year=&i and*/ agegrp=&j;
run;
data dxs_&j;
length rank 3;
set dxs_&j;
merge dxs_&j all_afm (where=(agegrp=&j));
by agegrp;
if _n_<=15;
rank=_n_;
percent=round((count/denom*100),.1);
/*percent=round(percent),.1;*/
format percent 4.1 DX $I9DXF.;;
/*rename dx=dx_&i count=count_&i percent=percent_&i;*/
format count comma9.;
/*drop year percent;*/
run;
/*%end;*/
%end;
%mend loop;
%loop;

/*data all_dx_2;
merge dxs_2003_2 dxs_2006_2 dxs_2009_2 dxs_2012_2;
run;
data all_dx_3;
merge dxs_2003_3 dxs_2006_3 dxs_2009_3 dxs_2012_3;
run;*/

data all_dx_by_age;
set dxs_2 dxs_3 dxs_4;
run;
options orientation=landscape;
ods rtf file="\\cdc.gov\private\L317\icj2\NCHS\HCUP\KID\Documentation\AFM_small_diagnoses_&sysdate..rtf" style=rtf
bodytitle sasdate startpage=no;
proc print data=all_dx_by_age noobs;
var /*rank*/ dx count percent  /*dx_2006 count_2006 percent_2006 
    blank dx_2009 count_2009 percent_2009 blank dx_2012 count_2012 percent_2012*/;
by agegrp;
title "Leading AFM discharge diagnoses by age group for 1st listed AFM (with EMG/LP/MRI)";
run;
ods rtf close;

***Determine top non-AFM diagnosis associated with 1st listed AFM discharges diagnoses - AC's way;
data all_dxs;
set afm_small_proc (keep=year recnum age agegrp dx1-dx15 discwt afm_f afm_f_pr);
array aa (15) $ dx1-dx15;
do i=1 to 15;
if aa(i)  ^in (' ','04590','32352','32382','32342','33529','34489','3239','3358','3359','3441','3442','3449')
and afm_f_pr=1 then do;
  dx=aa(i);
  output;
end;
end;
FORMAT DX1-DX15 $I9DXF.;
run;
proc sql;
create table all_dxs_sum as
select /*year,*/agegrp,dx, sum(discwt) as dx_n
from all_dxs
group by /*year,*/agegrp, dx;
quit;
proc freq data=all_dxs_sum noprint;
table agegrp*dx/**year*//out=dxs_allages;
weight dx_n;
*where agegrp=2;
run;
proc sort data=dxs_allages;
by agegrp /*year*/ count;
run;
*********************************************************************
**** total hosps by age grp for calculating percentages by total ****
**** hosps by age group and year                                 ****
*********************************************************************;
/*%macro loop;
%do i=2003 %to 2012 %by 3;*/
proc summary data=afm_small_proc nway;
class agegrp;
var afm_f_pr;
weight discwt;
output out=all_afm/*_&i*/ (drop=_type_ _freq_) sum=denom/*_&i*/;
/*where year=&i;*/
run;
/*%end;
%mend loop;
%loop;
*/

%macro loop;
/*%do i=2003 %to 2012 %by 3;*/
  %do j=2 %to 4;
proc sort data=dxs_allages out=dxs_&j;
by /*year*/ descending count;
where /*year=&i and*/ agegrp=&j;
run;
data dxs_&j;
length rank 3;
set dxs_&j;
merge dxs_&j all_afm (where=(agegrp=&j));
by agegrp;
if _n_<=15;
rank=_n_;
percent=round((count/denom*100),.1);
/*percent=round(percent),.1;*/
format percent 4.1 DX $I9DXF.;;
/*rename dx=dx_&i count=count_&i percent=percent_&i;*/
format count comma9.;
/*drop year percent;*/
run;
/*%end;*/
%end;
%mend loop;
%loop;

/*data all_dx_2;
merge dxs_2003_2 dxs_2006_2 dxs_2009_2 dxs_2012_2;
run;
data all_dx_3;
merge dxs_2003_3 dxs_2006_3 dxs_2009_3 dxs_2012_3;
run;*/

data all_dx_by_age;
set dxs_2 dxs_3 dxs_4;
run;
options orientation=landscape;
ods rtf file="\\cdc.gov\private\L317\icj2\NCHS\HCUP\KID\Documentation\nonAFM_small_diagnoses_&sysdate..rtf" style=rtf
bodytitle sasdate startpage=no;
proc print data=all_dx_by_age noobs;
var /*rank*/ dx count percent  /*dx_2006 count_2006 percent_2006 
    blank dx_2009 count_2009 percent_2009 blank dx_2012 count_2012 percent_2012*/;
by agegrp;
title "Leading non-AFM discharge diagnoses by age group associated with 1st listed AFM (with EMG/LP/MRI)";
run;
ods rtf close;
***Determine top 5 AFP diagnoses for first-listed AFM hospitalizations - AC's way;
/*data all_dxs;
set afm_small_proc (keep=year recnum age agegrp dx1-dx15 discwt afm_a afm_f afm_f_pr);
array aa (15) $ dx1-dx15;
do i=1 to 15;
if afm_f_pr=1 and aa(i) in ('04590','32352','32382','32342','33529','34489','3239','3358','3359','3441','3442','3449') then do;
  dx=aa(i);
  output;
end;
end;
FORMAT DX1-DX15 $I9DXF.;
run;
proc sql;
create table all_dxs_sum as
select year,agegrp,dx, sum(discwt) as dx_n
from all_dxs
group by year,agegrp, dx;
quit;
proc freq data=all_dxs_sum noprint;
table agegrp*dx*year/out=dxs_allages;
weight dx_n;
*where agegrp=2;
run;
proc sort data=dxs_allages;
by agegrp year count;
run;
*********************************************************************
**** total hosps by age grp for calculating percentages by total ****
**** hosps by age group and year                                 ****
*********************************************************************;
%macro loop;
%do i=2003 %to 2012 %by 3;
proc summary data=afm_small_proc nway;
class agegrp;
var afm_f_pr;
weight discwt;
output out=first_afm_&i (drop=_type_ _freq_) sum=denom_&i;
where year=&i;
run;
%end;
%mend loop;
%loop;


%macro loop;
%do i=2003 %to 2012 %by 3;
  %do j=2 %to 3;
proc sort data=dxs_allages out=dxs_&i._&j;
by year descending count;
where year=&i and agegrp=&j;
run;
data dxs_&i._&j;
length rank 3;
*set dxs_&i._&j;
merge dxs_&i._&j first_afm_&i (where=(agegrp=&j));
by agegrp;
if _n_<=15;
rank=_n_;
percent_&i=round((count/denom_&i*100),.1);
format percent_&i 4.1 DX $I9DXF.;;
rename dx=dx_&i count=count_&i /*percent=percent_&i;
format count comma9.;
drop year percent;
run;
%end;
%end;
%mend loop;
%loop;

data first_dx_2;
merge dxs_2003_2 dxs_2006_2 dxs_2009_2 dxs_2012_2;
run;
data first_dx_3;
merge dxs_2003_3 dxs_2006_3 dxs_2009_3 dxs_2012_3;
run;

data first_dx_by_age;
set first_dx_2 first_dx_3;
**** used to get a column to separate each year in the table ****;
blank=' ';
run;

proc print data=first_dx_by_age noobs;
var rank dx_2003 count_2003 percent_2003 blank dx_2006 count_2006 percent_2006 
    blank dx_2009 count_2009 percent_2009 blank dx_2012 count_2012 percent_2012;
by agegrp;
title "Leading AFM discharge diagnoses by age group for first-listed AFP hospitalizations with MRI/LP/EMG procedures";
run;*/
ods rtf close;
options orientation=portrait;


*** Determine top 5 accompanying procedures ***;
ODS PDF FILE="\\cdc.gov\private\L317\icj2\Studies\AFM\AFM_procedures_&sysdate..pdf";
%Macro procedure;
 %LOCAL I;
 %DO I=1 %TO 15;
 options fmtsearch=(formats.formats);
 DATA PR&I (KEEP=YEAR AGEGRP HOSPID afm_a afm_f afm_f_pr PR);
 	SET afm_small_proc;
	IF PR&I NE ' ' THEN PR=PR&I;
 RUN;

 %END;
%MEND PROCEDURE;
%PROCEDURE;
QUIT;
options fmtsearch=(formats.formats);
DATA PR;
SET PR1 PR2 PR3 PR4 PR5 PR6 PR7 PR8 PR9 PR10 PR11 PR12 PR13 PR14 PR15;
RUN;

***Top 20 procedures for primary coded AFP***;
PROC FREQ DATA=PR;
TABLES PR/noprint out=byvalue;
where AFM_f_pr=1 and PR ne ' ';
weight discwt;
RUN;
proc sort data=byvalue out=bycount;
by descending count;
run;
title'Top 5 AFM procedures for primary coded AFM';
options fmtsearch=(formats.formats);

proc print data=bycount (obs=5);
FORMAT PR $I9PRF.;
run;

ODS PDF CLOSE;

***Determine top procedures associated with 1st listed AFM discharges diagnoses ;
data all_prs;
set afm_small_proc (keep=year recnum age agegrp pr1-pr15 discwt afm_f afm_f_pr);
array aa (15) $ pr1-pr15;
do i=1 to 15;
if aa(i) ne ' ' and afm_f_pr=1 then do;
  pr=aa(i);
  output;
end;
end;
FORMAT PR1-PR15 $I9PRF.;
run;
proc sql;
create table all_prs_sum as
select /*year,*/agegrp,pr, sum(discwt) as pr_n
from all_prs
group by /*year,*/agegrp, pr;
quit;
proc freq data=all_prs_sum noprint;
table agegrp*pr/**year*//out=prs_allages;
weight pr_n;
*where agegrp=2;
run;
proc sort data=prs_allages;
by agegrp /*year*/ count;
run;
*********************************************************************
**** total hosps by age grp for calculating percentages by total ****
**** hosps by age group and year                                 ****
*********************************************************************;
/*%macro loop;
%do i=2003 %to 2012 %by 3;*/
proc summary data=afm_small_proc nway;
class agegrp;
var afm_f_pr;
weight discwt;
output out=all_afm/*_&i*/ (drop=_type_ _freq_) sum=denom/*_&i*/;
/*where year=&i;*/
run;
/*%end;
%mend loop;
%loop;
*/

%macro loop;
/*%do i=2003 %to 2012 %by 3;*/
  %do j=2 %to 4;
proc sort data=prs_allages out=prs_&j;
by /*year*/ descending count;
where /*year=&i and*/ agegrp=&j;
run;
data prs_&j;
length rank 3;
set prs_&j;
merge prs_&j all_afm (where=(agegrp=&j));
by agegrp;
if _n_<=15;
rank=_n_;
percent=round((count/denom*100),.1);
/*percent=round(percent),.1;*/
format percent 4.1 PR $I9PRF.;;
/*rename dx=dx_&i count=count_&i percent=percent_&i;*/
format count comma9.;
/*drop year percent;*/
run;
/*%end;*/
%end;
%mend loop;
%loop;

/*data all_dx_2;
merge dxs_2003_2 dxs_2006_2 dxs_2009_2 dxs_2012_2;
run;
data all_dx_3;
merge dxs_2003_3 dxs_2006_3 dxs_2009_3 dxs_2012_3;
run;*/

data all_pr_by_age;
set prs_2 prs_3 prs_4;
run;
options orientation=landscape;
ods rtf file="\\cdc.gov\private\L317\icj2\NCHS\HCUP\KID\Documentation\AFM_procedures_&sysdate..rtf" style=rtf
bodytitle sasdate startpage=no;
proc print data=all_pr_by_age noobs;
var /*rank*/ pr count percent  /*dx_2006 count_2006 percent_2006 
    blank dx_2009 count_2009 percent_2009 blank dx_2012 count_2012 percent_2012*/;
by agegrp;
title "Leading procedures by age group associated with 1st listed AFM (with EMG/LP/MRI)";
run;
ods rtf close;


***Determine top diagnosis associated with 1st listed AFM discharges diagnoses - AC's way;
data all_dxs;
set afm_small_proc (keep=year recnum age agegrp dx1-dx15 discwt afm_f afm_f_pr);
array aa (15) $ dx1-dx15;
do i=1 to 15;
if aa(i) ^=' ' and i ^=1/*  ^in (' ','04590','32352','32382','32342','33529','34489','3239','3358','3359','3441','3442','3449')
and */ and afm_f_pr=1 then do;
  dx=aa(i);
  output;
end;
end;
FORMAT DX1-DX15 $I9DXF.;
run;
proc sql;
create table all_dxs_sum as
select /*year,agegrp,*/dx, sum(discwt) as dx_n
from all_dxs
group by /*year,agegrp,*/ dx;
quit;
proc freq data=all_dxs_sum noprint;
table /*agegrp*/dx/**year*//out=dxs_allages;
weight dx_n;
*where agegrp=2;
run;
proc sort data=dxs_allages;
by /*agegrp year*/ count;
run;
*********************************************************************
**** total hosps by age grp for calculating percentages by total ****
**** hosps by age group and year                                 ****
*********************************************************************;
/*%macro loop;
%do i=2003 %to 2012 %by 3;*/
proc summary data=afm_small_proc nway;
/*class agegrp;*/
var afm_f_pr;
weight discwt;
output out=all_afm/*_&i*/ (drop=_type_ _freq_) sum=denom/*_&i*/;
/*where year=&i;*/
run;
/*%end;
%mend loop;
%loop;
*/

/*%macro loop;
%do i=2003 %to 2012 %by 3;
  %do j=2 %to 4;*/
proc sort data=dxs_allages out=dxs/*_&j*/;
by /*year*/ descending count;
/*where year=&i and agegrp=&j;*/
run;
data dxs/*_&j*/;
length rank 3;
set dxs/*_&j*/;
merge dxs/*_&j*/ all_afm /*(where=(agegrp=&j))*/;
/*by agegrp;*/
if _n_<=20;
if denom=. then denom=2006.3179658;
rank=_n_;
percent=round((count/denom*100),.1);
/*percent=round(percent),.1;*/
format percent 4.1 DX $I9DXF.;
/*rename dx=dx_&i count=count_&i percent=percent_&i;*/
format count comma9.;
/*drop year percent;*/
run;

options orientation=landscape;
ods rtf file="\\cdc.gov\private\L317\icj2\Studies\AFM\AFM_secondary_diagnoses_&sysdate..rtf" style=rtf
bodytitle sasdate startpage=no;
proc print data=dxs noobs;
var /*rank*/ dx count percent  /*dx_2006 count_2006 percent_2006 
    blank dx_2009 count_2009 percent_2009 blank dx_2012 count_2012 percent_2012*/;
/*by agegrp;*/
title "Leading secondary discharge diagnoses associated with 1st listed AFM (with EMG/LP/MRI)";
run;
ods rtf close;
***Determine top 5 AFP diagnoses for first-listed AFM hospitalizations - AC's way;
/*data all_dxs;
set afm_small_proc (keep=year recnum age agegrp dx1-dx15 discwt afm_a afm_f afm_f_pr);
array aa (15) $ dx1-dx15;
do i=1 to 15;
if afm_f_pr=1 and aa(i) in ('04590','32352','32382','32342','33529','34489','3239','3358','3359','3441','3442','3449') then do;
  dx=aa(i);
  output;
end;
end;
FORMAT DX1-DX15 $I9DXF.;
run;
proc sql;
create table all_dxs_sum as
select year,agegrp,dx, sum(discwt) as dx_n
from all_dxs
group by year,agegrp, dx;
quit;
proc freq data=all_dxs_sum noprint;
table agegrp*dx*year/out=dxs_allages;
weight dx_n;
*where agegrp=2;
run;
proc sort data=dxs_allages;
by agegrp year count;
run;
*********************************************************************
**** total hosps by age grp for calculating percentages by total ****
**** hosps by age group and year                                 ****
*********************************************************************;
%macro loop;
%do i=2003 %to 2012 %by 3;
proc summary data=afm_small_proc nway;
class agegrp;
var afm_f_pr;
weight discwt;
output out=first_afm_&i (drop=_type_ _freq_) sum=denom_&i;
where year=&i;
run;
%end;
%mend loop;
%loop;


%macro loop;
%do i=2003 %to 2012 %by 3;
  %do j=2 %to 3;
proc sort data=dxs_allages out=dxs_&i._&j;
by year descending count;
where year=&i and agegrp=&j;
run;
data dxs_&i._&j;
length rank 3;
*set dxs_&i._&j;
merge dxs_&i._&j first_afm_&i (where=(agegrp=&j));
by agegrp;
if _n_<=15;
rank=_n_;
percent_&i=round((count/denom_&i*100),.1);
format percent_&i 4.1 DX $I9DXF.;;
rename dx=dx_&i count=count_&i /*percent=percent_&i;
format count comma9.;
drop year percent;
run;
%end;
%end;
%mend loop;
%loop;

data first_dx_2;
merge dxs_2003_2 dxs_2006_2 dxs_2009_2 dxs_2012_2;
run;
data first_dx_3;
merge dxs_2003_3 dxs_2006_3 dxs_2009_3 dxs_2012_3;
run;

data first_dx_by_age;
set first_dx_2 first_dx_3;
**** used to get a column to separate each year in the table ****;
blank=' ';
run;

proc print data=first_dx_by_age noobs;
var rank dx_2003 count_2003 percent_2003 blank dx_2006 count_2006 percent_2006 
    blank dx_2009 count_2009 percent_2009 blank dx_2012 count_2012 percent_2012;
by agegrp;
title "Leading AFM discharge diagnoses by age group for first-listed AFP hospitalizations with MRI/LP/EMG procedures";
run;*/
ods rtf close;
options orientation=portrait;
