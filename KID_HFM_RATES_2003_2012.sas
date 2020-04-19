/* 
/ Program   : KID_HFM_Rates_2003_2012
/ Version   : 3.0 
/ Author    : Jessica M. Rudd 
/ Date      : 13 MAY 2016
/ Contact   : icj2@cdc.gov
/ Purpose   : Explore Core dataset with basic univariate stats and determine rates
				and standard errors for hand, foot, mouth disease 2003-2012
/ SubMacro	: 
/ Notes     : ***Calculating National Estimates - discharge weight elements***;
			  *** 2003 forward •DISCWT for all analyses
			  *** 2000 •DISCWT to create nationwide estimates for all analyses except those that involve total charges.
         			   •DISCWT_CHG to create nationwide estimates of total charges.
			  *** 1997 •DISCWT_U for all analyses ***;
/ Usage     : 
/ 
/=============================================================================== 
/ PARAMETERS: 
/-------name------- -------------------------description------------------------ 
  NEDS										Study Year
/=============================================================================== 
/ AMENDMENT HISTORY: 
/=============================================================================*/

proc format;
VALUE AGEGRP 1='LT 1' 2='1-4' 3='5-20';
VALUE AGEMO 0='<=2 MONTHS' 1='3--11 MONTHS' 2='12--60MONTHS';
VALUE SEX 1='MALE' 2='FEMALE';
VALUE RACE 1='WHITE' 2='BLACK' 3='HISPANIC' 4='ASIAN/PI' 5='AI/AN' 6='OTHER';
VALUE REGION 1='NORTHEAST' 2='MIDWEST' 3='SOUTH' 4='WEST';
VALUE PAY 1='MEDICARE' 2='MEDICAID' 3='PRIVATE INSURANCE' 4='SELF PAY' 5='NO CHARGE' 6='OTHER' ;
VALUE ZIP 1='0-25th' 2='26th-50th' 3='51st-75th' 4='76th-100th';
run;

%LET DENOM=100000; *** Denominator for rates is 100,000 person visits***;

LIBNAME KID '\\cdc.gov\private\L317\icj2\Studies\HFM';
LIBNAME FORMATS '  \\cdc.gov\private\L317\icj2\NCHS\HCUP\KID\Data\Formats';                                             
Proc contents data=kid.kid_hfm_03_12;
run;
 
options fmtsearch=(formats.formats);
options bufno=500 bufsize=32k;
DATA KID_HFM_ANALYSIS(sgio=yes) ;
set KID.KID_HFM_03_12 (sgio=yes);

/*Enter new age group definitions*/ 
IF AGE=0 or agemonth lt 12 or ageday lt 365 THEN AGEGRP=1; *LT 1;
IF AGE in (1,2,3,4) or agemonth >=12 THEN AGEGRP=2;  *1-5;
IF AGE GT 4 AND AGE LT 20 THEN AGEGRP=3;
IF AGEMONTH IN (0,1,2) THEN AGEMO=0; else
IF AGEMONTH GT 2 AND AGEMONTH LT 12 THEN AGEMO=1;else
IF AGEMONTH GE 12 AND AGEMONTH LE 60 THEN AGEMO=2;
ELSE AGEMO=.;
if atype=4 then delete;
IF YEAR IN (2006,2009,2012) and agegrp in (1,2);
dischgs =1;
if hfm ne 1 then hfm =2;

FORMAT YEAR 5. AGEGRP AGEGRP. AGEMO AGEMO. SEX SEX. REGION REGION. RACE RACE.;
RUN;

PROC SORT DATA=KID_HFM_ANALYSIS;
BY KID_STRATUM HOSPID;
RUN;

proc freq data=kid_HFM_analysis;
tables  YEAR age agegrp sex region HFM HFM_F amonth race died;
run;

DATA kid.kid_HFM_ANALYSIS;
SET kid_HFM_ANALYSIS;
RUN;

Data kid_hfm_analysis;
set kid.kid_hfm_analysis;
run;


***Create new survey weights in order to run domain analysis with medians and percentiles ***;
***Justification for this method found:    Graubard BI, Korn EL.  Survey inference for subpopulations.  American Journal of Epidemiology 1996;
Data kid_hfm_wt;
set kid_hfm_analysis;
if hfm=1 /*then discwt_domain=discwt*/;
/*else discwt_domain=0.0000001;*/
run;

Data nonHFM (keep=kid_stratum hospid discwt);
set kid_hfm_analysis;
if hfm=2;
discwt=0.0000000001;
run;

proc sort data=nonHFM nodupkey;
by kid_stratum hospid;
run;

Data HFM_weights;
set kid_hfm_wt nonHFM;
run;

ods rtf file="\\cdc.gov\private\L317\icj2\Studies\HFM\HFMD_demographics_&sysdate..rtf" bodytitle sasdate style=rtf;
title 'Weighted median length of stay (LOS) for HFMD hospitalizations';
proc surveymeans data=HFM_weights quartiles sum;
class year ;
cluster hospid;
strata kid_stratum;
var los cost totchg age;
weight discwt;
domain year;
run;

title 'Weighted frequencies of HFMD hospitalizations';
proc surveyfreq data=HFM_weights;
tables died agegrp region sex amonth year*(agegrp amonth region sex);
cluster hospid;
strata kid_stratum;
weight discwt;
run;
ods rtf close;

proc print data=HFM_weights;
where discwt=.;
run;


*** Determine top 5 accompanying diagnoses ***;
ODS PDF FILE="\\cdc.gov\private\L317\icj2\Studies\HFM\HFM_diagnoses_&sysdate..pdf";
%Macro diagnosis;
 %LOCAL I;
 %DO I=1 %TO 15;
 options fmtsearch=(formats.formats);
 DATA DX&I (KEEP=HOSPID hfm hfm_f hfm_3 hfm_herp DX);
 	SET HFM_weights;
	IF DX&I NE ' ' THEN DX=DX&I;
	FORMAT DX1-DX15 $I9DXF.;
 RUN;

 %END;
%MEND DIAGNOSIS;
%DIAGNOSIS;
QUIT;
options fmtsearch=(formats.formats);
DATA DX;
SET DX1 DX2 DX3 DX4 DX5 DX6 DX7 DX8 DX9 DX10 DX11 DX12 DX13 DX14 DX15;
RUN;

***Top 5 diagnoses for all coded HFM***;
PROC FREQ DATA=DX;
TABLES DX/noprint out=byvalue;
where hfm=1 and DX ne ' ';
format dx $I9DXF.;
RUN;
proc sort data=byvalue out=bycount;
by descending count;
run;
title'Top 5 diagnoses for all coded HFM';
options fmtsearch=(formats.formats);
proc print data=bycount (obs=10);
run;

*** Top 5 diagnosis for primary coded HFM***;
*** Determine top 5 accompnaying diagnoses ***;

PROC FREQ DATA=DX;
TABLES DX/noprint out=byvalue;
where hfm_f=1 and DX ne ' ';
format dx $I9DXF.;
RUN;
proc sort data=byvalue out=bycount;
by descending count;
run;
title 'Top 5 diagnosis for primary coded HFM';
options fmtsearch=(formats.formats);
proc print data=bycount (obs=10);
run;
/*
*** Top 5 diagnoses for first 3 coded HFM ***;
PROC FREQ DATA=DX;
TABLES DX/noprint out=byvalue;
where hfm_3=1 and DX ne ' ';
RUN;
proc sort data=byvalue out=bycount;
by descending count;
run;
title 'Top 5 diagnoses for first 3 coded HFM';
options fmtsearch=(formats.formats);
proc print data=bycount (obs=7);
run;

*** Top 5 diagnoses for HFM not coded in first 3 ***;
PROC FREQ DATA=DX;
TABLES DX/noprint out=byvalue;
where hfm_3=0 and DX ne ' ';
RUN;
proc sort data=byvalue out=bycount;
by descending count;
run;
title 'Top 5 diagnoses for HFM not coded in first 3';
options fmtsearch=(formats.formats);
proc print data=bycount (obs=7);
run;*/

ods pdf close;

*** Determine top 5 accompnaying procedures ***;
ODS PDF FILE="\\cdc.gov\private\L317\icj2\Studies\HFM\HFM_procedures_&sysdate..pdf";
%Macro procedure;
 %LOCAL I;
 %DO I=1 %TO 10;
 options fmtsearch=(formats.formats);
 DATA PR&I (KEEP=HOSPID hfm hfm_f hfm_3 PR);
 	SET HFM_weights;
	IF PR&I NE ' ' THEN PR=PR&I;
 RUN;

 %END;
%MEND procedure;
%procedure;
QUIT;
options fmtsearch=(formats.formats);
DATA PR;
SET PR1 PR2 PR3 PR4 PR5 PR6 PR7 PR8 PR9 PR10;
RUN;

***Top 5 procedures for all coded HFM***;
PROC FREQ DATA=PR;
TABLES PR/noprint out=byvalue;
where hfm=1 and PR ne ' ';
format pr $I9PRF.;
RUN;
proc sort data=byvalue out=bycount;
by descending count;
run;
title'Top 5 procedures for all coded HFM';
options fmtsearch=(formats.formats);
proc print data=bycount (obs=6);
run;

*** Top 5 procedurces for primary coded HFM***;
*** Determine top 5 accompnaying procedures ***;

PROC FREQ DATA=PR;
TABLES PR/noprint out=byvalue;
where hfm_f=1 and PR ne ' ';
format pr $I9PRF.;
RUN;
proc sort data=byvalue out=bycount;
by descending count;
run;
title 'Top 5 procedures for primary coded HFM';
options fmtsearch=(formats.formats);
proc print data=bycount (obs=6);
run;

/**** Top 5 diagnoses for first 3 coded HFM ***;
PROC FREQ DATA=PR;
TABLES PR/noprint out=byvalue;
where hfm_3=1 and PR ne ' ';
RUN;
proc sort data=byvalue out=bycount;
by descending count;
run;
title 'Top 5 procedures for first 3 coded HFM';
options fmtsearch=(formats.formats);
proc print data=bycount (obs=6);
run;

*** Top 5 diagnoses for HFM not coded in first 3 ***;
PROC FREQ DATA=PR;
TABLES PR/noprint out=byvalue;
where hfm_3=0 and PR ne ' ';
RUN;
proc sort data=byvalue out=bycount;
by descending count;
run;
title 'Top 5 procedures for HFM not coded in first 3';
options fmtsearch=(formats.formats);
proc print data=bycount (obs=6);
run;*/
ods pdf close;

**** Comparing years, regions and age groups using multiple comparison tests ***;
ODS PDF FILE="\\cdc.gov\private\L317\icj2\Studies\HFM\HFM_comparisons_&sysdate..pdf";
title "Multiple comparison tests of years";
proc freq data=kid_hfm_analysis;
where hfm=1;
	tables year/chisq;
run;

title "Multiple comparison tests of regions";
proc freq data=kid_hfm_analysis;
where hfm=1;
	tables region/chisq;
run;

title "Multiple comparison tests of age groups";
proc freq data=kid_hfm_analysis;
where hfm=1;
	tables agegrp agemo/chisq;
run;

title "Multiple comparison tests of sex";
proc freq data=kid_hfm_analysis;
where hfm=1;
	tables sex/chisq;
run;

title "Requesting Multiple Comparison Tests - Year";
proc glm data=kid_hfm_analysis;
	class year;
	model HFM=year/ss3;
	means year / snk;
	lsmeans year/pdiff adjust=tukey;
run;
quit;

title "Requesting Multiple Comparison Tests - Region";
proc glm data=kid_hfm_analysis;
	class region;
	model HFM=region/ss3;
	means region / snk;
	lsmeans region/pdiff adjust=tukey;
run;
quit;

title "Requesting Multiple Comparison Tests - Age Group";
proc glm data=kid_hfm_analysis;
	class agegrp;
	model HFM=agegrp/ss3;
	means agegrp / snk;
	lsmeans agegrp/pdiff adjust=tukey;
run;
quit;

title "Requesting Multiple Comparison Tests - Age Group - Months";
proc glm data=kid_hfm_analysis;
	class agemo;
	model HFM=agemo/ss3;
	means agemo / snk;
	lsmeans agemo/pdiff adjust=tukey;
run;
quit;

title "Requesting Multiple Comparison Tests - Sex";
proc glm data=kid_hfm_analysis;
	class sex;
	model HFM=sex/ss3;
	means sex /snk;
	lsmeans sex/pdiff adjust=tukey;
run;
quit;
ods pdf close;
/*title "Effect of year";
proc logistic data=kid_hfm_analysis;
	class year ;
	model hfm = year /expb;
run;

title "Effect of year and age group";
proc logistic data=kid_hfm_analysis;
	class year agegrp;
	model hfm = agegrp year*agegrp /expb;
run;

title "Effect of year and sex";
proc logistic data=kid_hfm_analysis;
	class year sex;
	model hfm = sex year*sex /expb;
run;

title "Effect of year and region";
proc logistic data=kid_hfm_analysis;
	class year region;
	model hfm = region year*region /expb;
run;*/

***************************************************************
****** READS IN THE US CENSUS DATA FOR RATE CALCULATIONS ******
***************************************************************;

LIBNAME CEN '\\cdc.gov\project\NCIRD_DVD_EB_DATA_1\census\bridged_race\data';

DATA CEN;                                                                       
  SET CEN.br90_12_b (KEEP=YEAR SEX AGE AGE5A POP STATE RACE);                       
IF YEAR IN (2006,2009,2012);                                                      
DIED=2; 
IF AGE5A=0 or age = 0 THEN AGEGRP=1; *LT 1;
ELSE IF AGE5A=1 or age in (1,2,3,4)THEN AGEGRP=2;  *1-4;
ELSE IF AGE5A=2 OR AGE IN (5,6,7,8,9)THEN AGEGRP=3; *5-9;
ELSE IF AGE5A IN (3,4) OR AGE IN (10,11,12,13,14,15,16,17,18,19,20)THEN AGEGRP=4;
ELSE AGEGRP=.;

****ENTER NEW AGE DEFINITION ABOVE THIS LINE****;  ***STEP_15***;               
FORMAT AGEGRP AGEGRP. SEX SEX.;                                                          
                                               
LABEL POP='AGE GROUP CENSUS';                                                  
  IF STATE IN(23,33,50,25,44,09,36,34,42) THEN REGION=1;                        
  IF STATE IN (39,18,17,26,55,27,19,29,38,46,31,20) THEN REGION=2;              
  IF STATE IN(10,24,11,51,54,37,45,13,12,21,47,01,28,05,22,40,48)               
    THEN REGION=3;                                                              
  IF STATE IN(30,16,56,08,35,04,49,32,53,41,06,02,15) THEN REGION=4;            
IF AGEGRP=. OR AGEGRP GT 2 THEN DELETE;
RENAME POP=POP2; 
RUN;                                                                            
     

PROC SUMMARY NWAY MISSING;                                                      
  CLASS YEAR SEX AGE AGE5A AGEGRP REGION;                                
  VAR POP2;                                                                     
OUTPUT OUT=CENSUS SUM=;                                                         
RUN;  

Data KID.census;
set cen;
run;


*************************************************************************
***** THIS SECTION CREATES OUTPUT DATASETS THAT HAVE THE ESTIMATES ******
***** AND SES BY DEMOGRAPHIC CHARACTERISTICS AND TIME PERIOD       ******
***** TO BE USED FOR RATE CALCULATIONS                             ******
*************************************************************************;
*SEX, AGEGRP, YEAR, REGION;
proc print data=kid_hfm_analysis;
where discwt=.;
run;

Data HFM_weights_rates;
set HFM_weights;
if agegrp=. or agegrp gt 2 then delete;
run;


options mprint;
DATA CAT;                                                                       
INPUT CAT $ @@;                                                    
CARDS;                                                                          
HFM
;
RUN;

DATA _NULL_;
SET CAT END=LAST;
CALL SYMPUT('CAT'||LEFT(PUT(_N_,3.)),CAT);
IF LAST THEN CALL SYMPUT('COUNT',_N_);
RUN; 

/*%MACRO LOOP;
%LOCAL I;
%DO I=1 %TO &COUNT;
%let ti=%cmpres(&&cat&i);
ODS RTF FILE="\\cdc.gov\private\L317\icj2\HCUP\KID\Documentation\&ti._rates_&sysdate..rtf";

TITLE "HFM Inpatient Visits, CATEGORY=&TI, US, 2003-2009, KID";
PROC CROSSTAB DATA=WORK.KID_HFM_ANALYSIS FILETYPE=SAS DESIGN=WR;
  WEIGHT DISCWT;
  SUBPOPN &TI=1;
  NEST kid_stratum HOSPID/ MISSUNIT;
  CLASS YEAR AGEGRP REGION amonth SEX/INCLUDE=MISSING;
  TABLES YEAR AGEGRP REGION sex YEAR*(amonth agegrp region);
  SETENV LEFTMGN=1;*COLWIDTH=10 DECWIDTH=0;   
  RTITLE "HFM Inpatient Visits, CATEGORY=&TI, US [KID], 2003-2009";
  PRINT NSUM WSUM COLPER ROWPER SEWGT SECOL SETOT;
  OUTPUT NSUM="UNWEIGHTED CASES" WSUM="WEIGHTED CASES" SEWGT="STANDARD ERROR"/FILENAME="WORK.VAR97"
  FILETYPE=SAS REPLACE;
  FORMAT YEAR 4. SEX SEX.; 
run;
quit;*/

*** Age group by months of age, including less than 2 months ***;%MACRO LOOP;
%LOCAL I;
%DO I=1 %TO &COUNT;
%let ti=%cmpres(&&cat&i);
ODS RTF FILE="\\cdc.gov\private\L317\icj2\Studies\HFM\&ti._rates_&sysdate..rtf";

TITLE "HFM Inpatient Visits, CATEGORY=&TI, US, 2003-2012, KID";
PROC CROSSTAB DATA=WORK.HFM_weights_rates FILETYPE=SAS DESIGN=WR;
  WEIGHT DISCWT;
  SUBPOPN &TI=1;
  NEST kid_stratum HOSPID/ MISSUNIT;
  CLASS YEAR AGEGRP REGION SEX/INCLUDE=MISSING;
  TABLES YEAR*(AGEGRP sex region);
  SETENV LEFTMGN=1;*COLWIDTH=10 DECWIDTH=0;   
  RTITLE "HFM Inpatient Visits, CATEGORY=&TI, US [KID], 2003-2012";
  PRINT NSUM WSUM COLPER ROWPER SEWGT SECOL SETOT;
  OUTPUT NSUM="UNWEIGHTED CASES" WSUM="WEIGHTED CASES" SEWGT="STANDARD ERROR"/FILENAME="WORK.VAR97"
  FILETYPE=SAS REPLACE;
  FORMAT YEAR 4. SEX SEX. agegrp agegrp. region region.; 
run;
quit;
************************
****** TOTAL RATE  *****
************************;

DATA TOTHOSPS;
SET VAR97;
IF YEAR=0 AND SEX=0 AND AGEGRP=-2 AND REGION=-2 /*AND AMONTH=-2 AND RACE=-2*/;
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
IF YEAR>0 AND region=-2 AND AGEGRP=-2 /*AND amonth=-2 and race=-2*/ AND SEX=0;
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


/*PROC SQL;                                                                       
CREATE VIEW T1 AS                                                               
 SELECT YEAR AS YEAR1, RATE AS RATE1, RATERSE1                           
 FROM ALL                                                                      
 WHERE YEAR=2006;                                                                
CREATE VIEW T2 AS                                                               
SELECT YEAR AS YEAR2, RATE AS RATE2, RATERSE1 AS RATERSE2                
 FROM ALL                                                                      
 WHERE YEAR=2009;                                                                
CREATE TABLE TTEST AS                                                           
 SELECT T1.*, T2.*                                                              
FROM T1, T2;                                                                    

DATA TTEST;
SET TTEST;
T=(RATE1-RATE2)/SQRT((RATERSE1**2)+(RATERSE2**2));
**** PLUG IN 100,000 AS A DUMMY VALUE FOR THE DEGREES OF FREEDOM ****;
  P=(1-PROBT(ABS(T),100000))*2;
RUN;

PROC PRINT DATA=TTEST (DROP=YEAR2 RATE2 RATERSE2);                                                                     
RUN;  */
**********************************
**** TEST FOR TREND OVER TIME ****
**********************************;
DATA TREND;
SET ALL;
X=YEAR;
Y=RATE;
 RSE=(SEWGT/WSUM);                          * MULTIPLY TIMES 100 TO GE %;      
 RATESE=Y*RSE;                       * SE FOR RATE;                             
 W=1/(RATESE**2);                                                               
 WXY=W*X*Y;                                                                     
 WX=W*X;                                                                        
 WY=W*Y;                                                                        
 WX2=W*(X**2);                                                                  
RUN;
 
PROC SUMMARY NOPRINT; 
VAR W X RATE WXY WX WY WX2;
OUTPUT OUT=TOTALS SUM= ;
RUN;
                                                                                
DATA TOTT; 
SET TOTALS;                                                              
XM=WX/W;                                                                       
YM=WY/W;                                                                       
B=(WXY-(W*(XM*YM)))/(WX2-(W*(XM**2)));
S=(1/(WX2-(W*(XM**2))))**(1/2);                                               
Z=B/S;
P=(1-PROBNORM(ABS(Z)))*2;
RUN;

PROC PRINT; 
VAR B S Z P;                                                            
TITLE2 'TEST FOR TREND OVERALL RATES';                                          
RUN;                 

***************************
****** BY AGE GROUP  *****
***************************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR=0 AND AGEGRP > 0 /*AND RACE=0 AND amonth=-2 AND REGION=0 AND SEX=0*/;
KEEP AGEGRP WSUM SEWGT;
RUN;

PROC SORT;
BY AGEGRP ;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT AGEGRP, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY AGEGRP;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY AGEGRP ;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
BY AGEGRP;
TITLE2 "RATE BY AGE GROUP ";
RUN;

PROC SQL;                                                                       
CREATE VIEW T1 AS                                                               
 SELECT AGEGRP AS AGEGRP1, RATE AS RATE1, RATERSE1                           
 FROM ALL                                                                      
 WHERE AGEGRP=1;                                                                
CREATE VIEW T2 AS                                                               
SELECT AGEGRP AS AGEGRP2, RATE AS RATE2, RATERSE1 AS RATERSE2                
 FROM ALL                                                                      
 WHERE AGEGRP=2;                                                                
CREATE TABLE TTEST AS                                                           
 SELECT T1.*, T2.*                                                              
FROM T1, T2;                                                                    

DATA TTEST;
SET TTEST;
T=(RATE1-RATE2)/SQRT((RATERSE1**2)+(RATERSE2**2));
**** PLUG IN 100,000 AS A DUMMY VALUE FOR THE DEGREES OF FREEDOM ****;
  P=(1-PROBT(ABS(T),100000))*2;
RUN;

PROC PRINT DATA=TTEST (DROP=YEAR2 AGEGRP2 RATE2 RATERSE2);                                                                     
RUN;  

********************************
****** BY MONTH & AGE GROUP  *****
********************************;
/*DATA TOTHOSPS;
SET VAR97;
IF YEAR>0 AND amonth>0 AND AGEGRP>0 AND REGION = 0 AND RACE=0 AND SEX=0;
KEEP YEAR amonth AGEGRP WSUM SEWGT;
RUN;

PROC SORT;
BY YEAR amonth AGEGRP;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT year, AGEGRP, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY year, AGEGRP;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY year AGEGRP;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
BY amonth;
TITLE2 "RATE BY MONTH AND AGE GROUP";
RUN;

************************
****** BY MONTH   *****
************************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR>0 AND AGEGRP=-2 AND AMONTH>0 AND RACE=-2 AND REGION=-2 AND SEX=-2;
KEEP amonth YEAR WSUM SEWGT;
RUN;

PROC SORT;
BY YEAR AMONTH;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT year, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY year AMONTH;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY year AMONTH;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
BY AMONTH;
TITLE2 "RATE BY MONTH";
RUN;*/

************************
****** BY RACE   *****
************************;
/*DATA TOTHOSPS;
SET VAR97;
IF YEAR>0 AND AGEGRP=0 AND AMONTH=-2 AND RACE > 0 AND REGION=0 AND SEX=0;
KEEP YEAR RACE WSUM SEWGT;
RUN;

PROC SORT;
BY RACE YEAR;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT year, RACE, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY RACE, year;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY RACE YEAR;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
BY RACE;
TITLE2 "RATE BY RACE";
RUN;

PROC SQL;                                                                       
CREATE VIEW T1 AS                                                               
 SELECT YEAR AS YEAR1, RACE AS RACE1, RATE AS RATE1, RATERSE1                           
 FROM ALL                                                                      
 WHERE YEAR=2009 AND RACE=3;                                                                
CREATE VIEW T2 AS                                                               
SELECT YEAR AS YEAR2, RACE AS RACE2, RATE AS RATE2, RATERSE1 AS RATERSE2                
 FROM ALL                                                                      
 WHERE YEAR=2009 AND RACE=1;                                                                
CREATE TABLE TTEST AS                                                           
 SELECT T1.*, T2.*                                                              
FROM T1, T2;                                                                    

DATA TTEST;
SET TTEST;
T=(RATE1-RATE2)/SQRT((RATERSE1**2)+(RATERSE2**2));
**** PLUG IN 100,000 AS A DUMMY VALUE FOR THE DEGREES OF FREEDOM ****;
  P=(1-PROBT(ABS(T),100000))*2;
RUN;

PROC PRINT DATA=TTEST (DROP=YEAR2 RACE2 RATE2 RATERSE2);                                                                     
RUN;  */

************************
****** BY REGION   *****
************************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR=0 AND /*AGEGRP=0 AND AMONTH=-2* AND RACE=-2 AND */ REGION>0  /*AND SEX=0*/;
KEEP REGION WSUM SEWGT;
RUN;

PROC SORT;
BY REGION ;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT REGION, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY REGION;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY REGION ;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
BY REGION;
TITLE2 "RATE BY REGION";
RUN;

/*PROC SQL;                                                                       
CREATE VIEW T1 AS                                                               
 SELECT YEAR AS YEAR1, REGION AS REGION1, RATE AS RATE1, RATERSE1                           
 FROM ALL                                                                      
 WHERE YEAR=2009 AND REGION=3;                                                                
CREATE VIEW T2 AS                                                               
SELECT YEAR AS YEAR2, REGION AS REGION2, RATE AS RATE2, RATERSE1 AS RATERSE2                
 FROM ALL                                                                      
 WHERE YEAR=2009 AND REGION IN (1,2,4);                                                                
CREATE TABLE TTEST AS                                                           
 SELECT T1.*, T2.*                                                              
FROM T1, T2;                                                                    

DATA TTEST;
SET TTEST;
T=(RATE1-RATE2)/SQRT((RATERSE1**2)+(RATERSE2**2));
**** PLUG IN 100,000 AS A DUMMY VALUE FOR THE DEGREES OF FREEDOM ****;
  P=(1-PROBT(ABS(T),100000))*2;
RUN;

PROC PRINT DATA=TTEST (DROP=YEAR2 REGION2 RATE2 RATERSE2);                                                                     
RUN;  */

************************
****** BY SEX   *****
************************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR=0 AND /*AGEGRP=0 AND AMONTH=-2 AND RACE=-2 AND REGION=0 AND */SEX > 0;
KEEP SEX WSUM SEWGT;
RUN;

PROC SORT;
BY SEX ;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT SEX, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY SEX;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY SEX ;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
BY SEX;
TITLE2 "RATE BY SEX ";
RUN;

PROC SQL;                                                                       
CREATE VIEW T1 AS                                                               
 SELECT SEX AS SEX1, RATE AS RATE1, RATERSE1                           
 FROM ALL                                                                      
 WHERE SEX=1;                                                                
CREATE VIEW T2 AS                                                               
SELECT SEX AS SEX2, RATE AS RATE2, RATERSE1 AS RATERSE2                
 FROM ALL                                                                      
 WHERE SEX=2;                                                                
CREATE TABLE TTEST AS                                                           
 SELECT T1.*, T2.*                                                              
FROM T1, T2;                                                                    

DATA TTEST;
SET TTEST;
T=(RATE1-RATE2)/SQRT((RATERSE1**2)+(RATERSE2**2));
**** PLUG IN 100,000 AS A DUMMY VALUE FOR THE DEGREES OF FREEDOM ****;
  P=(1-PROBT(ABS(T),100000))*2;
RUN;

PROC PRINT DATA=TTEST (DROP=YEAR2 SEX2 RATE2 RATERSE2);                                                                     
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
TITLE2 "RATE BY YEAR";
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
TITLE2 "RATE BY YEAR";
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
TITLE2 "RATE BY YEAR";
RUN;

%END;
%MEND LOOP;
%LOOP;


********************************************************************************
************ Average cost (cost-to-charge) for hfm illness (All diagnoses), 
************ children less than 5 years, by year *******************************
********************************************************************************;

proc descript data=kid.kid_hfm_analysis filetype = sas design=wr;
/*subpopn year=2006;*/
	weight DISCWT_chg;
	nest kid_stratum hospid;
	setenv colwidth = 24 decwidth =5;
	var LOS totchg cost;
	subgroup HFM ;
	levels 2 ;
	print total setotal mean semean ;
	rtitle "Mean length of stay, discharges, total charges and cost of HFM inpatient visits";
run;

/*proc descript data=kid.kid_hfm_analysis filetype = sas design=wr;
subpopn year=2009;
	weight DISCWT_chg;
	nest kid_stratum hospid;
	setenv colwidth = 24 decwidth =5;
	var LOS totchg cost;
	subgroup HFM ;
	levels 2 ;
	print total setotal mean semean ;
	rtitle "Mean length of stay, discharges, total charges and cost of HFM inpatient visits";
run;
/*proc descript data=kid.kid_hfm_analysis filetype=sas design=wr;
	weight discwt_chg;
	nest kid_stratum hospid;
	setenv colwidth =24 decwidth = 5;
	var Cost;
	subgrop HFM;
	levels 2;
	print total setotal mean semean ;
	rtitle "Mean cost of HFM inpatient visits";
run;*/
ODS RTF CLOSE;
