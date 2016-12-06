/* 
/ Program   : NEDS_HFM_create
/ Version   : 2.0 
/ Author    : Jessica M. Rudd 
/ Date      : 21 May 2015
/ Contact   : icj2@cdc.gov
/ Purpose   : Create subset data from all years of study for cases of hand, foot and mouth disease
/ Notes     : 
/ Usage     : 
/ 
/=============================================================================== 
/ PARAMETERS: 
/-------name------- -------------------------description------------------------ 
  NEDS										Study Year
/=============================================================================== 
/ AMENDMENT HISTORY: 
/ init --date-- mod-id ----------------------description------------------------
 JMR	02/17/2012							Added year 2009 data
 JMR	05/21/2015							Added years 2010-2012
/=============================================================================*/

/*Import core datasets*/
%MACRO LOOP;
 %LOCAL I;
 %DO I=2006 %TO 2012; /* Update here when new datasets become available*/

LIBNAME NEDS '\\cdc\project\NCIRD_DVD_EB_DATA_1\hcup\neds\data';

DATA NEDS_&I._CORE (compress=binary);
	SET NEDS.NEDS_&I._CORE;
	RUN;

PROC CONTENTS DATA=NEDS_&I._CORE; *** Review contents of dataset;
	RUN;

	%END;
	%MEND LOOP;
%LOOP;
QUIT;

   
/* Create a variable indicating whether any diagnosis code includes HFMD*/

%MACRO LOOP;
 %LOCAL I;
 %DO I=2006 %TO 2012;

DATA HFM_&I (compress=binary);
	SET NEDS_&I._CORE;
		ARRAY DIAGNOSIS (15) $ DX1-DX15;
			DO i=1 to 15;
				IF SUBSTR(DIAGNOSIS(i),1,4) IN('0743')THEN DO;
  				HFM=1;
  				IF I=1 THEN HFM_F=1; ELSE HFM_F=0;
			END;
		END;
	/*Recode gender variable*/
	IF FEMALE=0 THEN SEX=1;else;
	IF FEMALE=1 THEN SEX=2;
	RUN;
	%END;
	%MEND LOOP;
%LOOP;
QUIT;

/*Combine years and output subpopulation to control size and processing of analysis dataset*/
DATA HFM_06_12;
SET HFM_2006 HFM_2007 HFM_2008 HFM_2009 HFM_2010 HFM_2011 HFM_2012;
IF HFM=1 AND AGE LT 5 THEN OUTPUT;
RUN;

/*Checkout subpopulation dataset*/
PROC FREQ DATA=HFM_06_12;
	TABLES HFM HFM_F SEX AGE;
	BY YEAR;
		%LET TITLE=%STR(FREQUENCY OF HFM);
		ODS PROCLABEL "NEDS &TITLE";
	RUN;

	
LIBNAME HFM '\\cdc.gov\private\L317\icj2\Studies\HFM';   
/*Save subpop dataset to permament file*/
DATA HFM.HFM_06_12;
	SET HFM_06_12;
	RUN;

DATA HFM_06_12;
	SET HFM.HFM_06_12;
	RUN;

***DELETE WORK DATASETS TO AVOID RUNNING OUT OF RESOURCES IN SAS ***;

%MACRO LOOP;
	%LOCAL I;
	%DO I=2006 %TO 2012;
	PROC DATASETS;
	DELETE HFM_&I NEDS_&I._CORE;
	RUN;
	%END;
	%MEND LOOP;
%LOOP;
QUIT;
/*Create formats for study variables of interest*/
PROC FORMAT;

	VALUE SEX 1='MALE' 2='FEMALE';

	VALUE AGEGRP 1='LT 1' 2='1-4';
	
	VALUE REGION 1='NORTHEAST' 2='MIDWEST' 3='SOUTH' 4='WEST';

	VALUE MONTHF 1='JAN' 2='FEB' 3='MAR' 4='APR' 5='MAY' 6='JUN' 7='JUL' 8='AUG'
				 9='SEP' 10='OCT' 11='NOV' 12='DEC';
	RUN;

%LET DENOM=100000; *** Denominator for rates is 100,000 person visits***;

/*Create hospital weights table from NEDS hosp weights files*/
%MACRO LOOP;
 %LOCAL I;
 %DO I=2006 %TO 2012;

PROC SQL;
	CREATE TABLE HOSP&I AS
	SELECT YEAR,HOSP_ED,HOSP_CONTROL,HOSP_TRAUMA,HOSP_REGION,
	HOSP_UR_TEACH,NEDS_STRATUM,DISCWT 
	FROM NEDS.NEDS_&I._HOSPITAL
	ORDER BY YEAR,HOSP_ED;
	%END;
	%MEND LOOP;
%LOOP; 
QUIT;

/*Combine hospital weights files*/
DATA HOSPWTS;
	SET HOSP2006 HOSP2007 HOSP2008 HOSP2009 HOSP2010 HOSP2011 HOSP2012;
	RUN; 

/*Create analysis dataset based on study criteria*/
OPTIONS BUFNO=500 BUFSIZE=32k;
DATA HFM_NEDS_ANALYSIS(SGIO=YES) ;
	MERGE HFM.HFM_06_12 (IN=C SGIO=YES DROP=KEY_ED AWEEKEND DQTR CHRON1-CHRON15  
	  INTENT_SELF_HARM DXCCS1-DXCCS15 E_CCS1-E_CCS4 I NEDS_STRATUM)
      HOSPWTS (IN=B RENAME=(HOSP_REGION=REGION));
		BY YEAR HOSP_ED;

*this adds dummy records for any strata that might not be captured in the subset, to ensure correct variances;
		IF B=1 AND (C NE 1) THEN DO; 
 		HFM=0;
 		DISCWT=0.00000001;
		/*SEX=2; AGE=0; AMONTH=3;*/
		END;

	NEW_STRAT=NEDS_STRATUM||'-'||LEFT(YEAR);
	NEW_STRAT2=YEAR*10000000+NEDS_STRATUM;

	IF AGE=0 THEN AGEGRP=1; ELSE
	IF AGE IN (1,2,3,4) THEN AGEGRP=2;

	IF HFM_F=. THEN HFM_F=0;

	FORMAT YEAR 5. SEX SEX. REGION REGION. AMONTH MONTHF. AGEGRP AGEGRP.;
	RUN;


PROC SORT DATA=HFM_NEDS_ANALYSIS;
	BY NEW_STRAT2 HOSP_ED;
	RUN;

/* Check out analysis dataset */
PROC FREQ DATA=HFM_NEDS_ANALYSIS;
	/*WHERE HFM=1;*/
	TABLES YEAR AGE AGEGRP SEX REGION AMONTH HFM HFM_F;
	RUN;

/*Verify no missing weights */
PROC PRINT DATA=HFM_NEDS_ANALYSIS;
	WHERE DISCWT=.;
	RUN;

/*Save analysis dataset*/
DATA HFM.HFM_NEDS_ANALYSIS;
	SET HFM_NEDS_ANALYSIS;
	RUN;

DATA HFM_NEDS_ANALYSIS;
	SET HFM.HFM_NEDS_ANALYSIS;
	RUN;

/* Calculate weighted total charges*/
proc descript data=hfm_NEDS_analysis filetype = sas design=wr /*notsorted*/;
	weight DISCWT;
	nest NEW_STRAT2 hosp_ed;
	setenv colwidth = 24 decwidth =5;
	var totchg_ed ;
	subgroup HFM ;
	levels 2 ;
	print total setotal mean semean ;
	rtitle "Mean total charges of HFM ed visits";
run;

/* Calculate weighted frequencies of HFMD ED visits*/
proc crosstab data=hfm_NEDS_analysis filetype = sas design=wr /*notsorted*/;
	weight DISCWT;
	nest NEW_STRAT2 hosp_ed;
	setenv colwidth = 24 decwidth =5;
	subpopn HFM=1;
	class year;
	subgroup agegrp sex region ;
	levels 2 2 4 ;
	tables agegrp*sex sex*region agegrp*region;
	test chisq;
	print / style=nchs;
	rtitle "Weighted frequencies of HFMD ED visits";
run;

**** Comparing years, regions and age groups using multiple comparison tests ***;
ODS PDF FILE="\\cdc.gov\private\L317\icj2\Studies\HFM\HFM_NEDS_comparisons_&sysdate..pdf";

title "Multiple comparison tests of years";
proc freq data=hfm_NEDS_analysis;
	where hfm=1;
	tables year/chisq;
	weight discwt;
run;

title "Multiple comparison tests of regions";
proc freq data=hfm_NEDS_analysis;
	where hfm=1;
	tables region/chisq;
	weight discwt;
run;

title "Multiple comparison tests of age groups";
proc freq data=hfm_NEDS_analysis;
	where hfm=1 /*and agegrp le 2*/;
	tables agegrp/chisq;
	weight discwt;
run;

title "Multiple comparison tests of sex";
proc freq data=hfm_NEDS_analysis;
	where hfm=1;
	tables sex/chisq;
	weight discwt;
run;

ods pdf close;

***Determine top diagnoses associated with HFMD discharges;
data all_dxs;
	set HFM_NEDS_ANALYSIS (keep=year age agegrp dx1-dx15 discwt HFM);
	array aa (15) $ dx1-dx15;
		do i=1 to 15;
		if aa(i) ^IN(' ','0743') AND HFM=1 AND AGEGRP NE . then do;
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
	run;
proc sort data=dxs_allages;
	by agegrp year count;
	run;
*********************************************************************
**** total hosps by age grp for calculating percentages by total ****
**** hosps by age group and year                                 ****
*********************************************************************;
%macro loop;
	%do i=2006 %to 2012 ;
	proc summary data=HFM_NEDS_ANALYSIS nway;
	class agegrp;
	var HFM;
	weight discwt;
	output out=all_HFM_&i (drop=_type_ _freq_) sum=denom_&i;
	where year=&i;
	run;
%end;
%mend loop;
%loop;

%macro loop;
	%do i=2006 %to 2012 ;
  	%do j=1 %to 2;
	proc sort data=dxs_allages out=dxs_&i._&j;
	by year descending count;
	where year=&i and agegrp=&j;
	run;

	data dxs_&i._&j;
	length rank 3;
	set dxs_&i._&j;
	merge dxs_&i._&j all_HFM_&I (where=(agegrp=&j));
	by agegrp;
	if _n_<=15;
	rank=_n_;
	percent=round((count/denom_&i*100),.1);
/*percent=round(percent),.1;*/
	format percent 4.1 DX $I9DXF.;;
	rename dx=dx_&i count=count_&i percent=percent_&i;
	format count comma9.;
	/*drop year percent;*/
	run;
%end;
%end;
%mend loop;
%loop;

data all_dx_1;
merge dxs_2006_1 dxs_2007_1 dxs_2008_1 dxs_2009_1 DXS_2010_1 DXS_2011_1 DXS_2012_1;
run;
data all_dx_2;
merge dxs_2006_2 dxs_2007_2 dxs_2008_2 dxs_2009_2 DXS_2010_2 DXS_2011_2 DXS_2012_2;
run;

data all_dx_by_age;
set all_dx_1 all_dx_2;
blank=' ';
run;
options orientation=landscape;
ods rtf file="\\cdc.gov\private\L317\icj2\Studies\HFM\HFMD_DIAGNOSES_&sysdate..rtf" style=rtf
bodytitle sasdate startpage=no;
proc print data=all_dx_by_age noobs;
var rank /*dx count percent*/ dx_2006 count_2006 percent_2006 
    blank dx_2007 count_2007 percent_2007 blank dx_2008 count_2008 percent_2008 
	blank dx_2009 count_2009 percent_2009 blank dx_2010 count_2010 percent_2010
	blank dx_2011 count_2011 percent_2011 blank dx_2012 count_2012 percent_2012;
by agegrp;
title "Leading HMFD-ASSOCIATED discharge diagnoses by age group ";
run;
ods rtf close;

LIBNAME CEN '\\cdc\project\NCIRD_DVD_EB_DATA_1\census\bridged_race\data';

DATA CEN;                                                                       
  SET CEN.br90_13_B (KEEP=YEAR SEX AGE POP STATE);                       
	IF 2006<=YEAR<=2012;                                                      
	DIED=2; 
         
	FORMAT SEX SEX.;                                                          
                                               
	LABEL POP='AGE GROUP CENSUS';                                                  
  	IF STATE IN(23,33,50,25,44,09,36,34,42) THEN REGION=1;                        
  IF STATE IN (39,18,17,26,55,27,19,29,38,46,31,20) THEN REGION=2;              
  IF STATE IN(10,24,11,51,54,37,45,13,12,21,47,01,28,05,22,40,48)               
    THEN REGION=3;                                                              
  IF STATE IN(30,16,56,08,35,04,49,32,53,41,06,02,15) THEN REGION=4;            
IF AGE GT 4 THEN DELETE;
RENAME POP=POP2; 
RUN;                                                                            
     
DATA CEN;
SET CEN;
IF AGE=0 THEN AGEGRP=1;
IF AGE IN (1,2,3,4) THEN AGEGRP=2;
IF SEX=. THEN SEX=1;
FORMAT AGEGRP AGEGRP.;
RUN;

PROC FREQ DATA=CEN;
TABLES YEAR SEX AGEGRP AGE REGION ;
FORMAT YEAR 5. SEX SEX. REGION REGION. ;
RUN;

PROC SUMMARY NWAY MISSING;                                                      
  CLASS YEAR SEX AGEGRP REGION DIED;                                
  VAR POP2;                                                                     
OUTPUT OUT=CENSUS SUM=;                                                         
RUN;  

*************************************************************************
***** THIS SECTION CREATES OUTPUT DATASETS THAT HAVE THE ESTIMATES ******
***** AND SES BY DEMOGRAPHIC CHARACTERISTICS AND TIME PERIOD       ******
***** TO BE USED FOR RATE CALCULATIONS                             ******
*************************************************************************;
*SEX, AGE, YEAR, REGION, AMONTH;

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

%MACRO LOOP;
%LOCAL I;
%DO I=1 %TO &COUNT;
%let ti=%cmpres(&&cat&i);
ODS RTF FILE="\\cdc.gov\private\L317\icj2\Studies\HFM\&ti._rates_&sysdate..rtf" style=rtf
startpage=no bodytitle sasdate;

TITLE "Hand, Foot and Mouth Disease ED Visits,Children under 5 yrs, CATEGORY=&TI (All diagnoses), US, 2006-2012, NEDS";
PROC CROSSTAB DATA=HFM_NEDS_ANALYSIS FILETYPE=SAS DESIGN=WR;
	WEIGHT DISCWT;
	SUBPOPN &TI=1;
	NEST NEW_STRAT2 HOSP_ED/ MISSUNIT;
	CLASS YEAR SEX AGEGRP REGION /INCLUDE=MISSING;
   	TABLES SEX AGEGRP REGION year YEAR*(SEX AGEGRP REGION);
  	SETENV LEFTMGN=1;   
  		RTITLE "Hand, Foot and Mouth ED Visits, Children under 5 yrs, CATEGORY=&TI (All diagnoses), US [NEDS], 2006-2012";
  		PRINT NSUM WSUM COLPER ROWPER SEWGT SECOL SETOT;
  	OUTPUT NSUM="UNWEIGHTED CASES" WSUM="WEIGHTED CASES" SEWGT="STANDARD ERROR"/FILENAME="WORK.VAR97" 
  	FILETYPE=SAS REPLACE;
  	FORMAT YEAR 4. SEX SEX. AGEGRP AGEGRP. AMONTH MONTHF.; 
	QUIT;

************************
****** TOTAL RATE  *****
************************;

DATA TOTHOSPS;
	SET VAR97;
	IF YEAR=0 AND SEX=-2 AND AGEGRP=0 AND REGION=-2;
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
IF YEAR>0 AND SEX=0 AND AGEGRP=-2 AND REGION=-2;
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

PROC SQL;                                                                       
CREATE VIEW T1 AS                                                               
 SELECT YEAR AS YEAR1, RATE AS RATE1, RATERSE1                           
 FROM ALL                                                                      
 WHERE YEAR=2006;                                                                
CREATE VIEW T2 AS                                                               
SELECT YEAR AS YEAR2, RATE AS RATE2, RATERSE1 AS RATERSE2                
 FROM ALL                                                                      
 WHERE YEAR=2007; 
CREATE VIEW T3 AS                                                               
SELECT YEAR AS YEAR3, RATE AS RATE3, RATERSE1 AS RATERSE3                
 FROM ALL                                                                      
 WHERE YEAR=2008; 
CREATE VIEW T4 AS                                                               
SELECT YEAR AS YEAR4, RATE AS RATE4, RATERSE1 AS RATERSE4                
 FROM ALL                                                                      
 WHERE YEAR=2009; 
CREATE VIEW T5 AS                                                               
SELECT YEAR AS YEAR5, RATE AS RATE5, RATERSE1 AS RATERSE5                
 FROM ALL                                                                      
 WHERE YEAR=2010; 
CREATE VIEW T6 AS                                                               
SELECT YEAR AS YEAR6, RATE AS RATE6, RATERSE1 AS RATERSE6                
 FROM ALL                                                                      
 WHERE YEAR=2011; 
 CREATE VIEW T7 AS                                                               
SELECT YEAR AS YEAR7, RATE AS RATE7, RATERSE1 AS RATERSE7                
 FROM ALL                                                                      
 WHERE YEAR=2012; 
CREATE TABLE TTEST AS                                                           
 SELECT T1.*, T2.*, T3.*, T4.*, T5.*, T6.*, T7.*                                                              
FROM T1, T2, T3, T4, T5, T6, T7;   
                                                                  
DATA TTEST1;
SET TTEST;
T=(RATE1-RATE2)/SQRT((RATERSE1**2)+(RATERSE2**2));
**** PLUG IN 100,000 AS A DUMMY VALUE FOR THE DEGREES OF FREEDOM ****;
  P=(1-PROBT(ABS(T),100000))*2;
RUN;
DATA TTEST2;
SET TTEST;
T=(RATE1-RATE3)/SQRT((RATERSE1**2)+(RATERSE3**2));
**** PLUG IN 100,000 AS A DUMMY VALUE FOR THE DEGREES OF FREEDOM ****;
  P=(1-PROBT(ABS(T),100000))*2;
RUN;
DATA TTEST3;
SET TTEST;
T=(RATE2-RATE3)/SQRT((RATERSE2**2)+(RATERSE3**2));
**** PLUG IN 100,000 AS A DUMMY VALUE FOR THE DEGREES OF FREEDOM ****;
  P=(1-PROBT(ABS(T),100000))*2;
RUN;
DATA TTEST7;
SET TTEST;
T=(RATE7-RATE5)/SQRT((RATERSE7**2)+(RATERSE5**2));
**** PLUG IN 100,000 AS A DUMMY VALUE FOR THE DEGREES OF FREEDOM ****;
  P=(1-PROBT(ABS(T),100000))*2;
RUN;
DATA TTEST6;
SET TTEST;
T=(RATE6-RATE4)/SQRT((RATERSE6**2)+(RATERSE4**2));
**** PLUG IN 100,000 AS A DUMMY VALUE FOR THE DEGREES OF FREEDOM ****;
  P=(1-PROBT(ABS(T),100000))*2;
RUN;

PROC PRINT DATA=TTEST1 (DROP=YEAR3 RATE3 RATERSE3);
PROC PRINT DATA=TTEST2 (DROP=YEAR2 RATE2 RATERSE2);
PROC PRINT DATA=TTEST3 (DROP=YEAR1 RATE1 RATERSE1); 
PROC PRINT DATA=TTEST6 (DROP=YEAR4 RATE4 RATERSE4); 
PROC PRINT DATA=TTEST7 (DROP=YEAR5 RATE5 RATERSE5); 
RUN;  
*/
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
RUN;               */  



**********************************
****** BY SEX *****
**********************************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR=0 AND SEX>0 /*AND AGEGRP=-2 AND REGION=-2 /*AND AMONTH=-2*/;
KEEP YEAR SEX WSUM SEWGT;
RUN;

PROC SORT;
BY SEX /*YEAR*/ ;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT /*YEAR,*/SEX, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY SEX/*, YEAR*/;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY SEX /*YEAR*/ ;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
BY SEX;
TITLE2 "RATE BY SEX";
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

TITLE2 "RATE BY SEX";
DATA TTEST1;
SET TTEST;
T=(RATE1-RATE2)/SQRT((RATERSE1**2)+(RATERSE2**2));
**** PLUG IN 100,000 AS A DUMMY VALUE FOR THE DEGREES OF FREEDOM ****;
  P=(1-PROBT(ABS(T),100000))*2;
RUN;

PROC PRINT DATA=TTEST1 ;

RUN;  


**********************************
****** BY SEX AND YEAR *****
**********************************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR>0 AND SEX>0 /*AND AGEGRP=-2 AND REGION=-2 /*AND AMONTH=-2*/;
KEEP YEAR SEX WSUM SEWGT;
RUN;

PROC SORT;
BY SEX YEAR ;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT YEAR,SEX, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY SEX, YEAR;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY SEX YEAR ;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
BY SEX YEAR;
TITLE2 "RATE BY SEX AND YEAR";
RUN;

**********************************
****** BY AGE ******
**********************************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR=0 AND /*SEX=-2 AND */AGEGRP>0 /*AND REGION=-2 /*AND AMONTH=-2*/;
KEEP  /*YEAR*/ AGEGRP WSUM SEWGT;
RUN;

PROC SORT;
BY AGEGRP /*YEAR*/ ;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT /*YEAR,*/AGEGRP, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY AGEGRP/*,YEAR*/;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY AGEGRP /* YEAR*/;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
BY AGEGRP;
TITLE2 "RATE BY AGE ";
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

TITLE2 "RATE BY AGEGRP";
DATA TTEST1;
SET TTEST;
T=(RATE1-RATE2)/SQRT((RATERSE1**2)+(RATERSE2**2));
**** PLUG IN 100,000 AS A DUMMY VALUE FOR THE DEGREES OF FREEDOM ****;
  P=(1-PROBT(ABS(T),100000))*2;
RUN;

PROC PRINT DATA=TTEST1 ;

RUN;  


**********************************
****** BY AGE AND YEAR ******
**********************************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR>0 AND /*SEX=-2 AND */AGEGRP>0 /*AND REGION=-2 /*AND AMONTH=-2*/;
KEEP  YEAR AGEGRP WSUM SEWGT;
RUN;

PROC SORT;
BY AGEGRP YEAR ;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT YEAR,AGEGRP, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY AGEGRP,YEAR;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY AGEGRP YEAR;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
BY AGEGRP YEAR;
TITLE2 "RATE BY AGE AND YEAR ";
RUN;

**********************************
****** BY REGION *****
**********************************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR=0 /*AND SEX=-2 AND AGEGRP=-2 */AND REGION>0 /*AND AMONTH=-2*/;
KEEP /*YEAR */REGION WSUM SEWGT;
RUN;

PROC SORT;
BY REGION /*YEAR*/;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT /*YEAR,*/REGION, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY REGION/*,YEAR*/;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY REGION /*YEAR*/;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
BY REGION;
TITLE2 "RATE BY REGION";
RUN;

**********************************
****** BY REGION AND YEAR*****
**********************************;
DATA TOTHOSPS;
SET VAR97;
IF YEAR>0 /*AND SEX=-2 AND AGEGRP=-2 */AND REGION>0 /*AND AMONTH=-2*/;
KEEP /*YEAR */REGION WSUM SEWGT;
RUN;

PROC SORT;
BY REGION YEAR;
RUN;

PROC SQL;
CREATE TABLE TOTCEN AS
SELECT YEAR,REGION, SUM(POP2) AS POP2 LABEL='CENSUS TOTAL'
FROM CENSUS
GROUP BY REGION,YEAR;

DATA ALL;
MERGE TOTHOSPS TOTCEN;
BY REGION YEAR;
%RATECI;
RUN;

PROC PRINT NOOBS LABEL;
BY REGION YEAR;
TITLE2 "RATE BY REGION AND YEAR";
RUN;


ODS RTF CLOSE;
%END;
%MEND LOOP;
%LOOP;


