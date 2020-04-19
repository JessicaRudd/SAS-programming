
/* 
/ Program   : NEDS_HFM_Rates
/ Version   : 2.0 
/ Author    : Jessica M. Rudd 
/ Date      : 21 May 2015
/ Contact   : icj2@cdc.gov
/ Purpose   : Determine rates of hand, foot and mouth disease visits in children less than 5 years old from NEDS database
/ SubMacro	: 
/ Notes     : Please note that for NEDS data, CT and FL suppressed admission month.
/ Usage     : 
/ 
/=============================================================================== 
/ PARAMETERS: 
/-------name------- -------------------------description------------------------ 
 
/=============================================================================== 
/ AMENDMENT HISTORY: 
	JMM			02/17/2012				added 2009 data
	JMR			08/16/2016				added data through 2012
/=============================================================================*/


LIBNAME NEDS '\\cdc\project\NCIRD_DVD_EB_DATA_1\hcup\neds\Data';
LIBNAME HFM '\\cdc.gov\private\L317\icj2\Studies\HFM';                                               
 
/* Create formats for categorial variables created to simplify analysis*/

Proc format;
	/*value agegrp 1="LT 1" 2="1-4" 3="5-19" 4="20-54" 5="55-64" 6="65-74" 7="75-84"
             8=">=85";*/

	VALUE SEX 1='MALE' 2='FEMALE';

	VALUE AGEGRP 1='LT 1' 2='1-4' 3='5-17' 4='18-44' 5='45-64' 6='GE 65';
	
	VALUE REGION 1='NORTHEAST' 2='MIDWEST' 3='SOUTH' 4='WEST';

	/*VALUE COMPYR 1='2006' 2='2007' 3='2008';*/

	VALUE MONTHF 1='JAN' 2='FEB' 3='MAR' 4='APR' 5='MAY' 6='JUN' 7='JUL' 8='AUG'
				 9='SEP' 10='OCT' 11='NOV' 12='DEC';

	VALUE PAY 1='MEDICARE' 2='MEDICAID' 3='PRIVATE INSURANCE' 4='SELF-PAY'
			  5='NO CHARGE' 6='OTHER';
Run;
%LET DENOM=100000; *** Denominator for rates is 100,000 person visits***;


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
quit;

DATA HOSPWTS;
SET HOSP2006 HOSP2007 HOSP2008 HOSP2009 HOSP2010 HOSP2011 HOSP2012;
RUN; 


options bufno=500 bufsize=32k;
DATA HFM_NEDS_ANALYSIS(sgio=yes) ;
MERGE HFM.HFM_06_12 (IN=C sgio=yes DROP=KEY_ED AWEEKEND DQTR CHRON1-CHRON15  
	  INTENT_SELF_HARM DXCCS1-DXCCS15 E_CCS1-E_CCS4 I NEDS_STRATUM)
      HOSPWTS (IN=B RENAME=(HOSP_REGION=REGION));
BY YEAR HOSP_ED;
/*IF AMONTH=. THEN AMONTH=1+(12-1)*RANUNI(0);*/
*this adds dummy records for any strata that might not be captured in the subset;
IF B=1 AND (C NE 1) THEN DO; 
 HFM=0;
 DISCWT=0.00000001;
 SEX=2; AGE=0; REGION=1;AMONTH=3;
END;
NEW_STRAT=NEDS_STRATUM||'-'||LEFT(YEAR);
NEW_STRAT2=YEAR*10000000+NEDS_STRATUM;
IF YEAR=2006 THEN COMPYR=1;
ELSE IF YEAR=2007 THEN COMPYR=2;
ELSE IF YEAR=2008 THEN COMPYR=3;
ELSE IF YEAR=2009 THEN COMPYR=4;
ELSE IF YEAR=2010 THEN COMPYR=5;
ELSE IF YEAR=2011 THEN COMPYR=6;
ELSE IF YEAR=2012 THEN COMPYR=7;
IF Age IN(0,1,2,3,4) THEN OUTPUT;	
FORMAT YEAR 5. SEX SEX. REGION REGION. PAY1 PAY. PAY2 PAY. AMONTH MONTHF.;
RUN;

DATA HFM_ANALYSIS;
SET HFM_NEDS_ANALYSIS;
IF AGE=0 THEN AGEGRP=1;else
IF AGE in (1,2,3,4) THEN AGEGRP=2;
/*IF AGE GT 4 AND AGE LE 17 THEN AGEGRP=3;
IF AGE GT 17 AND AGE LE 44 THEN AGEGRP=4;
IF AGE GT 44 AND AGE LE 64 THEN AGEGRP=5;
IF AGE GT 64 THEN AGEGRP=6;*/
FORMAT AGEGRP AGEGRP.;
RUN;

PROC SORT DATA=HFM_ANALYSIS;
BY NEW_STRAT2 HOSP_ED;
RUN;

PROC FREQ DATA=HFM_ANALYSIS;
where hfm=1;
TABLES YEAR AGE AGEGRP SEX REGION AMONTH HFM HFM_F;
RUN;

DATA HFM.HFM_NEDS_ANALYSIS;
SET HFM_ANALYSIS;
RUN;


*** Calculate total charges, cost, length of stay ***;
ods rtf file="\\cdc.gov\private\L317\icj2\Studies\HFM\HFMD_ED_demographics_&sysdate..rtf" bodytitle sasdate style=rtf;
title 'Weighted total charges for HFMD ED visits';
proc surveymeans data=hfm_analysis quartiles sum;
class year ;
cluster hosp_ed;
strata new_Strat2;
var totchg_ed age;
weight discwt;
domain year;
run;

title 'Weighted frequencies of HFMD ED visits';
proc surveyfreq data=hfm_analysis;
tables agegrp region sex amonth year*(agegrp amonth region sex);
cluster hosp_ed;
strata new_strat2;
weight discwt;
run;
ods rtf close;

proc print data=hfm_analysis;
where discwt=.;
run;

*** Calculate total charges ***;
proc descript data=hfm_analysis filetype = sas design=wr notsorted;
	weight DISCWT;
	nest neds_stratum hosp_ed;
	setenv colwidth = 24 decwidth =5;
	var totchg_ed ;
	subgroup HFM year;
	levels 2 7;
	print total setotal mean semean;
	rtitle "Mean total charges of HFM inpatient visits";
run;

**** Comparing years, regions and age groups using multiple comparison tests ***;
ODS PDF FILE="\\cdc.gov\private\L317\icj2\HCUP\NEDS\HFM_comparisons_&sysdate..pdf";

title "Multiple comparison tests of years";
proc freq data=hfm_analysis;
where hfm=1;
	tables year/chisq;
run;

title "Multiple comparison tests of regions";
proc freq data=hfm_analysis;
where hfm=1;
	tables region/chisq;
run;

title "Multiple comparison tests of age groups";
proc freq data=hfm_analysis;
where hfm=1 and agegrp le 2;
	tables agegrp/chisq;
run;

title "Multiple comparison tests of sex";
proc freq data=hfm_analysis;
where hfm=1;
	tables sex/chisq;
run;

title "Requesting Multiple Comparison Tests - Year";
proc glm data=hfm_analysis;
	class year;
	model HFM=year/ss3;
	means year / snk;
	lsmeans year/pdiff adjust=tukey;
run;
quit;

title "Requesting Multiple Comparison Tests - Region";
proc glm data=hfm_analysis;
	class region;
	model HFM=region/ss3;
	means region / snk;
	lsmeans region/pdiff adjust=tukey;
run;
quit;

title "Requesting Multiple Comparison Tests - Age Group";
proc glm data=hfm_analysis;
	class agegrp;
	model HFM=agegrp/ss3;
	means agegrp / snk;
	lsmeans agegrp/pdiff adjust=tukey;
run;
quit;

title "Requesting Multiple Comparison Tests - Sex";
proc glm data=hfm_analysis;
	class sex;
	model HFM=sex/ss3;
	means sex /snk;
	lsmeans sex/pdiff adjust=tukey;
run;
quit;
ods pdf close;

/*title "Effect of year";
proc logistic data=hfm_analysis;
	class year ;
	model hfm = year /expb;
run;

title "Effect of year and age group";
proc logistic data=hfm_analysis;
	class year agegrp;
	model hfm = agegrp year*agegrp /expb;
run;

title "Effect of year and sex";
proc logistic data=hfm_analysis;
	class year sex;
	model hfm = sex year*sex /expb;
run;

title "Effect of year and region";
proc logistic data=hfm_analysis;
	class year region;
	model hfm = region year*region /expb;
run;*/

*** Determine top 5 accompnaying diagnoses ***;
***All-coded HFMD ***;
%Macro diagnosis;
 %LOCAL I;
 %DO I=1 %TO 15;

 DATA DX&I (KEEP=HOSPID hfm DX);
 	SET HFM_ANALYSIS;
	IF DX&I NE ' ' THEN DX=DX&I;
	FORMAT DX1-DX15 $I9DXF.
 RUN;

 %END;
%MEND DIAGNOSIS;
%DIAGNOSIS;
QUIT;

DATA DX;
SET DX1 DX2 DX3 DX4 DX5 DX6 DX7 DX8 DX9 DX10 DX11 DX12 DX13 DX14 DX15;
RUN;

PROC FREQ DATA=DX;
TABLES DX/noprint out=byvalue;
where hfm=1 and DX ne '';
format dx $I9DXF.
RUN;
proc sort data=byvalue out=bycount;
by descending count;
run;
proc print data=bycount (obs=10) ;
run;

*** Primary coded HFMD***;

%Macro diagnosis;
 %LOCAL I;
 %DO I=1 %TO 15;

 DATA DX&I (KEEP=HOSPID hfm_f DX);
 	SET HFM_ANALYSIS;
	IF DX&I NE ' ' THEN DX=DX&I;
	format dx $I9DXF.
 RUN;

 %END;
%MEND DIAGNOSIS;
%DIAGNOSIS;
QUIT;

DATA DX;
SET DX1 DX2 DX3 DX4 DX5 DX6 DX7 DX8 DX9 DX10 DX11 DX12 DX13 DX14 DX15;
RUN;

PROC FREQ DATA=DX;
TABLES DX/noprint out=byvalue;
where hfm_f=1 and DX ne '';
format dx $I9DXF.
RUN;
proc sort data=byvalue out=bycount;
by descending count;
run;
proc print data=bycount (obs=10) ;
run;

***************************************************************
****** READS IN THE US CENSUS DATA FOR RATE CALCULATIONS ******
***************************************************************;

LIBNAME CEN '\\cdc\project\NCIRD_DVD_EB_DATA_1\census\bridged_race\data';

DATA CEN;                                                                       
  SET CEN.br90_13_B (KEEP=YEAR SEX AGE POP STATE);                       
IF 2006<=YEAR<=2012;                                                      
DIED=2; 
         
FORMAT SEX SEX.;                                                          
                                               
*******DEFINES COMPARISON YEARS*********;
/*IF YEAR=2006 THEN COMPYR=1;
ELSE IF YEAR=2007 THEN COMPYR=2;
ELSE IF YEAR=2008 THEN COMPYR=3;
FORMAT COMPYR COMPYR.;*/ 
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


data hfm_analysis;
set neds.hfm_analysis;
if agegrp ^in (1,2) then delete;
run;

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
PROC CROSSTAB DATA=hfm_analysis FILETYPE=SAS DESIGN=WR;
	WEIGHT DISCWT;
	SUBPOPN &TI=1;
	NEST NEW_STRAT2 HOSP_ED/ MISSUNIT;
	CLASS YEAR SEX AGEGRP REGION /INCLUDE=MISSING;
   	TABLES SEX AGEGRP REGION year YEAR*(SEX AGEGRP REGION);
  	SETENV LEFTMGN=1;*COLWIDTH=10 DECWIDTH=0;   
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
IF YEAR=0 AND SEX=-2 AND AGEGRP=0 AND REGION=-2 /*AND AMONTH=-2*/;
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
IF YEAR>0 AND SEX=0 AND AGEGRP=-2 AND REGION=-2 /*AND AMONTH=-2*/;
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


/*PROC SQL;                                                                       
CREATE VIEW T1 AS                                                               
 SELECT YEAR AS YEAR1, RATE AS RATE1, RATERSE1                           
 FROM ALL                                                                      
 WHERE YEAR=2006 AND AGER=1;                                                                
CREATE VIEW T2 AS                                                               
SELECT YEAR AS YEAR2, RATE AS RATE2, RATERSE1 AS RATERSE2                
 FROM ALL                                                                      
 WHERE YEAR=2007 AND AGER=1;   
CREATE VIEW T3 AS                                                               
SELECT YEAR AS YEAR3, RATE AS RATE3, RATERSE1 AS RATERSE3                
 FROM ALL                                                                      
 WHERE YEAR=2008 AND AGER=1; 
CREATE TABLE TTEST AS                                                           
 SELECT T1.*, T2.*, T3.*                                                              
FROM T1, T2, T3;  

TITLE2 "RATE BY YEAR & AGE < 1 YEAR";
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

PROC PRINT DATA=TTEST1 (DROP=YEAR3 RATE3 RATERSE3);
PROC PRINT DATA=TTEST2 (DROP=YEAR2 RATE2 RATERSE2);
PROC PRINT DATA=TTEST3 (DROP=YEAR1 RATE1 RATERSE1); 
RUN;  
*/


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


ODS RTF CLOSE;
%END;
%MEND LOOP;
%LOOP;
