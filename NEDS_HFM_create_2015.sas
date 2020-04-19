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

%Macro loop;
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

   
/* Create a variable indicating whether any diagnosis code includes Intussusception*/

%Macro loop;
 %LOCAL I;
 %DO I=2006 %TO 2012;

Data HFM_&I (compress=binary);
set NEDS_&I._CORE;
ARRAY DIAGNOSIS (15) $ DX1-DX15;
DO i=1 to 15;
	IF SUBSTR(DIAGNOSIS(i),1,4) IN('0743')THEN DO;
  		HFM=1;
  		IF I=1 THEN HFM_F=1; ELSE HFM_F=0;
		END;
	IF SUBSTR(DIAGNOSIS(i),1,3) IN ('074') THEN DO;
		HFM_HERP=1; 
	IF I=1 THEN HFM_HERP_F=1; ELSE HFM_HERP_F=0;
		END;
END;

	IF FEMALE=0 THEN SEX=1;else;
IF FEMALE=1 THEN SEX=2;
ELSE IF FEMALE=. THEN SEX=(int(2*ranuni(35346)) + 1);
RUN;
%END;
%MEND LOOP;
%LOOP;
QUIT;

DATA HFM_06_12;
SET HFM_2006 HFM_2007 HFM_2008 HFM_2009 HFM_2010 HFM_2011 HFM_2012;
IF HFM=1 THEN OUTPUT;
RUN;

PROC FREQ DATA=HFM_06_12;
	TABLES HFM HFM_F SEX ;
	By Year;
		%let title=%str(Frequency of HFM);
		ods proclabel "NEDS &title";
RUN;



DATA NEDS.HFM_06_12;
SET HFM_06_12;
RUN;


