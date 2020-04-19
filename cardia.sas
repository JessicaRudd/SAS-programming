****************************
/* Cardia - GDM Study *****/
/* Jessica M. Rudd ********/
/* Date Created: 15Nov16 **/
****************************;

libname cardia '\\cdc.gov\private\L317\icj2\Studies';

Data cardia;
set cardia.p794;
run;

proc contents data=cardia;
run;


Data diet;
set cardia.dietpattern;
run;

proc contents data= diet;
run;

Data preg;
set cardia.y25_preg;
run;

proc contents data=preg;
run;

Data preg_keep (keep= ID gdm_y0 gdmever par0_20 par0_n par7_n par20_n y0_2gdm y2_5gdm y7_10gdm y10_15gdm y20_25gdm timegdm
y0_2birth y2_5birth y7_10birth y10_15birth y20_25birth);
set preg;
where par0_20 > 0 and gdmever ne 4;
run;

proc sort data=preg_keep;
by ID;
run;

proc freq data=preg_keep;
tables gdmever gdm_y0*par0_n y0_2gdm y2_5gdm y7_10gdm y10_15gdm y20_25gdm;
run;

/* Create outcome incident GDM variables */
Data preg_keep;
set preg_keep;
if (y0_2birth > 0 and y0_2gdm > 0) or (y2_5birth and y2_5gdm >0) then gdm_base = 1; else gdm_base=0;
if (y7_10birth > 0 and y7_10gdm >0) then gdm7_10 = 1; else gdm7_10 = 0;
if (y7_10birth > 0 and y7_10gdm >0) or (y10_15birth > 0 and y10_15gdm > 0) then gdm7_15 = 1; else gdm7_15 = 0;
if (y20_25birth > 0 and y20_25gdm > 0) then gdm20 = 1; else gdm20 = 0;

base_birth = y0_2birth + y2_5birth;
birth7_15 = y7_10birth + y10_15birth;
run;

Proc freq data=preg_keep;
tables gdm_base*base_birth gdm7_10*y7_10birth gdm7_15*birth7_15 gdm20*y20_25birth;
run;

