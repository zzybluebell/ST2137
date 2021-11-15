*Q1;
FILENAME REFFILE '/home/u59061977/weeklies.txt';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.magazine;
	DELIMITER=",";
	GETNAMES=YES; 
	DATAROW=2;
RUN;

/* To create a new variable diff = current - lastyear  */
data magazine;
set magazine;
diff  = current - lastyear;
run;

/* Test for diff: H_0: mu = 0 against H_1: mu != 0;  */
/* the ouput of code below will include sign test and signed rank test for diff   */
/* Also test for normality and produce confidence intervals for mean of diff */

proc univariate data=magazine mu0=0 normal cipctldf cibasic;
var diff;
histogram diff /normal; *produce histogram of diff with normal curve;
qqplot /normal (mu=est sigma=est); *produce qq plot of diff;
run;
* "CIBASIC" helps to get CI of mean diff;

/* another way to get the test for diff: H_0: mu = 0 against H_1: mu != 0 as below  */
proc ttest data=magazine alpha = 0.01; *can change alpha to 0.05;
title "A paired t test";
paired current*lastyear;
run;
*can get: t-test, CI for MEAN of diff, QQ plot of diff;




*Q2;
FILENAME REFFILE '/home/u59061977/machine.txt';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.machine;
	DELIMITER="";
	GETNAMES=YES; 
	DATAROW=2;
RUN;

proc print data = machine;
run;

/* To create the labels:    */
proc format;
value $oldnew "O" = "Old" "N" = "New";

proc ttest data = machine;
format machine $oldnew.;
title "Question 2 t-test";
class machine;
var strength;
run;


* check the assumptions:;

proc univariate data=machine normal ;
format machine $oldnew.;
class machine;
var strength;
histogram strength /normal;
qqplot /normal (mu=est sigma=est);
run;


*Q3;

FILENAME REFFILE '/home/u59061977/wip.txt';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.wip;
	DELIMITER="";
	GETNAMES=YES; 
	DATAROW=2;
RUN;

proc print data = wip;
run;


proc npar1way data=wip wilcoxon;
class plant;
var time;
exact wilcoxon; * this means without continuity correction;
run;



