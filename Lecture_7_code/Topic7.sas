* IMPORTING DATA FROM A TEXT/CSV FILE:;
FILENAME REFFILE '/folders/myfolders/babyweights.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.baby;
	DELIMITER=",";
	GETNAMES=YES; 
	DATAROW=2;
RUN;

/* To find few summaries (mean, sd...) of variable weight:  */
proc means data=baby;
var weight;
run;

/* Test H_0: mu = 3.3 against H_1: mu != 3.3;  */
/* the ouput of code below will include sign test and signed rank test also   */

proc univariate data=baby mu0=3.3;
var weight;
run;


*  ANOTHER DATASET: PROTEIN AND WEIGHT GAIN;
* IMPORTING DATA FROM A TEXT/CSV FILE:;
FILENAME REFFILE '/folders/myfolders/protein_and_weight_gain.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.weightgain;
	DELIMITER=",";
	GETNAMES=YES; 
	DATAROW=2;
RUN;

/* t test to compare means of weight gain, classified by level   */
PROC TTEST data = weightgain; *sides = L or U;
 var weight_gain;
class level;
 run;


*Perform the Mann-Whitney U Test;
proc npar1way data=weightgain wilcoxon;
class level;
var weight_gain;
*exact wilcoxon;
run;

*  CREATE DATA FOR MATH SCORE WITH/WO BREAKFAST: ;
data mathscore;
input score bf$;
datalines;
73 'no'
79 'no'
83 'no'
84 'yes'
87 'yes'
92 'yes' 
93 'no'
96 'yes'
;


proc npar1way data=MATHSCORE wilcoxon;
class bf;
var score;
exact wilcoxon;
run;





**** How to plot some summaries and figures of variable weight_gain by group:;

*Produce descriptive statistics by group/level;
proc means data=weightgain n nmiss mean std stderr median min max qrange maxdec=4;
class level;
var weight_gain;
run;

*Test for normality and produce confidence intervals on the median;
proc univariate data=weightgain normal cipctldf;
class level;
var weight_gain;
histogram weight_gain /normal;
qqplot /normal (mu=est sigma=est);
run;
 
*Produce boxplots;
proc sgplot data=weightgain;
title 'Boxplot of weight gain by level of protein';
vbox weight_gain /category=level;
run;



* PAIRED SAMPLE T-TEST;
* CREATE DATA PLATELET
data platelet;
input before after;
datalines;
25 27
25 29
27 37
44 56
30 46
67 82
53 57
53 80
52 61
60 59
28 43
;


PROC TTEST DATA=platelet;
    PAIRED after*before;
RUN;


data platelet2; /* create a new dataset name platelet2  */   
set platelet; /* the variable in dataset platelet will be used  */
diff = after - before; /* diff is created by after & before from dataset platelet  */
run;
/* dataset platelet2 will have 3 variables, while datset platelet does not change   */

proc univariate data=platelet2 mu0=0;
var diff;
run;


* CREATE DATA DRUG;
data drug;
input DrugA DrugB;
datalines;
20 18
40 36
30 32
45 46
19 15
27 22
32 29
26 25
;
data drug;    
set drug; /* the variable in dataset drug will be used  */
diff = DrugA - DrugB; /* diff is created by DrugA and DrugB */
run;
/* dataset drug now has 3 variables*/



proc univariate data=drug mu0=0;
var diff;
run;





* Good helpsheet for different tests in SAS;
* http://home.chpc.utah.edu/~u0789239/Miscellaneous/SAS/Top20stats/Top20stats_index.htm ;












 