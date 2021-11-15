

*********************    Q1 *******;

FILENAME REFFILE '/home/u59061977/flextime.txt';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.time;
	DELIMITER=" ";
	GETNAMES=YES; 
	DATAROW=2;
RUN;

/* To create a new variable diff = current - lastyear  */
data time;
set time;
diff  = after - before;
run;


*  Non parametric test: Wilcoxon Signed Rank test can be derived from the output of below code;

proc univariate data=time mu0=0 ; 
var diff;
run;


****************   Q2 ****;

FILENAME REFFILE '/home/u59061977/gasoline.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.gasoline;
	DELIMITER=",";
	GETNAMES=YES; 
	DATAROW=2;
RUN;


/* t test to compare means of GMP, classified by x11   */
PROC TTEST data = gasoline sides = U; * L or U for 1 sided test, 2 for two sided test;
var y;
class x11;
run;

* Look at the test of Equality of Variances, p-value is small, hence we check for ;
* t-test at the row of 'Satterthwaite': test statistic= 4.65 & p-value = 0.0005;

* to get the 99%CI for difference mean, the we need to fit a 2 sided test with alpha = 0.01; ;
PROC TTEST data = gasoline sides = 2 alpha = 0.01; * L or U for 1 sided test, 2 for two sided test;
var y;
class x11;
run;

* 99% CI for the mean of difference (the case of UNEQUAL variance) is (3.2378, 17.3726) ;




********************  Q3, LOCATE.TEXT DATA ;

FILENAME REFFILE '/home/u59061977/locate.txt';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.locate;
	DELIMITER=" ";
	GETNAMES=YES; 
	DATAROW=2;
RUN;


*   (a) - NORMALITY CHECKS ;
*Test for normality of sales;
proc univariate data=locate normal ;
var sales;
histogram sales /normal;
qqplot /normal (mu=est sigma=est);
run;

*Test for normality of sales classified by location ;
proc univariate data=locate normal ;
var sales;
class location;  ***classified by location ;
histogram sales /normal;
qqplot /normal (mu=est sigma=est);
run;





/*             ANOVA         */

PROC ANOVA data = locate; 
class location;
model sales = location;
means location;
run;



* OR ANOVA can be derived by: ;

PROC NPAR1WAY ANOVA data = locate;
class location;
var sales;
run;

* the code below also can help to create ANOVA;
PROC GLM data=locate;
title2 'Proc glm Analysis';
/* same as 'proc anova' except 'glm' allows residual plots but gives more junk output */
class location;
model sales = location;
output  out=salefit p=yhat r=resid;
/* store fitted values and fitted residuals in dataset called 'salefit' for later use */

proc univariate   data=salefit plot normal;
var resid;
/* plot qq-plot of ANOVA's residuals and ;
/* Shapiro test of normality, which has p-value as similar as in R */

* Test for equal variances of 3 locations: Levene test ;
PROC ANOVA data = locate; 
class location;
model sales = location;
means location / hovtest = bartlett alpha=0.05;
run;
* can replace "bartlett" by "levene" ;

* (b):  BONFERRONI TEST;


PROC ANOVA data =locate;
class location;
model sales = location;
means location / Bon cldiff alpha=0.05;
run;


/* Multiple Comparisons: Tukey */

PROC ANOVA data =locate;
class location;
model sales = location;
means location / tukey cldiff alpha=0.05;
run;


