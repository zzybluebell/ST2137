* IMPORTING DATA FROM A TEXT/CSV FILE:;
FILENAME REFFILE '/home/u59061977/midterm_marks';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.datamark;
	DELIMITER=",";
	GETNAMES=YES; 
	DATAROW=2;
RUN;

proc print data = datamark;
run;

* change variable name;
data datamark;
  set datamark(rename=  (var1=id x = mark));
run;

*Qa;
* to get mean and SD of variable mark:;
proc means data = datamark;
var mark;
run; 

* If you want to get the 3 quartiles, can use:;
proc means data = datamark Q1 Median Q3;
var mark;
run;

* Create a table of summaries for variable mark;
proc means data=datamark stackods;
ods output summary=summaries;
run;

* Save the table of summaries as a new dataset with name "summaries" so in the next questions we can use;
proc print data=summaries;
run;


*another way to save the summaries as another dataset;
ods exclude all;                  /* suppress display to open ODS destinations */
proc means data= datamark 
           N Mean Std Min Q1 MEDIAN Q3 Max  /* type every statistic you want */
           STACKODSOUTPUT;        /* preserve table form of output */
ods output Summary=MeansSummary;  /* write statistics to data set */
run;
ods exclude none;                 /* restore normal display of tables */
*The data set MeansSummary contains the statistics for every numerical variable in the original dataset datamark;


*Qb +Qc  ;

data new;
set summaries;
if Variable="mark" then Ttest = (Mean - 20)/(StdDev/sqrt(N));
if Variable="mark" then pvalue =cdf('T',Ttest,(N-1));
if Variable="mark" then lower = Mean - quantile('T', 0.975, (N-1))*(StdDev/sqrt(N));
if Variable="mark" then upper = Mean + quantile('T', 0.975, (N-1))*(StdDev/sqrt(N));
put Ttest=;
put pvalue =;
put lower=;
put upper=;
run;

* Qd;
* 2 sided test which can provides CI for mean;
proc univariate data=datamark mu0=20 CIBASIC;
var mark;
run;

*this way below can allow to choose 1 sided test, which provides 1-sided confidence interval;
PROC TTEST data = datamark alpha = 0.05 H0 = 20 sides = L;
var mark;
run; 



*Test for normality ;
proc univariate data=datamark normal ;
var mark;
histogram mark /normal;
qqplot /normal (mu=est sigma=est);
run;



*/ Glaucoma dataset/*

* IMPORTING DATA FROM A TEXT/CSV FILE:;
FILENAME REFFILE '/home/u59061977/glaucoma_dep.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.glaucoma;
	DELIMITER=",";
	GETNAMES=YES; 
	DATAROW=2;
RUN;

proc print data = glaucoma;
run;

*Qa;
proc univariate data=glaucoma mu0=0;
var diff;
run;

*Qb;
* We may transform the original data into a new set of data as 'glaucoma_ind.csv';

FILENAME REFFILE '/home/u59061977/glaucoma_ind.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.glaucoma2;
	DELIMITER=",";
	GETNAMES=YES; 
	DATAROW=2;
RUN;


*test equal variances by Levene test:;
PROC ANOVA data =glaucoma2;
class group;
model thickness = group;
means group / hovtest = levene alpha=0.05;
run;


*t test:;
PROC TTEST data = glaucoma2;
 var thickness;
class group;
 run;
 
*this t test above has the test for equal variance which has test statistic F = 1.19 and p-value = 0.8215;
*that test of equal variance is the same as the result of var.test() in R;




































 