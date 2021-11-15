
/*CREATING DATA MANUALLY:; */

data ex_1;
input subject gender $ CA1 CA2 HW $;
datalines;
10 m 80 84 a
7 m 85 89 a
4 f 90 86 b
20 m 82 85 b
25 f 94 94 a
14 f 88 84 c
;

proc means data=ex_1;
var CA1 CA2;
run;






/*CREATING DATA (FIXED FORMAT)  MANUALLY */


data ex_1_fix;

input subject 1-2 gender $ 3 CA1 5-6 CA2 8-9 HW $ 10;

datalines;
10m 80 84a
7 m 85 89a
14f 88 84c
;
run;




/* READING A CSV DATA FILE INTO SAS: */
/* this data file 'heats' has one column/variable with name  heat  */

FILENAME REFFILE '/home/u59061977/heats.csv'; /*directory is different for different people/*

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.heat;
	GETNAMES=YES; /* this will set the name for the variable/columns as the same as in the file*/
RUN;

PROC CONTENTS DATA=WORK.heat; RUN;

proc means data = heat;
var heat;
run;


proc print data = heat;
run;

* to get some descriptives statistics:;
* If you want to get the mean, var and 3 quartiles, min, max, can use:;
proc means data = heat mean var Q1 Median Q3 min max;
var heat;     *name of variable inside the dataset;
run;



/* create a subset from the given data set:   */

data subset_heat; /*name of the subset data that we want to create */
set heat;         /* the original data set */ 

where (heat >= 135 );  /* heat is the name of the variable in the original data */
/* "if" can be used in the place of "where" */
run;

proc print data = subset_heat;
run;





/* CREATING A PERMANENT DATA SET */

data '/home/u59061977/test';  
set subset_heat;  
/* Now we want to save the dataset 'test' as a csv file in our computer:  */

libname example '/home/u59061977';
run;

proc export data=example.test
            outfile=_dataout
            dbms=csv replace;
run;

%let _DATAOUT_MIME_TYPE=text/csv;
%let _DATAOUT_NAME=test.csv;

/* Click 'Open', then a file name test.csv has saved to your computer */
/* this file test.csv has variable 'heat' with 15 observations > 135 */







* IMPORTING DATA FROM A TEXT FILE:;

FILENAME REFFILE '/home/u59061977/ex_1_comma.txt';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.example1;
	DELIMITER=",";
	GETNAMES=NO; /*  the given dataset doesnot have any variable name */
	DATAROW=1;
RUN;

/* CHANGING VARIABLE NAMES */

data example1;
  set example1(rename=  (var1=id var2=gender var3 = CA1 var4 = CA2 var5 = Hw));
run;

/* this data has variable gender which is categorical with (M, F). We want to label it:*/
/* M is labelled as Male, F is labelled as Female */

/* To create the labels:    */
proc format;
value $gen 'F'='Female'
'M'='Male';

/* when we need this variable, we can assign the labels created above. For example, */
/* when you want to create a frequency table for variable gender, you can have it by:*/


PROC FREQ DATA=example1;
format gender $gen.; /* To assign the lables above to variable gender in data  */
TABLES gender; /* this is to create the frequency table for variable gender */
RUN;

/* The frequency table above will be different with the one below in term of labels:*/

PROC FREQ DATA=example1;
TABLES gender; /* this is to create the frequency table for variable gender */
RUN;




*******************************;

* KEEPING  a subset of variables:;
data example1_nogender; *create a new datset, named as example1_nogender;
set example1;           *the original dataset that we will use is example1;
keep id CA1 CA2 HW;     *only keep these variables from the original dataset;
run;


* DROP veriables from a dataset;
data example1_drop_id; *create a new datset, named as example1_drop_id;
set example1;          *the original dataset that we will use is example1;
drop id;               *drop this variable from the original dataset;
run;


data example1_m; /*name of the subset data that we want to create */
set example1;         
where (gender = "M" );  
/* "if" can be used in the place of "where" */
run;


data example1_f; /*name of the subset data that we want to create */
set example1;         
where (gender = "F" );  
/* "if" can be used in the place of "where" */
run;

*Concatenating two datasets example1_m and example1_f ;

data example1_concat;
set example1_m example1_f;
run;

* MERGING DATASETS: ;

proc sort data = example1_nogender;
by CA1;

proc sort data = example1_drop_id;
by CA1;

data example1_merge;
merge example1_drop_id example1_nogender;
by CA1;
run;


*   TRANSFORMING DATA:   ;

data example1_transf; 
set example1;
ave_mark= (CA1 + CA2)/2;
if HW = "." then final_grade= "";
else if ave_mark >= 90 and HW = "A" then final_grade = "A";
else if ave_mark >= 85 and (HW = "A" or HW= "B") then final_grade= "B";
else final_grade= "C";
run;

*  RANK THE SUBJECT:   ;

proc sort data=example1_transf;
by descending ave_mark;

data example1_rank; 
set example1_transf;
rank = _n_;
run;



* DOING LOOP:   ;

/* do <index variable> = <beginning number> to <ending number>;
statements;
end;
*/

data exdo;
do factora = 1 to 3;
    do factorb = 1 to 5;
        input y @ @;
        output;
    end;
end;
datalines;
22 24 16 18 19
15 21 26 16 25
14 28 21 19 24
run;




/* DATA STEPS: http://pages.stat.wisc.edu/~yandell/software/sas/data.html   */


****** Contingency table & Chi square test of independence: in the example below, we just consider/pretend that ;
*** gender and HW both are nominal variables;

PROC FREQ DATA=example1;
format gender $gen.;
TABLES gender*HW; /* this is to create the contingency table for gender and HW */
RUN;



PROC FREQ DATA=example1;
format gender $gen.;
TABLES gender*HW/chisq; /* this is to produce a chisquare test for gender and HW if applicable*/
RUN;

PROC FREQ DATA=example1;
format gender $gen.;
TABLES gender*HW/fisher; /* this is to produce Fisher exact test for gender and HW*/
RUN;

*** the link below may help to find suitable test for different dataset: ;
*** https://documentation.sas.com/?docsetId=procstat&docsetTarget=procstat_freq_syntax08.htm&docsetVersion=9.4&locale=en   ;





********   INFANT MALFORMATION dataset;

data malformation;
   input alcohol  $ mal $ Count @@;
   datalines;
0   yes 48 0 no 17066
0.5 yes 38 0.5 no 14464
1.5 yes 5  1.5 no 788
4   yes 1  4 no 126
7   yes 1  7 no 37
;

proc freq data=malformation order=data;
   tables alcohol*mal / chisq; 
   weight Count;
run;
/*the output also contain the linear-by-linear test at Mantel-Haenszel Chi-Square, but the scores are even: 1,2,3,4,5 */



proc freq data=malformation order=data;
   tables alcohol*mal / cmh; 
   weight Count;
run;
/* this is the linear-by-linear test with the scores of alcohol are even: 1,2,3,4,5 */


