********   TUTORIAL 10 ;

*****************   Q1   ;


FILENAME REFFILE '/home/u59061977/batfail.txt';

PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.batfail;
	DELIMITER=" ";
	GETNAMES=YES; 
	DATAROW=2;
RUN;



* (a) ;
*      Kruskal-Wallis Test;
proc npar1way data= batfail wilcoxon dscf;
class group;
var time;
run;


PROC ANOVA data =batfail;
class group;
model time = group;
means group / Bon cldiff alpha=0.05;
run;



************* LINREAR REGRESSION***********;


FILENAME REFFILE '/home/u59061977/crab.csv';
PROC IMPORT DATAFILE=REFFILE
	DBMS=DLM
	OUT=WORK.crab;
	DELIMITER=",";
	GETNAMES=YES; 
	DATAROW=2;
RUN;


*Q1 - (a);
*Simple version:;
proc sgscatter  data = crab;
   plot width * weight
   /group = spine;
run;

* Additional information (tittle, x-label, etc) version:;
ods graphics on / attrpriority=none;
proc sgplot data=crab;
   title 'Scatter plot of Width and Weight classified by Spine condition ';
   styleattrs datasymbols=(circlefilled squarefilled starfilled);
   scatter x=width y=weight / group=spine markerattrs=(size=10px);
run;
ods graphics / reset;


* Q2 - (b): choose spine = 3 be the reference, then spine is represented by 2 indicators: s1 and s2;
data crab;
set crab;
if spine = 1 then s1 = 1;  *s1 to indicate spine = 1;
    else s1 = 0;
if spine = 2 then s2 = 1;  *s2 to indicate spine = 2;
    else s2 = 0;
run;    

*Q2 - (b): form model;
proc reg data=crab ;
  model weight = width s1 s2 /SS1; 
run;
quit;

*Q3 - (c);
* testing the significance of width;
* test statistic = 24.33~ t_169, has p-value < 0.0001, hence it is significant;

*Q4 - (d);
*to get 95% CI for coefficient, we need to set alpha and require CLB when fitting model as below;
proc reg data=crab alpha = 0.05;
  model weight = width s1 s2 /clb; 
run;
quit;
*95% CI for beta1 of width is (0.22399,0.26354);
*CLB = confidence limit for beta;


* Q5 - (e): R-Square = 0.7918;

* Q6 - (f): sigma = Root MSE = 0.26564; 
*the estimation of variance of error term, sigma^2, is = 0.26564^2;

*Q7 - (g): Scatter plot of SR vs fitted is given from the code of Q2, it's the figure of RStudent vs Predicted Values;
* When fitting a model, the vector of standardized residuals is named as "STUDENT";
* The vector of fitted values is named as "PREDICTED" or "p" ;
* can check this link for more detail: https://documentation.sas.com/doc/en/statug/15.2/statug_reg_syntax10.htm ;
proc reg data=crab;
  model weight = width s1 s2;
output out=check P=yhat STUDENT=SR;
run;
quit;
* this is to create a new dataset, called "check" which has ;
* all the data and adding another two columns of the fitted values ;
* and the standardized residuals as "yhat" and "SR"; 
* the dataset "check" is under Libraries --> Work;

proc univariate data=check normal ;
var SR;
histogram SR /normal;
qqplot /normal (mu=est sigma=est);
run;

proc sgscatter data = check;
   plot SR*yhat;
run;



*Q8 - (h): ;
* linearity assumption: met; 
*# Normaliy assumption: From the qq plot of SR, it has both right tail and left tail;
*# are heavier than normal, due to some outliers. Hence, normality assmption might be violated lightly;
*# SR vs fitted: there are 4 outliers obviously with large |SR|;
*# the residuals might have variance slightly un-constant;

*Q9 - (i) prediction for a new data point;
*step 1: add the new data point into the data;
data crab; 
set crab end=last;
output;
if last then do;
    VAR1 = . ;
    color = . ; *information for color is missed;
    spine = .; *spine variable is not needed for the prediction;
    width = 27; *width is given, here it = 27cm;
    satell =. ;
    weight =. ;
    s1 = 1 ; *spine = 1 means s1 = 1 and s2 = 0;
    s2 = 0 ; 
    output;
end;
run;
* after running, check the last line of data, line 174, to see if the new point has been added;


*Using the model to predict the weight when width = 27 cm and spine = 1, output is saved in table "predict";
proc reg data=crab alpha = 0.05;
  model weight = width s1 s2;
output out=predict(where=(weight=.)) p=predicted uclm=UCL_Pred lclm=LCL_Pred;
run;
quit;
* Use lclm - uclm: lower and upper bounds of the CI for Mean weight; 
*Check the dataset "Predict" under My Libraries --> WORK folder in the LEFT side;
* Predicted weight and 95% CI is: 2.6520425038	(2.5658029279,	2.7382820798);

* same results as in R: 2.652043 (2.565803, 2.738282);




















