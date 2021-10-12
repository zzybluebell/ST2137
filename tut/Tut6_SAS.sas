

/*    Q2   */

proc format;
value $candid "A" = "Candidate A" "B" = "Candidate B";

data debate;
input gender $ before $ after $ count; /*all 3 variables: gender, after, before are categorical */
label before = "Preference before TV debate"
after = "Preference after TV debate";
format before $candid.
after $candid.;
datalines;
M A A 67
M A B 28
M B A 46
M B B 54
F A A 58
F A B 42
F B A 37
F B B 61
;

proc sort data = debate;
by gender;
run;


proc freq data=debate;
by gender;  /* create tables, tests for different gender  */
tables before*after/agree; /*For 2x2 tables, this option "agree" provides McNemar’s test, no correction*/
weight count;
title "Chi-square test for the paired samples";
run;
















