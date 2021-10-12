 ###   TUTORIAL 5


# Q1
student = read.csv("C:/Data/student.txt", sep = ",", header = TRUE)

student

attach(student)

#Q1(a+b)

gendergp <- ifelse(gender=="F","Female","Male") 
# for every observation in gender, if F then label as Female, else label as Male
table(gendergp)

travelgp <- ifelse(travel=="Y","Yes","No")
table(travelgp)

drivelicgp <- ifelse(drivelic=="Y","Yes","No")
table(drivelicgp)


# another way: create new column, x for gender:
x = factor(gender)
levels(x) = c("Female", "Male")


# Q1(c)

tab = table(gendergp, drivelicgp)

prop.table(tab, "gendergp")# conditional proportion on gendergrp





OR<-function(x, pad.zeros = FALSE, conf.level=0.95){
  if(pad.zeros){if(any(x==0)) {x<-x+0.5}}
  theta<-x[1,1]*x[2,2]/(x[2,1]*x[1,2])
  ASE<-sqrt(sum(1/x))
  CI<-exp(log(theta) +c(-1,1)*qnorm(0.5*(1+conf.level))*ASE)
  list(estimator=theta, ASE=ASE,conf.interval=CI, 
       conf.level=conf.level) }


OR(tab)

# The sample OR = 1.157. The 95% CI for true OR is (0.6633309 2.0185697) 
# The CI covers 1, hence the true OR might equal to 1. 
# Hence the two variable can be independent.


chisq.test(tab)

# Test statistic = 0.138. Its null distribution is 
# Chi-square with degrees of freedom = 1
# p-value of the test = 0.7099
# large p-value means data do not provide strong evidence against Ho.
# two variables (drive license and gender) might be independent





#Q1(d)
whg <- ifelse(workhour == 0, "None (0 hrs)", 
ifelse(workhour < 20, "Some (1 - 19 hrs)", "Many (20 - 99 hrs)"))




table(whg)

#Q1(e)
tab2 = table(whg, travelgp)

# For this table, it is a table that is larger than 2 x 2, hence using OR will not help.
# We need to use a test to test the association between the two variables.
# Since whg is an ordinal variable, we should use linear-by-linear association test.


test2 = chisq.test(tab2)
test2$expected
test2$stdres

#comment: we can conclude that the two variables work hour group and ever travel outside Asia 
# are independent at significance level alpha = 0.05, because p-vaue = 0.05382 > alpha.
# the standardized residuals are within the range of -3 up to 3.
# However, do note that variable whg is an ordinal variable, hence we can conduct a linear-by-linear test
# to test the association.


## Q2

samoa<- matrix(c(22,1179,22,1409), nr=2, byrow=T)
colnames(samoa) <- c("CVD", "No CVD")
rownames(samoa) <- c("Obese", "Non-Obese")
samoa

#Q2(a) 
#The conditional probability of having CVD given obese and teh probability of CVD given on-obese
#are more informative. Since they help to see: with different body condition, how it will affect the outcome 
#of CVD status.

#Q2(b)-(c): use the function OR defined in Q1.

OR(samoa)

#Q2(b): The sample OR = 1.195. This means: the odds of having CVD among obese males 
#is 1.195 times the odds of having CVD among non-obese males.
 

#Q2(c): The 95% CI for the true OR: (0.6585072, 2.1688716). This covers 1, hence 
#the CVD status and the obesity among Samoan males might be independent.

#Q2(d). This is a prospective study. The sample RR is:

RR = ( 22/(22+1179) ) / ( 22/(22 + 1409))

RR 
#the probability of having CVD among obese males is 1.1915 times the 
# probability of having CVD among non-obese males.





