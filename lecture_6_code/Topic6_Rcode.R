# TOPIC 6


data<-read.csv("C:/Data/bats.csv")
count = table(data$type)
count # frequency table

barplot(count)




###  CHEST PAIN EXAMPLE

chest.pain<-matrix(c(46,474,37,516), ncol=2, byrow=2)
dimnames(chest.pain)<-list(Gender=c("Male", "Female"),
                           CP=c("Yes","No"))
##2-sample test for equality of proportions without 
##continuity correction:
test<-prop.test(chest.pain,correct=FALSE)
RR<-(test$estimate[1])/(test$estimate[2])
odds<-test$estimate/(1- test$estimate)
OR<-odds[1]/odds[2];OR


#Function for finding OR and CI of OR:
OR<-function(x, pad.zeros = FALSE, conf.level=0.95){
  if(pad.zeros){if(any(x==0)) {x<-x+0.5}}
  theta<-x[1,1]*x[2,2]/(x[2,1]*x[1,2])
  ASE<-sqrt(sum(1/x))
  CI<-exp(log(theta) +c(-1,1)*qnorm(0.5*(1+conf.level))*ASE)
  list(estimator=theta, ASE=ASE,conf.interval=CI, 
       conf.level=conf.level) }
OR(chest.pain)


###############     CHI SQUARE TEST:

chisq.test(chest.pain)


chisq.test(chest.pain)$stdres   # STANDARDIZED RESIDUALS




################         FISHER EXACT TEST


claritin<-matrix(c(4,184,2,260), ncol=2, byrow=2)
fisher.test(claritin,alternative = "two.sided")


################ McNemar Test:

x = matrix(c(25,1,17,7), nrow = 2, byrow = TRUE)

mcnemar.test(x, correct = TRUE)





###################3   POLITICAL EXAMPLE


political<-matrix(c(762,327,468,484,239,477), ncol=3, byrow=2)
chisq.test(political)

chisq.test(political)$stdres





#################3   INFANT MALFORMATION EXAMPLE:

##  Using package COIN:

Input =(
"MI         Absent    Present
Alcohol                       
Zero         17066         48
Below.1      14464         38
1-2            788          5
3-5            126          1
>6              37          1
")


set = as.table(read.ftable(textConnection(Input)))

set

library(coin)
test = lbl_test(set,scores = list(MI = c(0,1), Alcohol = c(0,0.5,1.5,4,7)))

test


## MANUALLY CACULATING:



#############################
##########Infant Malformation and Mothers Alcohol Consumption

nc1<-c(17066,14464,788,126,37);## Column 1
nc2<-c(48,38,5,1,1);## Column 2

rsum<-nc1+nc2;  ## Row sums
csum<-c(sum(nc1),sum(nc2)); ## Column sums
n<-sum(csum)  ## total cell counts

rowp<-rsum/n  ## margin prob for rows
colp<-csum/n  ## margin prob for columns

pc1<-rsum*csum[1]/n;  ## prediction of Column 1
pc2<-rsum*csum[2]/n;  ## prediction of Column 2

############  Chi square test
#(this test ignores the ordinal information of variables)

X2<-sum((nc1-pc1)^2/pc1)+sum((nc2-pc2)^2/pc2)  ## Chi-squared test statistics

df<-(5-1)*(2-1) ## degrees of freedom for chi-squared test
p_value<-1-pchisq(X2,df)  




###Adjusted Residulas:
rc1<-(nc1-pc1)/sqrt(pc1*(1-rowp)*(1-colp[1]))  ## adjusted residuals for Column 1
rc2<-(nc2-pc2)/sqrt(pc2*(1-rowp)*(1-colp[2]))  ## adjusted  residuals for Column 2

rc1

rc2


########## Linear-by-Linear Association test:

v<-c(0,1);  ## specifying the scores for columns
u<-c(0,.5,1.5,4,7.0);  ## specifying the scores for rows
#u<-c(1,2,3,4,5)
#u<-c(1,3,5,7,9)



ubar=sum(u*rowp);  ## weighted average scores for rows
vbar<-sum(v*colp);  ## weighted average scores for columns
CV<-sum(c(sum((u-ubar)*nc1/n),sum((u-ubar)*nc2/n))*(v-vbar))  ## weighted covariance

V1<-sum((u-ubar)^2*rsum/n); ## weighted variance for rows' scores
V2<-sum((v-vbar)^2*csum/n);  ## weighted variance for columns' scores

r<-CV/sqrt(V1*V2)  ## weighted correlation
r

M<-sqrt(n-1)*r  ## Normalized test statistic
M

#one-sided P-value
1-pnorm(abs(M))

# Test statistic
M^2            

#Test P-value (2 sided p-value)

1-pchisq(M^2,1)
################################













