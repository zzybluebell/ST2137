########  Q1
setwd("C:/Users/staptkc/Desktop/ST2137")
rm(list = ls())

data = read.csv("flextime.txt", sep = "", header = TRUE)

attach(data)


diff = after - before

n = length(diff)

# (a-b)
### A non-parametric test for this case 
#(though we can use t-test, since the qq plot of diff and normality test shows that diff~normal.)

wilcox.test(diff)
#test staitsic V = 51; p-value = 0.01367

# The code to calculate the test ststistic for Wilcoxon Signed Rank test above (= 51)
d = abs(diff) # absolute value of the differences
rank(d) # assign the ranks to the values in d
V.pos = sum( rank(d)[which(diff>0)]) # this is the sum of all positive ranks  = test statistic =51
V.neg = sum( rank(d)[which(diff<0)]) # this is the sum of all negative ranks = sum(rank(d)) - V.pos



########  Q2 (b)
rm(list = ls())

data = read.csv("gasoline.csv", sep = ",", header = TRUE)

attach(data)

auto = y[which(x11 == 1)]
mean(auto)


manual = y[which(x11 == 0)]
mean(manual)

###########  QQ plot:
qqnorm(auto, datax = TRUE, ylab = "Sample Quantiels", xlab = "Theorical Quantiles", 
         main = "QQ Plot of GMP of Automatic's", pch = 20)
qqline(auto, datax = TRUE, col = "red")
# for auto: quite normal

qqnorm(manual, datax = TRUE, ylab = "Sample Quantiels", xlab = "Theorical Quantiles", 
         main = "QQ Plot of GMP of Manual's", pch = 20)
qqline(manual, datax = TRUE, col = "red")
# for manual: it's not very contradict if we assume the normality

######## Normality test:

shapiro.test(auto)

shapiro.test(manual)

#both groups can approximate by normal distribution.


####### Equal variance test (F test as below, or a Bartlett test (which is suitable for normal samples) also can)

var.test(auto, manual)
# p-value is small (<0.05) hence the variances of two groups may be UN-EQUAL
bartlett.test(y~x11, data = data) # p-value = 0.01575

####### 2 independent samples t-test: (should use 1 sided for manual>auto)

t.test(manual, auto, mu = 0, var.equal = FALSE, conf.level = 0.99, alternative = "greater")

#p-value = 0.0004905, very small. Reject Ho. 
#Conclude: The GMP of vehicles using manual transmission is better than that of the automatic ones.



############  Q3 locate.text data



rm(list = ls())

data = read.csv("locate.txt", sep = "", header = TRUE)
data

attach(data)

#Q1a - ANOVA
model1=aov(sales~location)



#Q1a - NORMALITY CHECKS  = check if the residuals of ANOVA ~ normal
mcheck=function(obj){
res=obj$resid
fit=obj$fitted
par(mfrow=c(2,1))
plot(fit,res,xlab="Fitted values",ylab="Residuals", pch = 20)
abline(h=0,lty=2, col = "red")
qqnorm(res,datax = TRUE, xlab="Z scores",ylab="Residuals", pch = 20)
qqline(res,datax = TRUE, lty=2, col = "red")
}
mcheck(model1)
par(mfrow=c(1,1))


shapiro.test(model1$resid) #p-value is large, hence residuals of ANOVA ~ normal.

#Q1a - QQ plot for each group:
F = sales[which(location == "F")]
M = sales[which(location == "M")]
R = sales[which(location == "R")]

par(mfrow=c(3,1))

qqnorm(F, datax = TRUE)
qqline(F, datax = TRUE)

qqnorm(M, datax = TRUE)
qqline(M, datax = TRUE)

qqnorm(R, datax = TRUE)
qqline(R, datax = TRUE)

#all 3 qq plots are quite fine. 
par(mfrow=c(1,1))



# Q1a -  NORMALITY TEST (all 3 groups are normal, since the p-values are all large)

shapiro.test(F)
shapiro.test(M)
shapiro.test(R)
# all 3 samples are normal.

# Q1a -  EQUAL VARIANCES TEST:
bartlett.test(sales ~ location, data = data)
#p-value = 0.1162.

# Conclude: totally suitable to use ANOVA

summary(model1)
# p-value of ANOVA is 0.000524. Hence, conclude: 3 locations have different average sale.



# Q1b: can use BONFERRONI TEST or TUKEY  TEST, however, Tukey is stronger
# BONFERRONI TEST:

pairwise.t.test(sales, location, p.adj = "bonf")
# conclude: Average sale of Front is different from that of Middle; Front is also different from Rear
# Middle and Rear are not significantly different for each other.


# TUKEY  TEST:

tukey<-TukeyHSD(model1, conf = 0.95)

tukey

#To find which couple are significantly different at a = 0.05:

which(tukey$location[,4]<0.05)

# conclude: similar as the results from Bonferroni



