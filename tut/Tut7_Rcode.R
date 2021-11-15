
rm(list=ls())
set.seed(99)

#Q1


data<-read.table("C:\\Users\\staptkc\\Desktop\\ST2137\\midterm_marks", sep = ",", header = TRUE)

mark = data[,2]

mu0 = 20

#test statistic: 
t = (mean(mark - mu0))/sqrt(var(mark)/length(mark)) 
df = length(mark) - 1

#p-value for 2 sided test:
2*pt(t,df)

#p-value for 1 sided left: Ho: mu < mu0
pt(t,df) # 0.000533046

#p-value for 1 sided right: Ho: mu > mu0
1 - pt(t,df)



# Forming a confidence interval for population mean:

CI = c( mean(mark) - qt(0.975, df)*sqrt(var(mark)/length(mark)), mean(mark) + qt(0.975, df)*sqrt(var(mark)/length(mark)) )
CI # 16.0294 18.9706

# C

# built-in function to test:

t.test(mark)# test if mean of mark equal 0 or not

t.test(mark, mu = mu0,alternative = "two.sided") #also produces the CI for population mean


t.test(mark, mu = mu0,alternative = "less")

t.test(mark, mu = mu0,alternative = "greater")


## Checking assumptions made: data is approximately normaly distributed
# histogram
hist(mark, freq=FALSE, main = paste("Histogram of mark"),xlab = "mark", ylab="Probability", axes = TRUE, 
     col = "grey",nclass = 10)

x <- seq(0, 30, 0.05)#length.out=98)
y <- dnorm(x, mean(mark), sd(mark))
lines(x, y, col = "red")

#qq plot
qqnorm(mark,datax = TRUE, ylab = "Sample Quantiels", xlab = "Theorical Quantiles", 
       main = "QQ Plot", pch = 20)
qqline(mark,datax = TRUE)

#Since sample size is large (98), if the distribution of variable mark is 
# not normal, the result of this test above is still reliable, since 
# this test is robust the the normaility assumption.



#Q2
data<-read.table("C:\\Users\\staptkc\\Desktop\\ST2137\\glaucoma_dep.csv", sep = ",", header = TRUE)

data

attach(data)

#Q1a

shapiro.test(diff)

t.test(diff, mu = 0,alternative = "less")

#p-value = 0.04841, less than 0.05. Hence, we reject Ho, and conclude:
#glaucoma decreases the thickness of the corneal.


#Q1b
var.test(glaucoma,unaffected) # variances are equal since p-value = 0.8 is large.

t.test(glaucoma,unaffected, mu = 0, var.equal = TRUE, alternative = "less") 

# p-value = 0.3417 > 0.05. Do not reject Ho. conclude: no evidence to show 
#that glaucoma decreases the corneal's thickness.
#the conclusion is different, since the samples (glaucoma and unaffected are treated as independent.


