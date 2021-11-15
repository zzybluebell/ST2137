#Tutorial 8

#Q1

rm(list = ls())

data = read.csv("C:/Data/weeklies.txt", sep = ",", header = TRUE)

attach(data)
data

diff = current - lastyear
n = length(diff) # sample size

# t-test for difference:
t.test(diff, mu = 0, conf.level = 0.95)

#95% CI for the difference is: (-21.41871,   1.33976).

#p-value is > 0.05. Hence, do not reject H0 at alpha = 0.05. 
#Conclude: there is no difference between current year and last year.


# paired t-test 
t.test(current, lastyear, paired = TRUE, mu = 0, conf.level = 0.95)

### this paired t-test and the t-test for diff above give the same result.




#### checking the assumption = histogram + normal curve; qq plot; and normality test:
hist(diff, freq = FALSE, col = 4, ylim = c(0,0.02))
x = seq(-60, 60, 1)
y = dnorm(x, mean(diff), sd(diff))
lines(x, y, col = "red")
# histogram looks fine, right tail might be longer/heavier than left one


###  QQ plot of the differences:
qqnorm(diff, datax = TRUE,ylab = "Sample Quantiels", xlab = "Theorical Quantiles", 
         main = "QQ Plot of Differences", pch = 20)
qqline(diff,  datax = TRUE, col = "red")
## QQ plot looks fine

###Normality test for the differences:

shapiro.test(diff)
#large p-value, hence diff ~normal distribution


### We can try a non-parametric test for this case (though not nesessary)
wilcox.test(diff)


##### Manually form a CI for the difference: D-bar +- t_(n-1)(0.025) *SE(D-bar)

alpha = 0.01 # for 99% CI. Use alpha = 0.05 for a 95% CI.

CI = c( mean(diff) - qt((1-alpha/2), (n-1))*sd(diff)/sqrt(n), mean(diff) + qt((1-alpha/2), (n-1))*sd(diff)/sqrt(n) )
CI

# the 99% CI is wider, since the value of t_(n-1)(alpha/2) which = qt((1-alpha/2), (n-1)) is
#larger when alpha is smaller. That value larger will make the margin of error larger hence the CI is wider.

#if alpha = 0.05, then t_(n-1)(alpha/2) = t_{18, 0.975}, or t_18(0.025)
#if alpha = 0.01, then t_(n-1)(alpha/2) = t_{18, 0.995}, or t_18(0.005)






#Q2

rm(list=ls())

data = read.table("C:/Data/machine.txt", sep = "", header = TRUE)

data

attach(data)


old <- strength[machine=="O"]

new <- strength[machine=="N"]

# Test if the variances are equal: We need to check the equal variance assumption to decide which t-test to use. 
# According to the equal variance test, the p-value is 0.4268 so we conclude that the equal variance assumption holds.


var.test(new,old) #p-value is 0.4268, large, --> equal variances

bartlett.test(strength ~ machine, data = data)
# p-value = 0.4268



# We use the t test with equal variance 

t.test(new, old, mu=0, alternative="greater", var.equal=TRUE) #test Ho: New = Old vs H1:  New >Old.

#we reject the null hypothesis as the p-value is 0.000079. 
#Conclude: the new machine is better than the old ones.

# Assumptions: normality of each sample. Equal variances (checked).

# Normality assumption checking for each sample 
#(should have histogram + normal curve; qq plot; and Shapiro Wilk test)

qqnorm(old)
qqline(old)

qqnorm(new)
qqline(new) #not very normal, but sample size is quite large (50).

shapiro.test(old)
shapiro.test(new)

#Both the plots and the ShapiroWilk tests for Old and New show that the normaility assumption is met or acceptable..




#Q3

rm(list= ls())

data = read.table("C:/Data/wip.txt", header = TRUE)

data

attach(data)


plant.one = time[plant==1] 

plant.two = time[plant ==2]

#these are 2 independent samples.
#Wilcoxon rank sum test to determine if there is a difference between the processing times of the two plants.

wilcox.test(plant.one,plant.two)

#There is not very strong evidence to show that the processing times for the two plants are different
# since the two sided p-value is not very small (0.0834).





