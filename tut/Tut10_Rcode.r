# TUTORIAL 10

set.seed(999)

###########################   Q1 

rm(list = ls())

data = read.csv("C:/Data/batfail.txt", sep = "", header = TRUE)
data

attach(data)

####  KRUSKALL-WALLIS TEST

kruskal.test(time~group)

#### Bonferroni to check which group is different from others:

pairwise.wilcox.test(time, group, p.adjust = "bonf")

#should not use pairwise.t.test(time, group, p.adj = "bonf")
#because each group is not normal, hence should not use t-test for pairwise comparison


############### Q2 LINEAR REGRESSION

rm(list=ls())

set.seed(999)


data<-read.table('C:/Data/crab.txt', header=T)

names(data)

attach(data)

data$spine = as.factor(spine)

#Q1 - (a)
# scatterplot of width ~ weight classified by spine
plot(width,weight, type = "n")
points(width[which(spine==1)],weight[which(spine==1)],pch = 20)
points(width[which(spine==2)],weight[which(spine==2)],pch = 6, col = "red")
points(width[which(spine==3)],weight[which(spine==3)],pch = 10, col= "blue")

#Q2 - (b) 
#Model 1: We choose the indicator for spine where spine = 3 is the reference.
s1 = c(rep(0,173))
s2 = c(rep(0,173))
s1[which(spine==1)] = 1
s2[which(spine==2)] = 1

data$s1 = s1
data$s2 = s2

attach(data)

model1 = lm(weight ~ width + s1 + s2, data = data)

summary(model1)
#Multiple R-squared:  0.7918,    Adjusted R-squared:  0.7881
# s1 ans s2 has SS_R = 0.099 + 0.2 = 0.299 with df = 1 + 1 = 2.



#Q2 - (b)
#Model 2: let R choose the indicator for variable spine
model2 = lm(weight ~ width + spine, data = data)

anova(model2)
#spine has SS_R = 0.299 with df = 2 because it has 2 coefficients in the model


#Model 1 and Model 2 have the same summary statistics of R2 and adjusted R2.
# the interpretation for variable spine should be similar



#Q3-4 - (c+d)
confint(model1, "width", level=0.95)
#the 95% CI for beta1 is (0.2239882, 0.2635374), which does not cover 0, hence
# variable width is significant.
# OR: the p-value of variable width is <2e-16, which is significant.

#Q5 - (e) 
R2 = summary(model1)$r.squared # = 0.7917598

#Q6 - (f)
sigma = summary(model1)$sigma # = 0.2656428
var = sigma^2 #this is the estimation of variance of error term.

#Q7 - (g)
SR = rstandard(model1)

#QQ plot of SR:
qqnorm(SR,datax = TRUE, ylab = "Standardized Residuals", xlab = "Z scores", main = "QQ Plot", pch = 20)
qqline(SR,datax = TRUE)



#standardized residuals vs fitted:
plot(model1$fitted.values,SR, xlab="fitted weight", ylab= "Standardized Residuals", main = "SR vs Fitted", pch = 20)
abline(h=0)

#Q8 - (h) 
#Normaliy assumption: From the qq plot of SR, it has both right tail and left tail
# are heavier than normal, due to some outliers. Hence, normality assmption might be violated.
#SR vs fitted: there are 4 outliers obviously with large |SR|.
#the residuals might have variance slightly un-constant.

#Q9 - (i)
newpoint=data.frame(width = 27,s1 = 1, s2 = 0)
# the 95% CI for the mean weight is:
predict(model1, newpoint, interval = "confidence", conf = 0.95) 



