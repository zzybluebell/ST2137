# Tutorial 4

set.seed(99)


rm(list=ls())

#Q1

data<- read.table("C:/Data/wip.txt",sep= " ", header=TRUE)
data

attach(data)

plant.one = time[plant==1] 

plant.two = time[plant ==2]


summary(plant.one)

IQR(plant.one)

range(plant.one)

var(plant.one) # variance

sd(plant.one) #standard deviation 

summary(plant.two)

IQR(plant.two)

range(plant.two)

var(plant.two) # variance

sd(plant.two) #standard deviation


###### histograms with density plot

#Separate histogram and boxplot:
#Plant One:


hist(plant.one, freq = FALSE, main ="Histogram of Time of Plant One")
xpoint <- seq(0 ,30,0.05)
ypoint <- dnorm(xpoint,mean(plant.one),sd(plant.one))
lines(xpoint,ypoint, col = "red")


hist(plant.one, freq = FALSE, main ="Histogram of Time of Plant One")
xpoint <- seq(min(plant.one)  ,max(plant.one),0.05)
ypoint <- dnorm(xpoint,mean(plant.one),sd(plant.one))
lines(xpoint,ypoint, col = "red")

boxplot(plant.one, xlab = "Plant One")


## plant Two:
hist(plant.two, freq = FALSE, main ="Histogram of Time of Plant Two")
xpoint <- seq(0,30,0.05)
ypoint <- dnorm(xpoint,mean(plant.two),sd(plant.two))
lines(xpoint,ypoint, col = "red")


boxplot(plant.two, xlab = "Plant Two")


### Histograms and boxplots in one figure:
par(mfrow=c(2,2))

hist(plant.one, freq = FALSE, main ="Histogram of Time of Plant One")
xpoint <- seq(0,30,0.05)
ypoint <- dnorm(seq(0,30,0.05),mean(plant.one),sd(plant.one))
lines(xpoint,ypoint, col = "red")

hist(plant.two, freq = FALSE, main ="Histogram of Time of Plant Two")
xpoint <- seq(0,30,0.05)
ypoint <- dnorm(seq(0,30,0.05),mean(plant.two),sd(plant.two))
lines(xpoint,ypoint, col = "red")

boxplot(time~plant)

par(mfrow=c(1,1))



# Q1c: Processing times for Plants 1 and 2 are quite different. 
#Plant 2 has a greater range of processing times, 
#much more dispersion among data values, a larger median, 
#a bigger value for the third quartile, and a greater extreme value than Plant 1.


#Q1d: in Python



#Q2

testscores<- read.table("C:/Data/testscores.txt",sep= " ", header=TRUE)
testscores

attach(testscores)


###########  Scatter plot with symbols and legend:
M = testscores[gender=="M", c(1:2)] #Male
F = testscores[gender=="F", c(1:2)] #Female
plot(A,B,type="n", ylab="Test B Scores", xlab="Test A Scores")
points(M$A, M$B, pch = 20)
points(F$A, F$B, pch=16,col='red')
legend(115,30,legend=c("Male", "Female"),col=c("black", "red"), pch=c(20,16), cex=1.2)



##Separate scatter plots in one figure:

par(mfrow=c(1,2)) # two plots in 1 row, 2 columns

plot(M$A, M$B, main="Plot of Two Test Scores for Males", ylab="Test B Scores", xlab="Test A Scores")
plot(F$A, F$B, main="Plot of Two Test Scores for Females", ylab="Test B Scores", xlab="Test A Scores")

par(mfrow=c(1,1))


#Comment:The overall scatter shows that there is a linear relationship between the scores of Test A and
#Test B for all the trainees. However if we look into the individual scatter plot for each gender,
#we find that there is weak relationship between the two test scores for the male group
#while there is a linear relationship between the two test scores for the female group and it seems 
#stronger than that in male group.


cor(M$A, M$B) 
cor(F$A, F$B) # stronger/larger than cor(M$A, M$B) for males


#Q3


furniture<- read.table("C:/Data/furniture.txt",sep= " ", header=TRUE)
furniture

attach(furniture)

hist(days)

mean(days)

mean(days, trim =0.2)


winsor<-function(x, alpha = 0.2) 
{ 
n = length(x)
xq = n * alpha

x = sort(x)

m = x[(round(xq)+1)]

M = x[(n - round(xq))]

x[which(x<m)] = m

x[which(x>M)] = M

return(c(mean(x),var(x)))  #return winsorized mean and variance of winsorized data
} 


winsor(days, alpha = 0.2)



#comments: the trimmed mean and Wonsorized mean are much smaller 
#than the arithmetic mean, which indicates that there is/are some large values that
#pull up the mean. Hence, the "central" location is 
#better measured by its median, or estimate by the 
#trimmed mean with alpha increased to 0.3.




sd(days)

mad(days, constant = 1) # this is the same as below
median(abs(days - median(days)))

mad(days) # note that: mad(days)/mad(days, constant = 1) = 1.4826


IQR(days)/1.35 # this is a robust estimate of sigma

#comment: the sd(days) is much larger than the estimation by MAD, and it is 
#also smaller than the estimation by IQR/1.35. This suggests: there could be a 
#large value(s) in the dataset that inflate the sd. 
#Hence, the better estimate of sigma is using MAD.

