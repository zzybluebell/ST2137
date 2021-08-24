# Topic4_Rcode


data<- read.csv("C:/Data/midterm_marks")
data<-data.frame(data)
names(data) = c("obs", "mark")
#mark<- data[,2]

attach(data)

####  NUMERICAL SUMMARIES:

length(mark)
summary(mark)
mean(mark)
median(mark)
quantile(mark)
range(mark)


var(mark)
sd(mark)
IQR(mark)
mark[order(mark)[1:5]] # The 5 smallest observations
size<-length(mark)   #sample size = 98
mark[order(mark)[(size-4):size]] #The 5 largest observations

################   SKEWNESS:

skew <- function(x){
n <- length(x)
m3 <- mean((x-mean(x))^3)
m2 <- mean((x-mean(x))^2)
sk=m3/m2^(3/2)*sqrt(n*(n-1))/(n-2)
return(sk)
}
skew(mark)

##################  KURTOSIS:

kurt <- function(x){
n <- length(x)
m4 <- mean((x-mean(x))^4)
m2 <- mean((x-mean(x))^2)
kurt = (n-1)/((n-2)*(n-3))*((n+1)*m4/(m2^2)-3*(n-1))
return(kurt)
}
kurt(mark)

################# HISTOGRAM using graphics package

hist(mark, freq=TRUE, main = paste("Histogram of mark"),
 xlab = "mark", ylab="frequency", axes = TRUE, col = "blue")


hist(mark, freq=FALSE, nclass = 10, main = paste("Histogram of mark"),
 xlab = "mark", ylab="Probability", axes = TRUE, col = "blue")



hist(mark, freq=FALSE, main = paste("Histogram of mark"),xlab = "mark", ylab="Probability", axes = TRUE, 
     col = "grey",nclass = 10)
x <- seq(0, 30, length.out=98)
y <-dnorm(x, mean(mark), sd(mark))
lines(x, y, col = "red")


############## HISTOGRAM using ggplot2 package

library(ggplot2)
data = data.frame(mark)
attach(data)
ggplot(data) + geom_histogram(aes(x = mark),data = data,
              binwidth = 3, fill = "grey", color = "red")


#### to add a normal density curve to a histogram
p <- ggplot(data) +
    geom_histogram(aes(x = mark, y = ..density..),
                   binwidth = 3, fill = "grey", color = "black")
x <- seq(0, 30, length.out=98)
y = dnorm(x, mean(mark), sd(mark))
df <- data.frame(x = x, y = y)
p + geom_line(data = df, aes(x = x, y = y), color = "red")





#################   BOXPLOTS


boxplot(mark, xlab = "mark")


ggplot(data) + geom_boxplot(aes(y = mark))



##############  QQ plots


qqnorm(mark,datax = TRUE, ylab = "Sample Quantiels", xlab = "Theorical Quantiles", 
         main = "QQ Plot", pch = 20)
qqline(mark,datax = TRUE)



qqnorm(mark, ylab = "Sample Quantiels", xlab = "Theorical Quantiles", 
         main = "QQ Plot", pch = 20)
qqline(mark, col = "red")


ggplot(data) + geom_qq(aes(sample = mark)) + geom_qq_line(aes(sample = mark),color = "red")

ggplot(data) + geom_qq(aes(sample = mark)) + geom_qq_line(aes(sample = mark),color = "red")+ coord_flip()


################

##########################################

final<-read.csv("C:/Data/final_marks")

final <- final[,2]

midterm<-read.csv("C:/Data/midterm_marks")

midterm <- midterm[,2]

cor(final,midterm)

plot(midterm,final, pch = 20)









###################################


bats<-final<-read.csv("C:/Data/bats.csv")

bats <-data.frame(bats)

attach(bats)

boxplot(energy ~type)



##################################


# To specify that 3 graphs in one column in one page

par(mfrow=c(2,2))

#Histogram for the energy of type 1

hist(energy[which(type ==1)], include.lowest = TRUE,freq=FALSE,
col="grey",xlab = "Type 1",main ="Histograms of Energy by types")

hist(energy[which(type ==2)], include.lowest = TRUE, freq=FALSE,
col="grey", xlab = "Type 2",main = "")

hist(energy[which(type ==3)], include.lowest = TRUE, freq=FALSE,
col="grey", xlab = "Type 3",main = "")

#To get back to 1 graph in one page.

par(mfrow=c(1,1))



