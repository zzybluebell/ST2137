
rm(list=ls())


data<- read.table("C:/Data/gasoline.csv",sep= ",", header=TRUE)
data

attach(data)

###########  QQ plot:
qqnorm(data$y, ylab = "Sample Quantiels", xlab = "Theorical Quantiles", 
         main = "QQ Plot of Gansoline Mileage", pch = 20)
qqline(data$y, col = "red")



###############  Histogram & Normal Density plot:

hist(data$y, freq = FALSE, main ="Histogram of Gasoline Mileage")

x = seq( (min(data$y)- 5), (max(data$y) +5), length.out = length(data$y))

yy <- with(data, dnorm(x, mean(data$y), sd(data$y)))
lines(x, yy, col = "red")


######  Boxplot:

boxplot(y, xlab = "Gasoline mileage")

#################  Separate observations by group

auto = data$y[which(x11 == 1)]
length(auto)
range(auto)



manual = data$y[which(x11 == 0)]
length(manual)
range(manual)

########################  Boxplots by group



data$x11 = factor(x11)

boxplot(data$y ~ x11, xlab = "Type of transmission", ylab = "Gasoline mileage performance" )



######################  Scatter plot
#gasoline mileage performance vs the overall length
plot(data$x8,data$y, main = "Gasoline Mileage vs Overall Length", xlab = "Overall Length", 
ylab = "Gasoline Mileage", pch = 20)


#gasoline mileage performance vs the width:
plot(data$x9,data$y, main = "Gasoline Mileage vs Width", xlab = "Width", 
ylab = "Gasoline Mileage", pch = 20)


################  Scatter plot by group

### y vs weight


plot(data$y[which(x11 == 0)]~data$x10[which(x11 == 0)],pch = 20,col="red",xlim=c(1800,5500),ylim=c(10,40), 
xlab="Weight",ylab="gasoline mileage", main = "Gasoline Data")

par(new=T) #this is to add more scatter plot onto the current plot above
plot(data$y[which(x11 == 1)]~data$x10[which(x11 == 1)],pch = 2,col="blue", xlim=c(1800,5500),ylim=c(10,40), 
xlab="",ylab="", main = "")
par(new=F) #this is to finish adding figure to the current one above
legend(4500,35,legend=c("Manual", "Auto"),col=c("red", "blue"), pch=c(20,2), cex=1.2)



############   ANOTHER WAY



plot(data$y[which(x11 == 0)]~data$x10[which(x11 == 0)], type = "n",xlim=c(1800,5500),ylim=c(10,40), 
xlab="Weight",ylab="gasoline mileage", main = "Gasoline Data")

points(data$x10[which(x11 == 1)],data$y[which(x11 == 1)],pch = 2,col="blue")
points(data$x10[which(x11 == 0)],data$y[which(x11 == 0)], pch = 20,col="red")
legend(4500,35,legend=c("Manual", "Auto"),col=c("red", "blue"), pch=c(20,2), cex=1.2)














