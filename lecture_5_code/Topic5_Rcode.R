#  TOPIC 5


winsor<-function(x, alpha = 0.2) 
{ 
n = length(x)
xq = n * alpha

x = sort(x)

m = x[(round(xq)+1)]

M = x[(n - round(xq))]

x[which(x<m)] = m

x[which(x>M)] = M

return(c(mean(x),var(x))) 
} 




data<-read.csv("C:\\Users\\staptkc\\Desktop\\ST2137\\heats.csv")
x = data$heat

mean(x)

mean(x, trim = 0.2)

winsor(x, alpha = 0.2) 




###MAD:

median(abs(x - median(x))) #MAD

mad(x) # estimate of \sigma, = 1.4826*MAD

### IQR

IQR(x)




####
library(MASS)

hubers(x, k= 0.84) # this gives the 20% Winsorized mean


























