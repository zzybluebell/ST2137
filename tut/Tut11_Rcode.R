# TUTORIAL 11


########  SIMULATION FOR CLT


set.seed(999)

lambda = 5000  

f = function(lambda = 5000,n,N){

ave = numeric(N)

for (i in 1:N){ 

x = rexp(n, rate = 1/lambda)

ave[i] = mean(x)
}
return(ave)
}


# (a)
ave = f(n = 30, N = 100); ave
s = sd(ave); s
# by CLT, if n is large, this sd(ave) should be close to lambda/sqrt(n)


# (b)
hist(ave); #OR more advanced as below

library(ggplot2)

data = data.frame(ave)
p <- ggplot(data) +
    geom_histogram(aes(x = ave, y = ..density..),
                   binwidth = 500, fill = "grey", color = "black")

x <- seq( (min(ave)-500), (max(ave)+500), length.out = N) 
y = dnorm(x, mean(ave), sd(ave))
df <- with(data, data.frame(x = x, y = y))
p + geom_line(data = data, aes(x = x, y = y), color = "red")

shapiro.test(ave) 

# use the rule of thumb to check is the histogram is within mean(ave) +- 3*sd(ave):
c( min(ave),max(ave))

mean(ave) + 3*sd(ave) # this should be the end of the right tail

mean(ave) - 3*sd(ave) # this should be the end of the left tail

#the interval of mean +- 3sd should cover 99.9% of data

c(mean(ave) - 2*sd(ave), mean(ave) + 2*sd(ave)) # 2 sd from the mean

less = which(ave<mean(ave) - 2*sd(ave))

more = which(ave>mean(ave) + 2*sd(ave))

length(more) + length(less) # count how many values that are more than 2sd from the mean
# the count should be 5% of points: ~= 0.05*N


# Q(c)
ave = f( n= 100, N = 1000)
s = sd(ave)# lambda/sqrt(100)

hist(ave, freq= FALSE)
x = seq(min(ave), max(ave), 1)
y = dnorm(x, mean(ave), s)
lines(x,y, col = "red")

shapiro.test(ave) 
# the histogram is well bell-shaped, resemble a normal distribution well.
# can check the rule of thumb as above.
# compared to (b) when n = 30, then this one is BETTER normally distributed.
# Question: it's because of the increase in N or in n? 
# to answer, we'll try Qd where N = 1000 again but n is dropped to 7.


# Q(d)
ave = f( n= 7, N = 1000)
s = sd(ave)

hist(ave, freq= FALSE)
x = seq(min(ave), max(ave), 1)
y = dnorm(x, mean(ave), s)
lines(x,y, col = "red")
# the histogram is skewed, not symmetric.
# compared to (c) when N = 1000, 
# when dropping n = 100 to n = 7, the histogram of sample means is not normal.
shapiro.test(ave) # small p-value, suggest not normal


# Q(e)
ave = f( n= 100, N = 50)
s = sd(ave)

hist(ave, freq= FALSE, ylim = c(0,0.001) )
x = seq(min(ave)-400, max(ave)+400, 1)
y = dnorm(x, mean(ave), s)
lines(x,y, col = "red")
shapiro.test(ave) 
# # the histogram should look normal, 
# if it does not look very normal, that is because of the number of samples (50) is not very large.
# It is similar as: from all the values of sample means, you have only 50 values of them, hence histogram
# might not show clearly the distribution of population of sample means. 





