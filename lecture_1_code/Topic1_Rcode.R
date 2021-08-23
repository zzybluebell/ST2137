
############  MATRIX AND WORKING WITH MATRICES

m = matrix(c(1:4), nrow = 2)

m[1,2]

m[1,2] = 5

solve(m)


m[1,]

m[, 2]

t(m)

n = matrix(c(5:8), nrow = 2)


m+n

m%*%n


diag(2)




a = c(1:4)

b = c(5:8)

cbind(a,b)

rbind(a,b)

r = rbind(a,b,c(6:9))

r[-3,]

r[,-2]



#setwd("C:/Users/")

read.fwf("ex_1_fixed.txt", width=c(2,1,3,3,1))




###############   DATAFRAMES

m = matrix( c(1:10), nrow = 5)

df1 = data.frame(m)

names(df1)

names(df1) = c("col 1", "col 2")

names(df1)[1] = "index"

varnames <- c("Subject", "Gender", "CA1", "CA2", "HW")

data2<-read.table("C:/Users/staptkc/Desktop/ex_1.txt", header = FALSE, col.names = varnames)

data2

data3<-read.table("C:/Users/staptkc/Desktop/ex_1_comma.txt", header = FALSE, sep = ",")

data3

data3<-read.csv("C:/Users/staptkc/Desktop/ex_1_comma.txt", header = FALSE)


attach(data3)








########## While Loop:
 #To find the sum of first 10 integers:


x<-0; S<-0
while(x<=10) {
  S<- S+ x
  x<-x+1
}
S

### find the sum of all even numbers that are smaller than 10:
x = 0
S = 0

while (
  x<=10) {#x = x+2

  S = S+x

  x = x+2
}
S








# Print the square of first 5 integers 
x <- 0
test <- TRUE  # test <- 1
while(test>0){
x <- x+1
test <- isTRUE(x<6) # (x<6)
cat(x^2,test,"\n")
}



################ While loop with  break



i <- 1

while (i <= 6) {
  if (i==4){break}
  print(i*i)
  i = i+1
}




################ While loop with  next


i <- 1

while (i <= 6) {
  if (i==4)
  {
    i=i+1
    next
  }
  print(i*i)
  i = i+1
}











###########  For Loop:
 #To find the sum of first 10 integers:
S<-0
for(i in 1:10){S <-S+i}
S

##More advanced way:

x <- numeric(10)
for (i in 1:10){
s <- 0
for (j in 1:i){s <- s+j}
x[i] <- s
cat("The sum of the first ", i, "numbers = ", x[i], "\n")
}



################ For loop with  break

for (j in 1:6) {
  if (j==4){break}
  print(j*j)
  j = j+1
}



################ For loop with  next

for (j in 1: 6) {
  if (j==4)
  {
    j=j+1
    next
  }
  print(j*j)
  j = j+1
}





##############  Redirecting Data:

sink("C:/Data/datasink_ex1.txt")  #directory should be changed accordingly
x <- numeric(15)
for (i in 1:15)
{ s <- 0
for (j in 1:i) {s <- s+j}
x[i] <- s
cat("The sum of the first ", i, "numbers = ",x[i], "\n")
}
sink()

cat("The sum of the first ", i, "numbers = ",x[i], "\n")



############  Write data into a file and save that file to computer


write.table(data2,"C:/Data/saved_file.txt") 
# 'data2' is the set of all the data including columns with header that you want to save
# a file, 'saved_file.txt' will be created in your computer, under C:/Data

#OR

write.csv(data2,"C:/Data/saved_file.csv") 

################ Function to find median:

med <- function(x) {
n <- length(x)
odd.even <- n%%2
if(odd.even == 0) (sort(x)[n/2] + sort(x)[n/2+1])/2
else sort(x)[ceiling(n/2)]
}

int<-c(1:11)
med(int) # 6

int<-c(1:10)
med(int) #5.5


