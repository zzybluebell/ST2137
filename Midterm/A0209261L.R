
# ST2137 quiz 1 

#clear data memory
rm(list=ls())

#load data
data = read.csv("Data/Life_Expectancy_89countries.csv", header = TRUE)

attach(data)
data
#Q1

summaries = function(x) {
  
  r = range(x)
  
  m = mean(x)
  
  sde = sd(x)
  
  Q1 = quantile(x)[2]
  
  Q2 = median(x)
  
  Q3 = quantile(x)[4]
  
  return(c(r, m, sde, Q1, Q2, Q3 ))
  
}

summaries(Life_expectancy)

#Q2
hist(Life_expectancy, freq = FALSE, main ="Life_expectancy ")


#Q3
ci = c(mean(Life_expectancy) - qt(0.99, 89)*sd(Life_expectancy)/ sqrt(89), 
       mean(Life_expectancy) + qt(0.99, 89)*sd(Life_expectancy))
ci



#Q4
cor(Life_expectancy, Adult_mortality)

#Q5:

plot(Life_expectancy, Adult_mortality, main = "Status", xlab = "Life_expectancy", 
     ylab = "Adult_mortality", pch = 20, col = "red")

# comments: this relation is negative assiciation, it has a proper assciation relationship

# Q6:
cor(Life_expectancy, Alcohol)

#Q7
al<-numeric(89)
for ( i in 1:89){
  
  if (Alcohol[i]<=3){
      al[i]=1
    }
  else if (Alcohol[i] <= 6 & Alcohol[i] > 3){
    al[i]=2
    }
  else if (Alcohol[i] <= 9 & Alcohol[i] > 6){
    al[i]=3 
    }
  else{al[i]=4}
}

#al
data$al=al


#Q8
table(al, Status)


#Q9
# yes, it is postive assiciation

#Q10
summary(lm(al~Status))


