 ###   TUTORIAL 6



## Q1

test<- matrix(c(16,24,654,306), nr=2, byrow=T)
colnames(test) <- c("Day", "Evening")
rownames(test) <- c("No", "Yes")
test

prop.table(test, margin = 2)
# conditional proportion/probability of Conforming = No given the shift should be used.
# Since the purpose of this study is to see if the shift affects the conforming, we can consider variable
# Shift as explanatory variable while Conforming as the response variable. Hence, the conditional
# proportion Pr(No|Shift = Day) and Pr(No|Shift = Evening) should be most infomative.


chisq.test(test)  #with continuity correction
# p-value = 0.0004079. This provides a very strong evidence that the conforming and shift are not independent.

chisq.test(test, correct = FALSE)  #without continuity correction






##  Q2 # paired data, hence McNemar test for paired nominal data should be used

#For Males:
debateM<- matrix(c(67,28,46,54), nr=2, byrow=T)
colnames(debateM) <- c("Candidate A", "Candidate B")
rownames(debateM) <- c("Candidate A", "Candidate B")
debateM

mcnemar.test(debateM) # with continuity correction 
# same result as in Python: 


# For Females:

debateF <- matrix(c(58,42,37,61), nr=2, byrow=T)
colnames(debateF) <- c("Candidate A","Candidate B")
rownames(debateF) <- c("Candidate A","Candidate B")
debateF

mcnemar.test(debateF) # with continuity correction 

mcnemar.test(debateF, correct = FALSE) # without continuity correction 

#mcnemar(debateM, correction = True, exact = False)


# Both tests for male group and female group have similar set of hypothese:
# H0 : the opinion of voters before and after the debate are independent vs
# H1 : the opinion of voters changes after the debate:

# The debate has no effect on changing the opinions of the female voters (large p-value means data do not
# provide evidence against H0, meaning the opinion before and after are independent), but the debate
# has an effect on the male voters' opinions (small p-value means data provide strong evidence against H0).









