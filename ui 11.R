#  log-level multiple regression (problems 1-3)


# 1

#A regression gives: log(y^) = 2.5 + 0.049*x1 − 0.052*x2
#If X1 rises by 2 and X2 rises by 3, by about WHaT PERCENT DOES PREDICTED Y CHaNGE? 

#  Hint: LOG-LEVEL  MLR: %Δy = 100*(β1^*Δx1 + β2^*Δx2)

b1 <- 0.049
b2 <-  - 0.052

x1 <- 2
x2 <- 3

pct_change <- 100 * (b1* x1 + b2* x2)
pct_change

#--------------------------------------------------------------------------------------
#

#2
# Data: wage1 (wage = average hourly earnings in dollars; 
#               educ = years of education; exper = years of experience;
#             tenure = years with the current employer). 
#Run reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1).

#By about what PERCENT does PREDICTED WaGE CHaNGE when Experience RISES BY 2 YEaRS
#and Tenure RISES by 1 YEaR, holding education FIXED???    (Give a percent. Use the unrounded coefficients.) 

#   HINT: % Wage^= 100 *^B_Exper * 2 + ^B_Tenure *1)
#        with  b <- reg$coefficients: 100 * (b[3]*2 + b[4]*1).

library(wooldridge)
reg <- lm(log(wage) ~ educ + exper + tenure, data=wage1)
reg

b <- reg$coefficients   #educ=1  exper=2  tenure=3

100*(b[3]*2 + b[4]*1)

# more tries

b <- reg$coefficients

Wge_chnge <- 100*(b[3]*2 + b[4]*1)

b <- reg$coefficients[1]
b1 <- reg$coefficients[2]
b3 <- reg$coefficients[3]

Wge_chnge <- 100*(b0[3]*2 + b0[4]*1)
Exper_chnge <- 100*(b[3]*2 + b[4]*1)
Tenure_chnge <- 
  
########----------------------------------------------------------------------
library(wooldridge)
reg <- lm(log(wage) ~ educ + exper + tenure,data=wage1)
reg

b <- reg$coefficients 
b
b0<- reg$coefficients[1]
b0
b1 <- reg$coefficients[2]    # EDUC
b1
b3 <- reg$coefficients[3]    #Exper
b3
b4 <- reg$coefficients[4]    # tenure
b4

wge_chnge <- 100 * (b3*2 + b4*1)
wge_chnge
#---------------------------------------------------------------------------------------------







#3 
#Data: wage1 (wage = average hourly earnings in dollars;
#             educ = years of education; 
#            exper = years of experience; 
#           tenure = years with the current employer). 
#Using the same regression of log(wage) on educ, exper and tenure, 
#what is the predicted hourly wage, in dollars, of a worker with 
#  12 years of education, 10 years of experience and 3 years of tenure?


#       HINT: Hint: predict log(wage) first, 
#  then undo the log with exp(): exp(b[1] + b[2]*12 + b[3]*10 + b[4]*3).
  
library(wooldridge)

reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)

b <- coef(reg)

exp(b[1] + b[2]*12 + b[3]*10 + b[4]*3)
predict_log <- b[1] + b[2]*12 + b[3]*10 + b[4]*3

exp(predict_log)

 
#-----------------------------------------------------------------------------------


# OLS fitted values and residuals (problems 4-6)



#4
#After an OLS multiple regression of Y on X1,....Xk (with an intercept) 
#and residuals: Ui^ = Y_i - Y_i^ 
#  which statements are always true???? Select all that apply.
  
#Hint: the notes list three properties, the same as in simple regression.
# Careful: the residuals are uncorrelated with the fitted values, not with Y itself.

 - The regression passes through the point of sample means: 
    Y_bar = B0^ + B1^*X_bar1 + ......Bk^*X_bar.k
  
-  The sample covariance between each Xj and the residuals is 0.
  
 - The sample average of the residuals is 0.

#-----------------------------------------------------------------------------------

#6
#A multiple regression gives: Y^ = 6.3 + 0.3*x1 − 0.8*x2 
#The sample means of the independent variables are X1 = 12.4 , X2= 4.5
#What is the sample mean Y_bar???
  
  #Hint: the OLS regression passes through the point of means:
#            Y_bar = B0^ + B1^*X_bar1 + ......Bk^*X_bar.k

b0 <- 6.3
b1 <- 0.3
b2 <- -0.8

x1_bar <- 12.4
x2_bar <- 4.5

ybar <- b0 + b1*x1_bar + b2*x2_bar

round(ybar, 2)

#----------------------------------------------------------------------------------
#        the MLR assumptions and unbiasedness (problems 7-10)#


#7
#Which of these regressions violate MLR3 (no perfect collinearity), 
#so that OLS cannot estimate them? Select all that apply.

- voteA on expendA, expendB and totexpend, where totexpend = expendA + expendB

- wage on income in dollars and income in thousands of dollars

- a regression with 5 independent variables, estimated on 4 observations



#-------------------------------------------------------------------------------------


#9
#  Which situation makes MLR4, E[U|X1, .... Xk] = 0 fail 
#  (so that the X variables are endogenous)?

# HINT: MLR4 can fail because of omitted variables or the wrong functional form.  
  
nswer: A variable that affects Y and is correlated with X1 is left out of the regression.

# ^^^^^ is OMITTED VARIABLE BIAS




#---------------------------------------------------------------------------------------------

#  simple vs. multiple regression and formula (3.23) (problems 11-15)#


#11

#Formula (3.23) : B1~bar = B1^ + B2^ * δ1_bar(squiggly)
#The simple regression of Y on X1 gives >>>   B1~bar = -1.27
#The multiple regression of Y on X1 gives >>> B2^ = 0.55
#and the slope from regressing X2 on X1 gives >>  δ1_bar(squiggly)= -0.66

#Find the multiple-regression slope B1^??????
  
  #Hint: solve (3.23) for [ B1^ = B1~bar - B2^ * δ1_bar(squiggly)]
# Watch the signssss  

b1_tilde <- -1.27
b2 <- 0.55
squiggly <- -0.66

b1_hat <- b1_tilde - b2*squiggly

round(b1_hat, 2)  
  
  


#------------------------------------------------------------------------

#14     Omitted Variable Bias Formula
#Data: bwght (bwght = a baby's birth weight in ounces; 
#             cigs = cigarettes the mother smoked per day during pregnancy; 
#             faminc = the family's income in thousands of dollars). 
#Regress faminc (the variable left out of the simple regression in problem 12) on cigs, 
#lm(faminc ~ cigs, data = bwght), TO GET: δ1 --Compute [ B2^ * δ1 ]????
#,the faminc coefficient from the multiple regression times this slope.

#Hint: by (3.23),  [B2^ * δ1] = [B1~ - B1^]
#the simple-regression cigs slope minus the multiple-regression one. Use that to check your answer.



library(wooldridge)

# beta2 from Question 13
reg1 <- lm(bwght ~ cigs + faminc, data = bwght)
beta2 <- coef(reg1)["faminc"]

# delta1
reg2 <- lm(faminc ~ cigs, data = bwght)
delta1 <- coef(reg2)["cigs"]

# beta2 * delta1
round(beta2 * delta1, 2)



#-------------------------------------------------------------------------------------

#      the direction of omitted-variable bias (problems 16-19) #

#16
#An omitted variable X2 lowers Y (β2<0), and X2 is negatively correlated w/ 
#the included X1. The simple-regression slope on X1 is:?????
  
  # Hint: Table 3.2: Bias(β1~) =β2 *δ1~
#               and δ1~  has the sign of Cor (X1, X2)

  Sign:  (−)*(−)=(+)
Positive bias means the simple-regression slope is too high.

✅ Answer: biased upward


#-----------------------------------------------------------------------------

#17
#A researcher regresses crop yield on fertilizer use across farms, leaving out land quality. 
#Better land raises yield, and farmers tend to use more fertilizer on better land. 
#The simple-regression fertilizer slope most likely:???
  
#  Hint: two legs: the sign of land quality's effect on yield (B2) 
#          and the sign of its correlation with fertilizer (δ1~)


Bias: β2*δ1~ =(+)*(+)=(+)\

Positive bias means the fertilizer coefficient gets pushed upward.

✅ Answer: overstates the effect of fertilizer (upward bias)







#       END OF UI extr  #






#----------------------------------------------------------------------------------------------------


















#5
#Data: gpa1 (colGPA = college GPA;
#            ACT = ACT score;
#            hsGPA = high-school GPA).
#Run reg <- lm(colGPA ~ ACT + hsGPA, data = gpa1).
#Compute the predicted colGPA at ACT = mean(gpa1$ACT) and 
#                               hsGPA = mean(gpa1$hsGPA)???

#Hint: plug the two means into the fitted equation: 
#b <- reg$coefficients, then b[1] + b[2]*mean(gpa1$ACT) + b[3]*mean(gpa1$hsGPA). 
#Compare your answer with mean(gpa1$colGPA). Why are they the same??

library(wooldridge)

reg <- lm(colGPA ~ ACT + hsGPA, data = gpa1)

b <- coef(reg)

ColGPA_Predicted <-  b[1] + b[2] * mean(gpa1$ACT) + b[3] * mean(gpa1$hsGPA)
ColGPA_Predicted

#OR
mean(gpa1$colGPA)










#8
#Data: vote1 (voteA = percent of the vote won by candidate A;
#expendA and expendB = campaign spending by candidates A and B,in thousands of dollars).

#Create total spending with: 
#vote1$totexpend <- vote1$expendA + vote1$expendB, 
#then run lm(voteA ~ expendA + expendB + totexpend, data = vote1). 
#What does R report for the coefficient on totexpend???????
  
#  Hint: run it and look at the output.
#  Can you change totexpend while holding expendA and expendB fixed?
  
vote1$totexpend <- vote1$expendA + vote1$expendB

lm(voteA ~ expendA + expendB + totexpend, data = vote1)

#NOTE:  totexpend = expendA+expendB
#    This is exact perfect collinearity.
#   So R cannot estimate all three coefficients simultaneously.

# ---------- SHOWS:
# (Intercept)      expendA      expendB    totexpend  
#  49.61900      0.03833     -0.03613           NA


ANSWER : NA: R drops totexpend because it is an exact linear function of expendA and expendB.




#10
#Unbiasedness only needs MLR1-MLR4. 
#Why not simply add every variable we can find to the regression?

Adding variables can increase the variance of the OLS estimates 
and can even violate MLR3..




#12
#Data: bwght (bwght = a baby's birth weight in ounces; 
#             cigs = cigarettes the mother smoked per day during pregnancy; 
#             faminc = the family's income in thousands of dollars).
#Run the simple regression lm(bwght ~ cigs, data = bwght).
#What is the slope on cigs (the predicted change in birth weight, in ounces, 
#  from one more cigarette per day)? 
  
#Hint: read the cigs coefficient off lm(bwght ~ cigs, data = bwght)$coefficients. It is negative.

library(wooldridge)

reg <- lm(bwght ~ cigs, data = bwght)

coef(reg)
coef(reg)["cigs"]



# 132
#Data: bwght (bwght = a baby's birth weight in ounces; 
#            cigs = cigarettes the mother smoked per day during pregnancy; 
#           faminc = the family's income in thousands of dollars).
#Now add family income: reg <- lm(bwght ~ cigs + faminc, data = bwght). 
#  is the slope on cigs, holding faminc fixed?????


#Hint: read the cigs coefficient off reg$coefficients. Compare it with your answer to the previous problem.

reg <- lm(bwght ~ cigs + faminc, data = bwght)

coef(reg)
coef(reg)["cigs"]



#15
#and the multiple-regression slope (holding faminc fixed) is abt -0.46
#Which reading is correct?
#  Hint:  B2^ >0 :(income raises birth weight) and 
#        δ1_bar<0 :(mothers who smoke more have lower family incomes).

#So when you omit faminc, part of income's effect gets mixed into the smoking coefficient.

#The difference is: −0.51−(−0.46)=−0.05-0.51 - (-0.46) = -0.05−0.51−(−0.46)=−0.05

# That extra −0.05-0.05−0.05 comes from the omitted income variable.


About -0.46 per cigarette is the estimated effect of smoking holding income fixed; 
the other -0.05 is income's effect riding along, because heavier smokers tend to have lower incomes.








#18

# A school regresses exam scores on hours of tutoring, leaving out prior ability.
#Higher ability raises exam scores, but the students who sign up for the most tutoring tend to be the ones with lower prior ability. 
#The simple-regression tutoring slope most likely:

#Hint: find the sign of each leg: the effect of ability on scores
B2 and Cor (Tutoring, ability)

+*- = - 

NSSWER:::  understates the effect of tutoring (downward bias)



#19 You regress Y on X₁ alone, leaving out X₂. When does this cause no omitted-variable bias in the slope on X₁? Select all that apply.

Hint: Bias(β1~) =  β2*⋅δ1δ~ is zero when either leg is zero.


- X₂ is uncorrelated with X₁ in the sample (δ1~ = 0)

- X₂ has no ceteris-paribus effect on Y(β2=0)


