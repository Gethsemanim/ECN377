##
#
#       MUltiple choice uestions    #

#                     Exam 2 :    problem set 9    
#----------------------------------------------------------------------------------------------

#1

# A LOG-LOG reg of CEO salary on firm sales:
#                                        log(salary)^ =  B0^ + B1^ * log(sales)
# (with salary in thousands of dollars)  Salary is then measured in dollars instead.
#  What happens?

B1^ does not change; only B0^ changes
#----------------------------------------------------------------------------------------------

#4

#In which model is the slope read as : % Δy = 100 * β1 * Δx

LOG-LEVEL: log(y) = β0 + β1*x + u    # LOG-LOG

#----------------------------------------------------------------------------------------------

# 8

#Theorem 2.1 says that under SLR1–SLR4, E(β1^) = β1. What does this mean?

Averaged over many random samples, b1^ = b1 (actual)


#----------------------------------------------------------------------------------------------

#9

#In Wage = b0 + b1 * educ + U. 
#which is likely to contribute omitted-variable bias to the OLS slope on educ?

The workers ability, which raises wages and tends to be higher for people 
with more education.


#----------------------------------------------------------------------------------------------

# 10

#In a sample of workers, every worker has exactly 12 years of education. 
#You regress wage on education. What goes wrong?

SLR3 fails: the X values are all the same, so the OLS slope cannot be computed.

#----------------------------------------------------------------------------------------------

#13

#Holding everything else fixed, which change makes Var(b1^) smaller?

More spread-out values of X (a larger SSTx).

#----------------------------------------------------------------------------------------------
# 17

#In regression of food spending on income, the spread of food spending around the 
#regression line grows as income grows. Which statement is correct?

SLR5 (homoskedasticity) fails, so the variance formula of Theorem 2.2,
Var(b1^) = σ2 / SSTx , no longer applies (OLS is still unbiased).

#----------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------
#                     Exam 2 :    problem set 10    
#----------------------------------------------------------------------------------------------


#1
# A reg:  test score^ = 12 + 8 * log(income)                 #LEVEL-LOG: 
# What does the 8 mean?                                              ***  ΔY = β1/100   ****

A 1% rise in income is associated with a rise of about 0.08 points in the 
predicted test score.


# bc B1= 8  ==>  8/100 = 0.08

b1 <- 8       # coefficient on log(x)
x_rise <- 1 # percent change in x

change_y <- b1 * (x_rise / 100)
cat("Change in Y =", change_y, "units\n")

#----------------------------------------------------------------------------------------------


#Expected values / variances of the OLS estimators & their standard errors

#4)

#  Suppose you could draw 1,000 different random samples from same population
#  and compute B1^  in each one. If OLS is unbiased (SLR1–SLR4 hold), 
#  which statement is correct?

#    NOTE: if OLS is unbiased   =>   E(B1^) = B1

The average of the 1,000 estimates is close to B1

#----------------------------------------------------------------------------------------------

#5

#Why do we prefer an unbiased estimator with a smaller variance Var ( B1^) ??

Its estimates land closer to B1 from sample to sample, so any one estimate is more reliable.


#----------------------------------------------------------------------------------------------

#6

# Under SLR1–SLR5, Var(β1^) =  σ^2/SSTx   Holding everything else fixed, 
# which changes would make Var(β1^) larger?
1. A larger error variance σ^2
2. A smaller sample (fewer observations with X spread the same way, so a smaller SSTx)
3. X values bunched more tightly together (a smaller SSTx)

#----------------------------------------------------------------------------------------------


#11

#  In Wage^ = β0^ + β1^ *educ + β2^ *exper  , What does B1^ measure?

The predicted change in wage from one more year of education, holding experience fixed.


#----------------------------------------------------------------------------------------------

#20

#An omitted variable X2 lowers Y (β2<0), and X2 is positively correlated with X1.
#The simple-regression slope on X1 is:

biased downward       ----> bc Correlted Pos (+)  *  B2 neg (-)   =  (-)  


#----------------------------------------------------------------------------------------------





#----------------------------------------------------------------------------------------------
#                                 Problem Set 11                                               #
#----------------------------------------------------------------------------------------------



#4

# After an OLS multiple regression of Y on X1,....Xk (with an intercept) 
# and residuals: Ui^ = Y_i - Y_i^ 
#  which statements are always true????       [Select all that apply]

#Hint: the notes list three properties, the same as in simple regression.
# Careful: the residuals are uncorrelated with the fitted values, not with Y itself.

- The regression passes through the point of sample means: 
  Y_bar = B0^ + B1^*X_bar1 + ......Bk^*X_bar.k

-  The sample covariance between each Xj and the residuals is 0.

- The sample average of the residuals is 0.

#-----------------------------------------------------------------------------------

#        the MLR assumptions and unbiasedness (problems 7-10)#


#7
#Which of these regressions violate MLR3 (no perfect collinearity), 
#so that OLS cannot estimate them? Select all that apply.

- voteA on expendA, expendB and totexpend, where totexpend = expendA + expendB

- wage on income in dollars and income in thousands of dollars

- a regression with 5 independent variables, estimated on 4 observations

#-----------------------------------------------------------------------------------

#8

#Data: vote1 (voteA = percent of the vote won by candidate A;
#           expendA and expendB = campaign spending by candidates A and B,in thousands of dollars).

#Create total spending with: 
#                             vote1$totexpend <- vote1$expendA + vote1$expendB, 

#THEN run: lm(voteA ~ expendA + expendB + totexpend, data = vote1). 

#What does R report for the coefficient on totexpend???????

#  Hint: run it and look at the output.
#  Can you change totexpend while holding expendA and expendB fixed?

vote1$totexpend <- vote1$expendA + vote1$expendB               #not nswer

lm(voteA ~ expendA + expendB + totexpend, data = vote1)        #Not nswer 

#NOTE:  totexpend = expendA+expendB
#    This is exact perfect collinearity.
#   So R cannot estimate all three coefficients simultaneously.

# ---------- SHOWS:
# (Intercept)      expendA      expendB    totexpend  
#  49.61900      0.03833     -0.03613           NA


ANSWER :    NA: R drops totexpend because it is an exact linear function of expendA and expendB.


#----------------------------------------------------------------------------------------------


#9

#  Which situation makes MLR4, E[U|X1, .... Xk] = 0 fail 
#  (so that the X variables are endogenous)?

# HINT: MLR4 can fail because of omitted variables or the wrong functional form.  

nswer: A variable that affects Y and is correlated with X1 is left out of the regression.

# ^^^^^ is OMITTED VARIABLE BIAS

#----------------------------------------------------------------------------------------------

#10

#Unbiasedness only needs MLR1-MLR4. 
#Why not simply add every variable we can find to the regression?

Adding variables can increase the variance of the OLS estimates 
and can even violate MLR3..

#---------------------------------------------------------------------------------------------- 

#  simple vs. multiple regression and formula (3.23) (problems 11-15)#
          

#15

# In the birth-weight regressions, the simple-reg cigs slope= -0.51
# and the multiple-reg slope = -0.46(holding faminc fixed)

# Which reading is correct?

#  Hint:  B2^ >0 :(income raises birth weight) and 
#        δ1_bar<0 :(mothers who smoke more have lower family incomes).


#So when you omit faminc, part of income's effect gets mixed into the smoking coefficient.

#The difference is: [−0.51 − (−0.46)] = −0.05 - 0.51 - (-0.46) = -0.05 − 0.51 − (−0.46) = −0.05

# That extra −0.05-0.05−0.05 comes from the omitted income variable.


About -0.46 per cigarette is the estimated effect of smoking holding income fixed; 
the other -0.05 is incomes effect riding along, because heavier smokers tend to have lower incomes.



#----------------------------------------------------------------------------------------------

#      the direction of omitted-variable bias (problems 16-19) #



#16
# An omitted variable X2 lowers Y (β2<0), and X2 is negatively correlated w/ 
# the included X1. The simple-regression slope on X1 is:?????

# Hint: Table 3.2: Bias(β1~) = [ β2 * δ1~ ]
#                  and δ1~  has the sign of Cor (X1, X2)

Sign:  (−)*(−)=(+)
Positive bias means the simple-regression slope is too high.

✅ Answer: biased upward


#-----------------------------------------------------------------------------

#17

# A researcher regresses crop yield on fertilizer use across farms, leaving out land quality. 
# Better land raises yield, and farmers tend to use more fertilizer on better land. 
# The simple-regression fertilizer slope most likely:???

#  Hint: two legs: the sign of land quality's effect on yield (B2) 
#          and the sign of its correlation with fertilizer (δ1~)


Bias: β2*δ1~ =(+)*(+)=(+)\
Positive bias means the fertilizer coefficient gets pushed upward.

✅ Answer: overstates the effect of fertilizer (upward bias)

#----------------------------------------------------------------------------------------------

#18

# A school regresses exam scores on hours of tutoring, leaving out prior ability.
#Higher ability raises exam scores, but the students who sign up for the most tutoring tend to be the ones with lower prior ability. 
#The simple-regression tutoring slope most likely:

#Hint: find the sign of each leg: the effect of ability on scores
#                                    B2 and Cor (Tutoring, ability)

(+) * (-) = (-) 

NSSWER:::  understates the effect of tutoring (downward bias)

#----------------------------------------------------------------------------------------------



#19 

# You regress Y on X₁ alone, leaving out X₂. 
# When does this cause no omitted-variable bias in the slope on X₁?   [Select all that apply]

# Hint: Bias(β1~) =  [β2 * ⋅δ1δ~]  is zero when either leg is zero.


- X₂ is uncorrelated with X₁ in the sample (δ1~ = 0)

- X₂ has no ceteris-paribus effect on Y(β2=0)
