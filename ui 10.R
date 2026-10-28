                                         # logs
# 1)
#A regression gives: test score_hat =12 + 8 * log(income)     #LEVEL-LOG 
#What does the 8 mean?                                  ***  ΔY = β1/100   ****
  
   A 1% rise in income is associated with a rise of about 0.08 points in the 
   predicted test score.
   
   # bc B1= 8  ==>  8/100 = 0.08
   
   
b1 <- 8       # coefficient on log(x)
pct_change_x <- 1 # percent change in x
   
change_y <- b1 * (pct_change_x / 100)
   
cat("Change in Y =", change_y, "units\n")





#2)
#  A LOG-LEVEL regression:    log(y)_hat = 2.4 + 0.02*X   
#  By about what percent does Y change when x falls by 3?

   # HINT: %Δy = 100 (β1) *ΔX
   
b1 <- 0.02
chnge_x <- -3

pct_change_y <- 100 * b1 * chnge_x

cat("Percent change in Y =", round(pct_change_y, 2), "%\n")



#3)
#Data: bwght (bwght = a baby's birth weight in ounces; faminc = the family's income in thousands).
#Run the log-log regression lm(log(bwght) ~ log(faminc), data = bwght). 
#By about what percent does predicted birth weight change when family income rises by 20%?


#HINT: LOG-LOG => %Δy = β1 * %Δx    The slope is an elasticity.

reg <- lm(log(bwght) ~ log(faminc), data = bwght)
reg       #(Intercept) B0 = 4.69673  |  log(faminc) B1 = 0.02061

b1 <- 0.02061

pct_x <- 20

# Predicted percent change in y
pct_y <- b1 * pct_x

round(pct_y, 2)




        #Expected values / variances of the OLS estimators & their standard errors

#4)
#  Suppose you could draw 1,000 different random samples from the same population
#  and compute B1^  in each one. If OLS is unbiased (SLR1–SLR4 hold), 
#  which statement is correct?

#    NOTE: if OLS is unbiased   =>   E(B1^) = B1

The average of the 1,000 estimates is close to B1




#6
# Under SLR1–SLR5, Var(β1^) =  σ^2/SSTx   Holding everything else fixed, 
# which changes would make Var(β1^) larger?
1. A larger error variance σ^2
2. A smaller sample (fewer observations with X spread the same way, so a smaller SSTx)
3. X values bunched more tightly together (a smaller SSTx)





#7
# A simple regression on n=9 observations has σ^ =8.3 and
# the sample standard deviation of X => Sx = 1.2
# Find the standard error SE(B1^)?
  
  # HINT: SSTX = (n−1)*Sx^2  (square the standard deviation), then SE(B1^) = σ^/sqrt(SSTx)

n <- 9
sigma_hat <- 8.3  # σ
sx <- 1.2

SSTx <- (n - 1) * sx^2
SE_b1 <- sigma_hat / sqrt(SSTx)

round(SE_b1, 2)  



  
#9
# Under SLR1–SLR5, a regression has sd(β^1) = 0.25  
# If the sample were 4 times as large, with the X values spread out in the same way 
# (so SSTx  is also 4 times as large), what would sd(β^1) be?

 # HINT: sd(β^1) = σ /sqrt(SSTx)  Multiplying SSTx by k divides sd by sqrt(k) not just k

sd_old <- 0.25
k <- 4                      # Enter how many times larger SSTx becomes

sd_new <- sd_old / sqrt(k)

round(sd_new, 2)






                    #multiple regression predictions and predicted changes

#12
# A multiple regression gives Y_hat = 10 + 2.5*x1 + 2.3*x2
# Find the predicted value Y_hat at x1= 13 , x2= 15 ?
  
  # HINT: plug both values in, exactly as in simple regression: 
   #           Y_hat = b0^ + b1^ *x1 + b2^ *x2

b0 <- 10
b1 <- 2.5
b2 <- 2.3

x1 <- 13
x2 <- 15

yhat <- b0 + b1*x1 + b2*x2

round(yhat, 2)





#13
A multiple regression gives: Y_hat = 6 + 3.2*x1 − 0.9*x2 
If x1 rises by 3 and x2 rises by 4, by how much does Y_hat (y^) change? 

     # HINT: Δy^ = β1*Δx1 + β2*Δx2

b1 <- 3.2
b2 <- -0.9

x1 <- 3
x2 <- 4

y_hat <- b1*x1 + b2*x2

round(y_hat, 2)


#or
b1 <- 3.2
b2 <- -0.9

dx1 <- 3
dx2 <- 4

dy <- b1*dx1 + b2*dx2

round(dy, 2)


#14
# Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; 
#             exper = years of experience; tenure = years with the current employer).
# reg <- lm(wage ~ educ + exper + tenure, data = wage1).
# What is the predicted hourly wage of a worker w/ educ= 10 , tenure = 5 ??
  
     
#     Hint: save the coeff with b <- reg$coefficients, then do something like b[1] + b[2]*14 + b[3]*10 + b[4]*5.


reg <- lm(wage ~ educ + exper + tenure, data = wage1)
reg

b <- coef(reg)    # Save coefficients
b

educ <- 14        #  Inputs #  
exper <- 10
tenure <- 5

# Predicted wage
yhat <- b[1] + b[2]*educ + b[3]*exper + b[4]*tenure

round(yhat, 2)

#Or even shorter:
  

predict(reg,
        newdata = data.frame(
          educ = 14,
          exper = 10,
          tenure = 5
        ))








#15
#Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; 
#             exper = years of experience; tenure = years with the current employer).

# Using the same regression of wage on educ, exper and tenure, 
# by how much does the predicted hourly wage change when tenure RISES by 3 years ??
# ( holding education and experience fixed )


#   Hint: Wage_hat = B3^ * Δtenure     when educ and exper do not change.

reg <- lm(wage ~ educ + exper + tenure, data = wage1)

b_tenure <- coef(reg)["tenure"]        # Coefficient on tenure
change_tenure <- 3                 # Change in tenure

change_wage <- b_tenure * change_tenure # Change in predicted wage

round(change_wage, 2)


#FAST:
# Prediction
b <- coef(reg)
yhat <- b[1] + b[2]*educ + b[3]*exper + b[4]*tenure

# Change from tenure increase
delta_tenure <- 3
delta_wage <- coef(reg)["tenure"] * delta_tenure

summary(reg)















#NOT on ui :     omitted-variable bias

#8
x <- c(1,2,6,7,9)
SSR <- 103
n <- 5

sigma_hat <- sqrt(SSR/(n-2))
sstx <- sum((x - mean(x))^2)

se_b1 <- sigma_hat / sqrt(sstx)

round(se_b1, 2)


#10
# Run regression
reg <- lm(salary ~ roe, data = ceosal1)

# Sample size
n <- nrow(ceosal1)

# SSR (sum of squared residuals)
SSR <- sum(residuals(reg)^2)

# sigma_hat
sigma_hat <- sqrt(SSR/(n - 2))

# SSTx
SSTx <- sum((ceosal1$roe - mean(ceosal1$roe))^2)

# Standard error of beta1
se_b1 <- sigma_hat / sqrt(SSTx)

round(se_b1, 2)

#11



#16
#Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; roe = the firm's return on equity, in percent; sales = firm sales in millions of dollars). Run reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1). By about what percent does predicted salary change when return on equity rises by 10 points, holding sales fixed? (Give a percent. Use the unrounded coefficient.) (round to the hundredths)
#Hint: the roe term is log-level, so 
#, holding sales fixed.
reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)

b <- coef(reg)

beta_roe <- b["roe"]

delta_roe <- 10

pct_change_salary <- 100 * beta_roe * delta_roe

round(pct_change_salary, 2)

#17
reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)

b <- coef(reg)

sales <- 5000
roe <- 15

log_salary_hat <- b[1] +
  b[2] * log(sales) +
  b[3] * roe

salary_hat <- exp(log_salary_hat)

round(salary_hat, 2)


#18
b1_hat <- 0.84
b2_hat <- 0.52
delta1 <- -0.44

b1_tilde <- b1_hat + b2_hat * delta1

round(b1_tilde, 2)


#19
beta2 <- -1.17
delta1 <- -0.26

bias <- beta2 * delta1

round(bias, 2)


#21
# Multiple regression
reg1 <- lm(wage ~ educ + exper, data = wage1)

# Regression of omitted variable on included variable
reg2 <- lm(exper ~ educ, data = wage1)

# Get coefficients
beta2_hat <- coef(reg1)["exper"]
delta1_hat <- coef(reg2)["educ"]

# Product
answer <- beta2_hat * delta1_hat

round(answer, 2)

