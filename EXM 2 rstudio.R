#                     Exam 2 :    problem set 9    
library(wooldridge)
#----------------------------------------------------------------------------------------------

#2

#A LOG-LEVEL reg: log(wage), on tenure (months) , slope = 0.0095
#By what percent does wage rise with one more year of tenure ??? 

# Hint: 1 yr = 12 months, so Δx = 12.     log-level => % Δy = 100 * β1*⋅Δx 

Slope <- 0.0095
change_x <- 12

percent_change <- 100 * Slope * change_x
percent_change


#----------------------------------------------------------------------------------------------

#3

# Reg: log(salary)^ = 4.93 + 0.27 * log(sales),  [w/ salary in thousands of dollars] 
# If salary measured in dollars instead (evry salary *1000),

# what is the new intercept ???

#Hint: log(1000*y) = log(1000) + log(y)  ;  log(1000) = 6.91

int <- 4.93
scale_factor <- 1000

new_int <- int + log(scale_factor)
new_int


#----------------------------------------------------------------------------------------------


#5

# A LOG-LOG reg: log(y)^  =3.9 + 0.62*log(x)   
# By what percent does Y change when X FaLLS by 20% ??? 

# Hint: log-log: %Δy = β1* %Δx, and a fall means %Δx is negative.

b1 <- 0.62
pct_change_x <- - 20

Change_y <- b1 * pct_change_x
Change_y



#----------------------------------------------------------------------------------------------

#6

#Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; 
#                  roe = the firm's return on equity, in percent; 
#                sales = firm sales in millions of dollars).

#log-level reg=  lm(log(salary) ~ roe, data = ceosal1).
#By what percent does ^salary change when return on equity rises by 5 points?

#   Hint: use:  log-level => % Δy = 100 * β1 * Δx   , with  ΔROE = 5

model <- lm(log(salary) ~ roe, data = ceosal1)

b1 <- coef(model)[2]

change_x <- 5

pct_change_y <- 100 * b1 * change_x
pct_change_y



#----------------------------------------------------------------------------------------------

# 7

#Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; 
#                  roe = the firm's return on equity, in percent;
#                sales = firm sales in millions of dollars).

#level-log reg: lm(salary ~ log(sales), data = ceosal1).

#By how many thousand dollars does predicted salary change when sales rise by 10%? 

# Hint: level-log: %Δy =(β1/100) * %Δx   -- Salary is already in thousands of dollars.

model <- lm(salary ~ log(sales), data = ceosal1)

b1 <- coef(model)[2]

pct_change_x <- 10

change_y <- (b1 / 100) * pct_change_x
change_y


#----------------------------------------------------------------------------------------------


# 11

# Under SLR1–SLR5, the error variance is σ2 = 134;  SSTx = 44;
# Find Var(b1^)?

#Hint: Theorem 2.2: Var(b1^) = σ2 / SSTx        Recall: σ2 = sigm

sigma2 <- 134
SSTx <- 44

var_b1 <- sigma2 / SSTx
var_b1


#----------------------------------------------------------------------------------------------

# 12

# Under SLR1–SLR5, the error standard deviation is σ = 14, n = 9,
# Find sd(b1^) ???                Sample variance Sx^2 = 6

#Hint: SSTx = (n−1)*Sx2   then sd(b1^) = σ/ sqrt(SSTx)

sigma <- 14
n <- 9
sx2 <- 6

SSTx <- (n - 1) * sx2
sd_b1 <- sigma / sqrt(SSTx)

sd_b1



#----------------------------------------------------------------------------------------------

#14

#Under SLR1–SLR5, the error standard deviation is σ = 3  A sample: x = (1,2,5,6,7)
#Find sd(b1^)??

# Hint: first SSTx = (x - x_bar)^2     -->     sum((x - mean(x))^2)
#        then sd(b1^) = σ/ srt(SSTx)

sigma <- 3  
x <- c(1,2,5,6,7)

SSTx <- sum( (x - mean(x))^2 )

Sd <-  sigma/ sqrt(SSTx)
Sd


#----------------------------------------------------------------------------------------------

#15

#A simple reg on   n = 12 , SSR = 675
#Find the estimate of the error variance σ2^ ?? 

# HINT: σ2^ = SSR / (n-2)  --> sqrt(sum(reg$residuals^2) / (nrow(wage1) - 2))

SSR <- 675
n <- 12

sigma2_hat <- SSR / (n - 2)
sigma2_hat



#----------------------------------------------------------------------------------------------

#16

#A simple reg     n = 15,    SSR = 827,     SSTx = 14
#Find the standard error SE(b1^) ??

#Hint: first σ^ = srt( SSR / (n-2) )  
#       then SE(b1^) = σ^/ srt(SSTx)

SSR <- 827
n <- 15
SSTx <- 14

sigma_hat <- sqrt(SSR / (n - 2))

SE_b1 <- sigma_hat / sqrt(SSTx)
SE_b1

#----------------------------------------------------------------------------------------------

# 18

#    Data: wage1 (wage = average hourly earnings in dollars; 
#                 educ = years of education).
#reg <- lm(wage ~ educ, data = wage1). 

# What is σ^, the standard error of the regression ????

# HINT: σ2^ = SSR / (n-2) -->  sqrt(sum(reg$residuals^2) / (nrow(wage1) - 2))
# and check it against summary(reg)$sigma.

reg <- lm(wage ~ educ, data = wage1)
reg

sigma2_hat <- sqrt( sum(reg$residuals^2) / (nrow(wage1) - 2) )     # σ2^ = sigma2_hat

summary(reg)$sigma


#----------------------------------------------------------------------------------------------

# 19

#  Data: wage1 (  wage = average hourly earnings in dollars;    educ = years of education).
# What is SSTx for educ, the total sum of squares of the explanatory variable? 

data("wage1")
SSTx <- sum((wage1$educ - mean(wage1$educ))^2)
SSTx


#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------











#----------------------------------------------------------------------------------------------
#                                 problem set 10  #
#----------------------------------------------------------------------------------------------


#2

#  A LOG-LEVEL reg:    log(y)_^ = 2.4 + 0.02*X   
#  By what percent does Y change when x FaLLS by 3 ?? 

# HINT:         % Δy = 100 * (β1) *ΔX

b1 <- 0.02
chnge_x <- -3

pct_change_y <- 100 * b1 * chnge_x

cat("Percent change in Y =", round(pct_change_y, 2), "%\n")



#----------------------------------------------------------------------------------------------

#3)

#   Data: bwght (bwght = a baby's birth weight in ounces; 
#               faminc = the family's income in thousands).
#Run LOG-LOG reg: lm(log(bwght) ~ log(faminc), data = bwght). 

#By what percent does predicted birth weight change when family income rises by 20%?


#HINT: LOG-LOG => %Δy = β1 * %Δx    The slope is an elasticity.

reg <- lm(log(bwght) ~ log(faminc), data = bwght)
reg       #(Intercept) B0 = 4.69673  |  log(faminc) B1 = 0.02061

b1 <- reg$coef[2]
X <- 20


Y_chnge <- b1 * X
round(Y_chnge, 2)


#----------------------------------------------------------------------------------------------

#7

# A simple regression on n = 9 , σ^ = 8.3 and
#       sample standard deviation of X => Sx = 1.2

# Find the standard error SE(B1^)?

# HINT: SSTX = (n−1) * Sx^2  (square the standard deviation), then   SE(B1^) = σ^ /sqrt(SSTx)

n <- 9
sigma_hat <- 8.3  # σ
sx <- 1.2     # sample standev

SSTx <- (n - 1) * sx^2

SE_b1 <- sigma_hat / sqrt(SSTx)
round(SE_b1, 2)  


#----------------------------------------------------------------------------------------------

#8

# Simple REG uses five X values :   X =(1,2,6,7,9)  and  SSR =103
#Find the standard error SE (B1^) ??

#Hint: n = 5                --->     σ^ = sqrt( SSR / (n - 2) )
#     SSTx =  (Xi - X_bar)^2    ---> SE(B1^) = σ^/ sqrt(SSTx)    

x <- c(1,2,6,7,9)
SSR <- 103
n <- 5

sigma_hat <- sqrt( SSR / (n-2) )

SSTx <- sum( (x - mean(x))^2 )


SE_b1 <- sigma_hat / sqrt(SSTx)
round(SE_b1, 2)



#----------------------------------------------------------------------------------------------
#9

#  Under SLR1–SLR5, a REG:  sd(β^1) = 0.25  
#  If the sample were 4 times as large, with the X values spread out in the same way 
#  (so SSTx  is also 4 times as large),
# what would sd(β^1) be ???

# HINT: sd(β^1) = σ /sqrt(SSTx) 
#Multiplying SSTx by k divides sd by sqrt(k) not just k

sd_old <- 0.25
k <- 4                      # Enter how many times larger SSTx becomes

sd_new_b1 <- sd_old / sqrt(k)

round(sd_new_b1, 2)

#----------------------------------------------------------------------------------------------

#10


#  Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; 
#                    roe = the firm's return on equity, in percent;
#                  sales = firm sales in millions of dollars).
#  Run reg <- lm(salary ~ roe, data = ceosal1).

#  Compute the standard error SE(B1^) ???      by hand from σ^ and SSTx 


#  HINT :  σ^ = sqrt( SSR / (n - 2) ) and SSTx = sum((ceosal1$roe - mean(ceosal1$roe))^2)
#   Then  ------>   SE(B1^) = σ^ / sqrt(SSTx) 

#  Check it:  summary(reg) (column "Std. Error" for roe)


reg <- lm(salary ~ roe, data = ceosal1)
SSR <- sum(residuals(reg)^2)
            
sigma_ht <- sqrt( SSR / (nrow(ceosal1)-2) )

SSTx <- sum( (ceosal1$roe - mean(ceosal1$roe))^2 )


SE_b1 <- sigma_hat/sqrt(SSTx)
SE_b1

#OR
summary(reg)     # "Std. Error" for roe


#----------------------------------------------------------------------------------------------

 #12

 # A multiple regression :  Y_hat = 10 + 2.5*X1  +  2.3*X2

 # Find the predicted value Y_hat at X1 =13 , X2 =15 ????
 
 # HINT: plug both values in, exactly as in simple regression: 
 #           Y_hat = b0^ + b1^ *x1 + b2^ *x2
 
 b0 <- 10
 b1 <- 2.5
 b2 <- 2.3
 
 x1 <- 13
 x2 <- 15
 
 yhat <- b0 + b1*x1 + b2*x2
 
 round(yhat, 2)
 
 
 #----------------------------------------------------------------------------------------------
 
 
 #13
 
# A multiple REG: Y_hat = 6 + 3.2*X1 − 0.9*X2 
# If X1 rises by 3  and  X2 rises by 4, 
#  By how much does Y_hat (y^) change? 
   
   # HINT: Δy^ = β1 * ΔX1 + β2 * ΔX2
   
 b1 <- 3.2
 b2 <- -0.9
 
 x1 <- 3
 x2 <- 4
 
 y_hat <- b1*x1 + b2*x2
 
 round(y_hat, 2)
 

 
 #----------------------------------------------------------------------------------------------
 #14
 
 # Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; 
 #             exper = years of experience; tenure = years with the current employer).
 # reg <- lm(wage ~ educ + exper + tenure, data = wage1).
 
 # What is the predicted hourly wage of a worker w/ educ = 14 , exper =10 tenure = 5 ??

 #Hint: save coeff with b <- reg$coefficients, then do something like b[1] + b[2]*14 + b[3]*10 + b[4]*5.
 
 
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
 
 
 #----------------------------------------------------------------------------------------------
 
 #15
 
 #Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; 
 #             exper = years of experience; tenure = years with the current employer).
 
 # regression of wage on educ, exper and tenure,
 
 # By how much does the predicted hourly wage change when tenure RISES by 3 years ???
 
 
 #   Hint: Wage_hat = B3^ * Δtenure     ( holding education and experience fixed )
 
 reg <- lm(wage ~ educ + exper + tenure, data = wage1)

 b0 = reg$coefficients[1]  #wge
 b1 = reg$coefficients[2]  # exper
 b2 = reg$coefficients[3]  #Tenure
 
 b3 = reg$coefficients[4]
 b4 = reg$coefficients[5]
 
 Tenure_rise <- 3
 
 Wge_chnge <- b3 * Tenure_rise
 round(Wge_chnge, 2)
 
 summary(reg)
 #ORRR
 
 b_tenure <- coef(reg)["tenure"]        # Coefficient on tenure
 change_tenure <- 3                 # Change in tenure
 
 change_wage <- b_tenure * change_tenure # Change in predicted wage
 
 round(change_wage, 2)
 
 
 
 #----------------------------------------------------------------------------------------------
 
 #16
 #  Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars;
 #                 roe = the firm's return on equity, in percent;
 #               sales = firm sales in millions of dollars).
 
 #Run reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1). 
 
 #By what percent does predicted salary change when return on equity rises by 10 points  ????? 
 
 #Hint: the roe term is LOG-LEVEL, so: % ΔSalary = 100* B_roe^ *Δroe  (holding sales constant)
 
 reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
 

 b3 = reg$coefficients[3]   # ROE
 
 ROE_rise <- 10
 
 
 Slry_chnge <- 100* b3 * ROE_rise
 round(Slry_chnge, 2)
 
 
 #### ORRR
 b <- coef(reg)
 
 b3_roe <- b["roe"]
 
 roe_chnge <- 10
 
 
 change_salary <- 100 * b3_roe * roe_chnge
 round(change_salary, 2)
 
 
 #----------------------------------------------------------------------------------------------
 
 #17
 
 # Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars;
#                   roe = the firm's return on equity, in percent;
#                   sales = firm sales in millions of dollars). 
 
# Reg of log(salary) on log(sales) and roe,
 
# what is the predicted salary (thousands) of a CEO whose firm has sales of 5,000 ($5 billion) 
# and a return on equity of 15 ???    
 
# Hint: first predict log(salary) by plugging in log(5000) and roe = 15,
# then undo the log with exp()   :  b <- reg$coefficients: exp(b[1] + b[2]*log(5000) + b[3]*15).
 
 
reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
 
b <- reg$coefficients
b
 
sales <- 5000
roe <- 15
   
 
log_salary <- exp( b[1] + b[2]*log(sales) + b[3]*roe)
round(log_salary, 2)
 
 
 #----------------------------------------------------------------------------------------------
 
 #18
#Formula (3.23) links the simple and multiple regression slopes: B1~ = B1^ + B2^ * δ1~(squiggly).
#The multiple regression of Y on X1 and X2 gives B1^= 0.84 and B2^= 0.52, and slope from regressing X1
#on X1 is δ1= -0.44. 
#Find the simple-regression slope B1~ from regressing Y on X1 alone ????

#Hint: plug into B1~ = B1^ + B2^ * δ1~  [Watch the signs]


 b1_hat <- 0.84
 b2_hat <- 0.52
 squiggly_1 <- -0.44
 
 b1_tilde <- b1_hat + b2_hat * squiggly_1
 
 round(b1_tilde, 2)
 
 
 #----------------------------------------------------------------------------------------------
 
 #19
 
# In the population, Y= B0 + B1*X1 + B2*X2 + U with B2 = -1.17. You omit 
# X2 and regress Y on X1 alone, and the slope from regressing X2 on X1 is (squiggly) δ1~ = -0.26
# What is the bias of the simple-regression slope, Bias(B1~) = E[B1~] - B1 ? 
   
# Hint:    Bias(B1~) = B2 *δ1~   [neg =  slope is biased downward]
 
 b2 <- -1.17
 squiggly_1 <- -0.26
 
 bias <- b2 * squiggly_1
 
 round(bias, 2)
 
 #----------------------------------------------------------------------------------------------
 
 
 #21
 
# Data: wage1 (wage = average hourly earnings in dollars; 
#               educ = years of education; 
#              exper = years of experience;
#             tenure = years with the current employer). 
 
#Run Multiple REG:  lm(wage ~ educ + exper, data = wage1) 
#REG of omitted variable:  lm(exper ~ educ, data = wage1).
 
# Compute  B2^ *δ1~   ??? , the EXPER coeff from first REG times EDUC slope from second REG ??? 
 
# Hint: by (3.23), simple-regression educ slope from: lm(wage ~ educ, data = wage1)) minus the multiple-regression educ slope. 
 
 
 # Multiple regression
 reg1 <- lm(wage ~ educ + exper, data = wage1)
 
 # Regression of omitted variable on included variable
 reg2 <- lm(exper ~ educ, data = wage1)
 
 # Get coefficients
 b2_hat <- coef(reg1)["exper"]
 squiggly <- coef(reg2)["educ"]
 
 # Product
 Bias <- b2_hat * squiggly
 
 round(Bias, 2)
 
 
 #----------------------------------------------------------------------------------------------
 #----------------------------------------------------------------------------------------------
 #----------------------------------------------------------------------------------------------
 #----------------------------------------------------------------------------------------------
   
 
 
 
 
 #----------------------------------------------------------------------------------------------
 #                                 Problem Set 11                                               #
 #----------------------------------------------------------------------------------------------
 
 # 1
 
 # A REG : log(y^) = 1.9 + 0.065*x1 − 0.054*x2
 
 # If X1 RISES by 4 and X2 RISES by 3, by WHaT PERCENT DOES PREDICTED Y CHaNGE ??? 
 
 #  Hint: LOG-LEVEL  MLR:  % Δy = 100 * ( β1^*Δx1 + β2^*Δx2 )
 
 b1 <- 0.065
 b2 <- -0.054
 
 x1 <- 4
 x2 <- 3
 
 Y_change <- 100 * (b1*x1 + b2*x2)
 Y_change
 
 
 #----------------------------------------------------------------------------------------------

 
 #2
 
 # Data: wage1 (wage = average hourly earnings in dollars; 
 #               educ = years of education; exper = years of experience;
 #             tenure = years with the current employer). 
 
 #Run Reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1).
 
 #By what PERCENT does PREDICTED WaGE CHaNGE when Exper RISES BY 2 YRS
 #and Tenure RISES by 1 YR, holding education FIXED  ???
 
 #  HINT: % Wage^ = 100 * B_Exper * 2 + B_Tenure *1)
 
 #  with  b <- reg$coefficients: 100 * (b[3]*2 + b[4]*1).
 
 library(wooldridge)
 reg <- lm(log(wage) ~ educ + exper + tenure, data=wage1)
 reg
 
 b <- reg$coefficients   #educ=1  exper=2  tenure=3
 
 b0 <- reg$coefficients[1]  
 b0
 b1 <- reg$coefficients[2]  #educ
 b1
 b2 <- reg$coefficients[3]  #exper
 b2
 b3 <- reg$coefficients[4]  #tenure
 b3
 
Exper <- 2
Tenure <- 1

 
 Wge_chnge <- 100* ( b2*Exper +  b3*Tenure)
 Wge_chnge
 

 #---------------------------------------------------------------------------------------------
 
 #3 
 
 #Data: wage1 (wage = average hourly earnings in dollars;
 #             educ = years of education; 
 #            exper = years of experience; 
 #           tenure = years with the current employer). 
 
 # Use REG of log(wage) on educ, exper and tenure, 
 # What is predicted hourly Wage of a worker with 
 #   educ= 12 yrs, exper= 10 yrs and tenure= 3 yrs???
 
 
 #  HINT: predict log(wage) first, 
 #       then undo log with exp(): exp(b[1] + b[2]*12 + b[3]*10 + b[4]*3).
 
 library(wooldridge)
 
 reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
 
 b <- coef(reg)
 b
 #                         educ      exper      tenure
 log_wage <- exp(b[1] + b[2]*12 + b[3]*10 + b[4]*3)
 log_wage
 
 
 #-----------------------------------------------------------------------------------
 
 # OLS fitted values and residuals (problems 4-6) #
 
 
 
 #5
 #Data: gpa1 (colGPA = college GPA;
 #            ACT = ACT score;
 #            hsGPA = high-school GPA).
 
 #Run reg <- lm(colGPA ~ ACT + hsGPA, data = gpa1).
 
 #Compute the predicted colGPA at ACT = mean(gpa1$ACT) and 
 #                               hsGPA = mean(gpa1$hsGPA)???
 
 #Hint: plug the two means into the fitted equation: 
 #b <- reg$coefficients, THEN b[1] + b[2]*mean(gpa1$ACT) + b[3]*mean(gpa1$hsGPA). 
 #Compare your answer with mean(gpa1$colGPA). Why are they the same??
 
 library(wooldridge)
 
 reg <- lm(colGPA ~ ACT + hsGPA, data = gpa1)
 reg
 
 b <- coef(reg)
 b
 
 ColGPA_Predicted <-  b[1] + b[2] * mean(gpa1$ACT) + b[3] * mean(gpa1$hsGPA)
 ColGPA_Predicted
 
 #OR
 mean(gpa1$colGPA)
 
 #-----------------------------------------------------------------------------------
 
 #6
 
 # A multiple REG : Y^ = 7.5 + 0.4*X1 − 0.5*X2 
 # The sample means of the independent variables are X1= 7.2 , X2= 2.1
 # What is the sample mean Y_bar???
 
 #Hint: the OLS regression passes through the point of means:
 #            Y_bar = B0^ + B1^*X_bar1 + ......Bk^*X_bar.k
 
 b0 <- 7.5
 b1 <- 0.4
 b2 <- -0.5
 
 x1_bar <- 7.2
 x2_bar <- 2.1
 
 ybar <- b0 + b1*x1_bar + b2*x2_bar
 
 round(ybar, 2)
 
 
 
 #-------------------------------------------------------------------------------------
 #  simple vs. multiple regression and formula (3.23) (problems 11-15)#
 

 
 #11
 
 #Formula (3.23) :          B1~bar = B1^ + B2^ * δ1_bar(squiggly)
 #The simple REG of Y on X1 gives   >>>   B1~bar = -0.54
 #The multiple REG of Y on X1 gives >>>   B2^ = 0.79
 #and slope from REG X2 on X1 gives >>  δ1_bar(squiggly) = 0.39
 
 #Find the multiple-regression slope B1^? ?????
 
 #Hint: solve (3.23): B1^ = [ B1~bar - B2^ * δ1_bar(squiggly) ] 
 # Watch the signssss  
 
 b1_tilde <- -0.54
 b2 <- 0.79
 squiggly <- 0.39
 
 b1_hat <- b1_tilde - b2*squiggly
 
 round(b1_hat, 2)  
 
 
 #------------------------------------------------------------------------
 
 #12
 
 #Data: bwght (bwght = a baby's birth weight in ounces; 
 #             cigs = cigarettes the mother smoked per day during pregnancy; 
 #             faminc = the family's income in thousands of dollars).
 
 #Run simple REG:  lm(bwght ~ cigs, data = bwght).
 # What is the slope on cigs ???? 
 #(the predicted change in birth weight, in ounces, from one more cigarette per day)
 
 #Hint: read cigs coef from: lm(bwght ~ cigs, data = bwght)$coefficients. It is negative.
 
 library(wooldridge)
 
 reg <- lm(bwght ~ cigs, data = bwght)
 
 coef(reg)
 coef(reg)["cigs"]   # THISSS
 
 
 lm(bwght ~ cigs, data = bwght)$coefficients   #  check
 
 #------------------------------------------------------------------------
 
 # 13
 #Data: bwght (bwght = a baby's birth weight in ounces; 
 #            cigs = cigarettes the mother smoked per day during pregnancy; 
 #           faminc = the family's income in thousands of dollars).
 
 #Now add family income: reg <- lm(bwght ~ cigs + faminc, data = bwght). 
 # Is the slope on cigs, holding faminc fixed?????
 
 
 #Hint: read  cigs coeff from reg$coefficients. Compare it with your answer to the previous problem.
 
 reg <- lm(bwght ~ cigs + faminc, data = bwght)
 
 coef(reg)
 coef(reg)["cigs"]
 
 
 #------------------------------------------------------------------------
 
 #14     Omitted Variable Bias Formula
 
 #Data: bwght (bwght = a baby's birth weight in ounces; 
 #             cigs = cigarettes the mother smoked per day during pregnancy; 
 #             faminc = the family's income in thousands of dollars).
 
 # Regress faminc on cigs, lm(faminc ~ cigs, data = bwght), TO GET: δ1.
 
 # Compute [ B2^ * δ1 ]  ????
 # ,the faminc coefficient from the multiple regression times this slope.
 
 # Hint: by (3.23),  [B2^ * δ1] = [B1~ - B1^]
 # the simple-regression cigs slope minus the multiple-regression one. [Use to check your answer]
 
 
 
 library(wooldridge)
 
 # B2
 reg1 <- lm(bwght ~ cigs + faminc, data = bwght)
 b2 <- coef(reg1)["faminc"]
 
 # squiggly
 reg2 <- lm(faminc ~ cigs, data = bwght)
 squiggly_1 <- coef(reg2)["cigs"]
 
 # b2 * squiggly
 round( b2 * squiggly_1, 2 )
 
 
 #----------------------------------------------------------------------------------------------
 #----------------------------------------------------------------------------------------------
 #----------------------------------------------------------------------------------------------
 



 
 #----------------------------------------------------------------------------------------------
#                     Exam 1 :  REVIEW            #
#----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
 
# Problems 13-21 cover units of measurement and logs (Day 13).
#----------------------------------------------------------------------------------------------

#1

#   In Test Score = β0 + β1 * class size + U, 
#family income is part of U ---- Richer schools have smaller classes.
#Which statement is correct?

The zero conditional mean assumption fails, because the average of U
changes with class size.

#      [if Omitted Variable (family income) affects Y & is correlated 
#    w/ the included var. (Class Size), Then zero Conditional mean Fails.]


#----------------------------------------------------------------------------------------------


#2

# The OLS estimates β^0, β^1 are "least squares estimates". 
# A researcher fits four candidate lines to the same sample and records three sums for each.
# Exactly one is the OLS line. Which one?

# HINT: U^2_hat is OLS... min. sum of squared resid. [Look @ lowest number]


#-----------------------------------------------------------------------------------------------

#3

#Given : E[X]=1 ; E[X2]=37  Find the STANDEV of 3X - 2

# HINT:     1)   Var(X) = E[X2] − (E[X])^2    
#           2)    sd(x) = sqrt(VarX)
#           3) sd(aX+b) = ∣a∣*sd(X)

EX <- 1
EX2 <- 37

VarX <- EX2 - EX^2     #Var(X)= E[X2] − (E[X])^2

sdX <- sqrt(VarX)      

Stan_dev <- 3 * sdX    #sd(aX+b) = ∣a∣*sd(X)
Stan_dev

#------------------------------------------------------------------------------------------

#4

## Given : Var(X) = 8  , Var(Y) = 9  , Cov(X,Y)= −5.8
#  FIND Var(3X−3Y)
#           a  b  

# HINT: USE -->  Var(aX+bY) = a^2*Var(X) + b^2*Var(Y) + 2*a*b*Cov(X,Y)

a <- 3
b <- -3
VarX <- 8
VarY <- 9           #Var(aX+bY)= a^2*Var(X) + b^2*Var(Y) + 2*a*b*Cov(X,Y)
CovXY <- -5.8

Var_3x3y <- a^2*VarX + (b)^2*VarY + 2*a*(b)*CovXY
Var_3x3y     


#------------------------------------------------------------------------------------------------------

#5

# X and Y have the joint distribution below. 
#  FIND Cor(x,y)?     [Correltion]
x <- c(1,2,4,8)
y <- c(9,8,2,7)
p <- c(0.2,0.3,0.4,0.1)

EX <- sum(x*p)
EY <- sum(y*p)
EXY <- sum(x*y*p)

CovXY <- EXY - EX*EY

VarX <- sum(x^2*p) - EX^2
VarY <- sum(y^2*p) - EY^2

CorXY <- CovXY / sqrt(VarX*VarY)
CorXY

#--------------------------------------------------------------------------------

#6

# Sample  x = (3,4,5,6,10) , y = (1,10,11,8,3) , regress Y on X by OLS. 

# FIND  RESIDUaL of the third observation?  (x3, y3) = (5,11)

x <- c(3,4,5,6,10)         #Hint: residual = actual minus fitted
y <- c(1,10,11,8,3)        # U_hat3= y3 - (B0^ + B1^ * x3)

resid(lm(y ~ x))[3]

#-----------------------------------------------------------------------

#7

# An OLS regression: 
# six observations has residuals: -2.8, 1.6, -0.7, -2.6, -0.3, and one more.
# What is the sixth residual?

#   Hint: OLS residuals always sum to zero!!!!!

Missing = -2.8 + 1.6 - 0.7 - 2.6 - 0.3 
Missing       # = -4.8

# -4.8 + [4.8] = 0 !    --->  so 6th = 4.8


#---------------------------------------------------------------------------------

#8

# Sample, x = (2,3,5,8,10,14)          regress y on x by OLS.
#         y = (14,12,12,14,10,15)

# Compute SUM (xi * Ui^) ?

# Hint: an algebraic property of OLS gives this without any computing. 
#(If you check in R, a printed value like -8.7e-15 means 0: enter 0.)

x <- c(2,3,5,8,10,14)
y <- c(14,12,12,14,10,15)

model <- lm(y ~ x)

sum(x * resid(model))   # 4.440892e-16 is "0" 


#----------------------------------------------------------------------

#9

# A regression has explained sum of squares , SSE=186  , R2=0.13
#  Find the sum of squared residuals SSR?

# HINT: r2= SSE/SST    and SST= SSE + SSR

SSE <- 186
R2 <- 0.13

SST <- SSE / R2
SSR <- SST - SSE

SSR


#-----------------------------------------------------------------------

#10

# A sample:  n= 48 ,  sample variance S^2 = 25
# Find SST  (total sum of squares) ?

#HINT: (Variance) S^2 = SST / (n-1)  ---> SST = S^2 * ( n-1)

n <- 48
S2 <- 25

SST <- S2 * (n - 1)
SST


#-------------------------------------------------------------------------

#11

#   Data: wage1 (wage = average hourly earnings in dollars;
#              tenure = years with the current employer).
# Run reg <- lm(wage ~ tenure, data = wage1). 

# What is the OLS residual of the worker in row 12 ??? 

# Hint: reg$residuals[12], or that worker's wage minus the fitted value

library(wooldridge)
data("wage1")
View(wage1)

reg <- lm(wage ~ tenure, data = wage1)

reg$residuals[12]        #residuals(reg)[12]



#---------------------------------------------------------------------------

#12

#An OLS regression line has intercept b0_hat= 1 , B1_hat = 2.4
#  The sample mean of X is 4. 

#  What is sample mean of Y?

#HINT: the point ( x_bar, y_bar) is always on the OLS line!
b0_hat <- 1
b1_hat <- 2.4

xbar <- 4

ybar <- b0_hat + b1_hat*xbar
ybar


#-------------------------------------------------------------------------

#13

#In reg of Y on X, the OLS ^b1 = 0.58, Every value of Y is multiplied by 10  
# ex. (dollars to cents when the multiplier is 100) 
# What is the new OLS slope (B1^) ??? 

# Hint: if Y  is multiplied by c, both b0 and b1 are multiplied by c

b1 <- 0.58
c <- 10

new_OLS_slope <- c*b1

new_OLS_slope

#-------------------------------------------------------------------------

#14

#In reg of y on x, the OLS  ^b1= 0.97 [Every value of x is divided by 100]
# What is the new OLS slope ??? 

#Hint: if x is multiplied by c, b1 is multiplied by 1/c 
# and b0 does not change. Dividing by k, is multiplying by c = 1/k

b1 <- 0.97
# x is divided by 100
c <- 1/100

# New slope
new_Slope <- b1 / c
new_Slope


#-------------------------------------------------------------------------

# 15
#In a regression of y on x, every value of x is multiplied by 10.

#Which statement is correct?

The slope is divided by 10, the intercept does not change, and R2
does not change.



#-------------------------------------------------------------------------

#16

#Data: ceosal1 (salary = a CEO's salary in thousands of dollars;
#                sales = firm sales in millions of dollars).
#Run lm(salary ~ sales, data = ceosal1).
#If salary is measured in dollars instead of thousands of dollars,

#what is the OLS slope ???      (don't round)

#Hint: multiply the salary column by 1000, run the regression again,
#or use the rule for rescaling y. R prints the [ slope as 1.547e-02,
#                                              which means 0.01547.]
library(wooldridge)
data("ceosal1")
reg <- lm(salary ~ sales, data = ceosal1)
reg

coef(reg)

slope <- coef(reg)[2]
slope

new_slope <- 1000 * slope
new_slope


#-------------------------------------------------------------------------

#17

# A log-level regression gives : Log Y_ ht = 2.1 + 0.07*x

# By what percent does Y change when x rises by 1 ???          (in percent)

#HINT: log level = % Δy =100 *⋅β1*⋅Δx

b1 <- 0.07
x_chnge <- 1

y_percent <- 100 * b1 * x_chnge

y_percent


#-------------------------------------------------------------------------

#17

# A LOG-LOG regression gives :log(y)=3.2 + 1.09* log(x)
# By about what percent does Y change when X rises by 7%?

#HINT: LOG-LOG model = % Δy = β1 * ⋅%Δx

beta1_hat <- 1.09
pct_change_x <- 7

pct_change_y <- beta1_hat * pct_change_x

pct_change_y


#-------------------------------------------------------------------------

#19

#A LEVEL-LOG regression gives: y_hat= 129 + 20.1 * log(x)

#By how many units does Y change when X rises by 15% ???

# HINT: level-log : Δy = (β1/100)* %Δx

b1_hat <- 20.1

pct_x_change <- 15

y_change <- (b1_hat/100) * pct_x_change

y_change


#-------------------------------------------------------------------------

#20

# A study estimates:  log(rent)^ = 2.1 + 0.45 * log(income)
# What does the 0.45 mean?

A: A 1% rise in income is associated with about a 0.45% rise in rent.

#-------------------------------------------------------------------------

#21

# Data: wage1 (wage = average hourly earnings in dollars;
#             exper = years of potential work experience).

# Run LOG-LEVEL :  lm(log(wage) ~ exper, data = wage1).

# what is percent for predicted wage change with 10 more years of experience? 

# HINT: % Δwage = 100 * β1 * Δexper

library(wooldridge)

data(wage1)

reg <- lm(log(wage) ~ exper, data = wage1)

b1 <- coef(reg)[2]
exper <- 10
  
chnge_wge <- 100 * b1 * exper
chnge_wge

#-------------------------------------------------------------------------


