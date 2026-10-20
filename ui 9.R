
#Problem Set 8:
#uestion 14
  
#In a regression of y on x,the OLS b1= 0.97 Evry value of x is divided by 100.
  # What is the new OLS slope? 
  
#Hint: if x is multiplied by c, b1 is multiplied by 1/c 
#       and b0 does not change. Dividing by k, is multiplying by c = 1/k
  
  b1 <- 0.97
  # x is divided by 100
  c <- 1/100
  
  # New slope
  b1_new <- b1 / c
  b1_new

#uestion 17

# A log-level regression = log(y)_ht = 2.1 + 0.07*x
# By about what percent does Y change when x rises by 1?
  #  hint log-level: %Δy = 100* β1* Δx
  
b1_hat <- 0.07
chnge_x <- 1
  
pct_change_y <- 100 * b1_hat * chnge_x
  
pct_change_y
  

#uestion 19
# level-log regression: Y_ht = 129+ 20.1*log(x)
# By about how many units does Y change when X rises by 15%? 
  #  Hint: level-log: %Δy =(β1/100) * %Δx
  
b1_hat <- 20.1

chnge_x <- 15

y_change <- (b1_hat/100) * chnge_x

y_change


#  Problem set 9
  
#uestion 1
#A log-log regression of CEO salary on firm sales:
  #                                          log(salary)_ht =  B0_ht + B1_ht * log(sales)
#, with salary in thousands of dollars. Salary is then measured in dollars instead.
#  What happens?
  
B1_ht does not chnge, only B0 chnges
  

#uestion 5

# A log-log regression gives: log(y)_ht =3.9 + 0.62*log(x)   b0 + b1
#By about what percent does Y change when X falls by 20%? 

#          Hint: log-log: %Δy = β1* %Δx, and a fall means %Δx is negative.

b1 <- 0.62
pct_change_x <- - 20

Change_y <- b1 * pct_change_x
Change_y


#uestion 6
#Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; 
#roe = the firm's return on equity, in percent; sales = firm sales in millions of dollars).
#log-level regression =  lm(log(salary) ~ roe, data = ceosal1).
#By what percent does predicted salary change when return on equity rises by 5 points?

#   Hint: use the log-level row of Table 2.3 with  ΔROE = 5

model <- lm(log(salary) ~ roe, data = ceosal1)

b1 <- coef(model)[2]
change_x <- 5

pct_change_y <- 100 * b1 * change_x
pct_change_y

#uestion 9
#In wge= b0 + b1 * educ + U. 
#which is the most likely to contribute omitted-variable bias to the OLS slope on educ?

The workers ability, which raises wages and tends to be higher for people 
with more education.

#uestion 13
#Holding everything else fixed, which change makes Var(b1^) smaller?

More spread-out values of X (a larger SSTx).

#uestion 14
#Under SLR1–SLR5, the error standard deviation is σ=3  A sample:x=(1,2,5,6,7)
#Find sd(b1^)?
  
# Hint: first SSTx = (x - x_bar)^2 -->sum((x - mean(x))^2)
#        then  sd(b1^) = σ/ srt(SSTx)
sigma <- 3  
x <- c(1,2,5,6,7)

SSTx <- sum((x - mean(x))^2)

Sd <-  sigma/ sqrt(SSTx)
Sd

#uestion 15

#A simple regression on n=12 , SSR=675
#Find the estimate of the error variance σ2_ht 

# HINT: σ2_ht = SSR / (n-2)  __> sqrt(sum(reg$residuals^2) / (nrow(wage1) - 2))

SSR <- 675
n <- 12

sigma2_hat <- SSR / (n - 2)
sigma2_hat



#uestion 16

#A simple regression n=15, SSR=827, SSTx=14
#Find the standard error SE(b1^)
#Hint: first σ_ht = srt( SSR / (n-2) )  
#       then SE(b1^) = σ_ht/ srt(SSTx)
SSR <- 827
n <- 15
SSTx <- 14

sigma_hat <- sqrt(SSR / (n - 2))
SE_b1 <- sigma_hat / sqrt(SSTx)

SE_b1
































































#Extr from problem set 9  >>>

#uestion 2
#A log-level regression: log(wage), on tenure, with tenure measured in months,slope: 0.0095.
#By about what percent does wage rise with one more year of tenure? 
  
 # Hint: one year is 12 months, so Δx=12  . Use the log-level row of Table 2.3.
  
Slope <- 0.0095
change_x <- 12

percent_change <- 100 * Slope * change_x
percent_change

#uestion 3
# regression: log(salary)_ht = 4.93 + 0.27*log(sales), with salary in thousands of dollars. 
#If salary measured in dollars instead (evry salary *1000), what is the new intercept?

  #Hint: log(1000*y) = log(1000) + log(y) nd log(1000) = 6.34
intercept <- 4.93
scale_factor <- 1000

new_int <- intercept + log(scale_factor)
new_int

#uestion 4
#In which model is the slope read as : %Δy = 100 * β1*⋅Δx
log level: log(y)= β0 + β1*x + u

#uestion 7
#Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; 
#roe = the firm's return on equity, in percent; sales = firm sales in millions of dollars).
#level-log regression: lm(salary ~ log(sales), data = ceosal1).

#By about how many thousand dollars does predicted salary change when sales rise by 10%? 

 #    Hint: level-log: %Δy =(β1/100) * %Δx-- Salary is already in thousands of dollars.

model <- lm(salary ~ log(sales), data = ceosal1)

b1 <- coef(model)[2]
pct_change_x <- 10

change_y <- (b1 / 100) * pct_change_x
change_y

#uestion 8
#Theorem 2.1 says that under SLR1–SLR4, E(β1^)=β1. What does this mean?
Averaged over many random samples, b1_ht equals b1 (ctul)


#uestion 10
#In a sample of workers, every worker has exactly 12 years of education. 
#You regress wage on education. What goes wrong?
  
SLR3 fails: the X values are all the same, so the OLS slope cannot be computed.

#uestion 11
#Under SLR1–SLR5, the error variance is σ2=134,  SSTx=44,  Find Var(b1^)?
#Hint: Theorem 2.2: Var(b1^) = σ2 / SSTx        Recll: σ2 = sigm
sigma2 <- 134
SSTx <- 44

var_b1 <- sigma2 / SSTx
var_b1

#uestion 12
#Under SLR1–SLR5, the error standard deviation is σ=14, n=9, Smple vrince Sx^2= 6
#Find sd(b1^)?
#Hint: SSTx=(n−1)*Sx2   thn sd(b1^) = σ/ sr(SSTx)
sigma <- 14
n <- 9
sx2 <- 6

SSTx <- (n - 1) * sx2
sd_b1 <- sigma / sqrt(SSTx)

sd_b1


#uestion 17

#In regression of food spending on income, the spread of food spending around the 
#regression line grows as income grows. Which statement is correct?
SLR5 (homoskedasticity) fails, so the variance formula of Theorem 2.2,
Var(b1^) = σ2 / SSTx , no longer applies (OLS is still unbiased).

#uestion 18
#Data: wage1 (wage = average hourly earnings in dollars; 
#educ = years of education).
#reg <- lm(wage ~ educ, data = wage1). 
#What is σ_ht, the standard error of the regression?

#HINT: σ2_ht = SSR / (n-2) --> sqrt(sum(reg$residuals^2) / (nrow(wage1) - 2))
# and check it against summary(reg)$sigma.
reg <- lm(wage ~ educ, data = wage1)
sqrt(sum(reg$residuals^2) / (nrow(wage1) - 2))
summary(reg)$sigma


#uestion 19
#Data: wage1 (wage = average hourly earnings in dollars; educ = years of education).
#What is SSTx for educ, the total sum of squares of the explanatory variable? 

SSTx <- sum((wage1$educ - mean(wage1$educ))^2)
SSTx
