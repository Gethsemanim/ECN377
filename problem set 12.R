#problem set 12

#


#2

# In wage = β0 + β1*educ + β2*exper + β3*tenure + u
# and want SE(B2^) the standard error of the exper coefficient.
# Which regression gives R2j^

# HINT: Rj2 measures how well the other independent variables explain Xj. Y is not involved.

The regression of exper on educ and tenure: lm(exper ~ educ + tenure)


#4

#A multiple regression with K=3 independent variables, n=65 , SSR=2913
#Find the standard error of the regression, σ^???
  
  # HINT: df = n- K - 1
  #    THEN : σ^ = sqrt(SSR / df)

n <- 65
K <- 3
SSR <- 2913

df <- n - K - 1
sigma_hat <- sqrt(SSR / df)

sigma_hat


#6

#A multiple regression has  σ^= 7.2 
#For the independent variable Xj, SSTj= 572 and regressing Xj on 
#the other independent variables gives R2j= 0.85
#FIND SE(Bj^) =  σ^ / sqrt ( SSTj * (1 - R2j) )

# Given values
sigma_hat <- 7.2
SSTj <- 572
R2j <- 0.85

# Standard error of b_j
SE_bj <- sigma_hat / sqrt(SSTj * (1 - R2j))
SE_bj
# Rounded answer
round(SE_bj, 2)



#9

#Data: hprice1 (price = house price in thousands of dollars; 
#               sqrft = size of the house in square feet;
#               bdrms = number of bedrooms; 
#               lotsize = size of the lot in square feet).

#Use REG: reg <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1),
#compute the standard error of the regression SE????   σ^ = sqrt(SSR / df)


#Hint: SSR <- sum(reg$residuals^2),
# THEN sigma_hat <- sqrt(SSR / 84).

reg <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)

SSR <- sum(reg$residuals^2)

sigma_hat <- sqrt(SSR / 84)

round(sigma_hat, 2)


#12

#Data: hprice1 (price = house price in thousands of dollars; 
#               sqrft = size of the house in square feet;
#               bdrms = number of bedrooms; 
#               lotsize = size of the lot in square feet).

#Put the pieces together: compute SE(Bj^) =  σ^ / sqrt(SSTj * (1 - R2j))
#for the bdrms coefficient ?????

#Hint: use the unrounded objects from problems 9-11: 
#  sigma_hat / sqrt(SST_j * (1 - R2_j))

reg <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)

SSR <- sum(reg$residuals^2)
sigma_hat <- sqrt(SSR / 84)

SSTj <- sum((hprice1$bdrms - mean(hprice1$bdrms))^2)

reg_bdrms <- lm(bdrms ~ sqrft + lotsize, data = hprice1)

R2_j <- summary(reg_bdrms)$r.squared

SE_bj <- sigma_hat / sqrt(SSTj * (1 - R2_j))

round(SE_bj, 2)





