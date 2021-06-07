library(MASS)
## Read in Supervisor Data
super = read.table("../Data/Supervisor.txt",header = TRUE)
n = nrow(super)
super.lm = lm(Rating ~ Complaints + Privileges + Learn + Raises +
                Critical + Advance,data=super)
sigma_hat = summary(super.lm)$sigma

summary(super.lm)$cov.unscaled

########################################
## By hand  ############################
########################################

y = super[,1]
X = cbind(1,as.matrix(super[,-1]))
head(X)

### beta hat

beta_hat = solve(t(X) %*% X) %*% t(X) %*% y
c(beta_hat)
coef(super.lm)

### standard error

SE_beta =sqrt(diag(sigma_hat^2  * solve(t(X) %*% X)))
SE_beta
summary(super.lm)$coef[,2]      

### ### ### ### 
### Projection
### ### ### ### 

P_x = X %*% solve(t(X) %*% X) %*% t(X)

### mu hat

c(P_x %*% y)     ## projection
c(predict.lm(super.lm)) ## using beta hat


## SE of mu hat

sqrt(diag(sigma_hat^2 * P_x))
c(predict.lm(super.lm,se.fit = TRUE)$se.fit)

### residuals

c((diag(n) -  P_x) %*% y)     ## projection
c(resid(super.lm)) ## using beta hat

### Standardized residuals


c((diag(n) -  P_x) %*% y) / sqrt(diag(sigma_hat^2 *(diag(n) -  P_x)))
stdres(super.lm)

