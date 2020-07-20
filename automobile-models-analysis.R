# download the data
install.packages("corrgram")
library(corrgram)
data(auto)
auto1 <- na.omit(auto)
auto1$Model <- NULL
auto1$Origin <- NULL

# simple linear regression
lmod <- lm(Price~., auto1)
summary(lmod)

# polynomial regression
lmod1 <- lm(Price~poly(MPG,2), auto1)
summary(lmod1)

# stepwise regression
# forward stepwise
templm1 <- lm(Price~1, auto1)
tempScope1 <- formula(lm(Price~., auto1))
step1 <- step(templm1, scope=tempScope1, direction="forward", trace=F)
summary(step1)
step1$anova
# backward stepwise
templm2 <- lm(Price~., auto1)
tempScope2 <- formula(lm(Price~., auto1))
step2 <- step(templm2, scope=tempScope2, direction="backward", trace=F)
summary(step2)
step2$anova

# cross validation
library(boot)
fullmodel <- glm(Price~., data=auto1)
MSE_11_cv1 <- cv.glm(auto1, fullmodel, K=11)$delta[1]
bdmodel <- glm(Price ~ Rep78 + Hroom + Rseat + Weight + Length + Turn, 
               data=auto1)
MSE_11_cv2 <- cv.glm(auto1, bdmodel, K=11)$delta[1]
MSE_11_cv1
# [1] 5745764
MSE_11_cv2
# [1] 4349219

# ridge
library(MASS)
rgmod <- lm.ridge(Price~., auto1, lambda=seq(0,0.1,by=0.001))
which.min(rgmod$GCV)
# 0.100 101
rgmod$GCV[101]
# 0.100 68909.64
coef(rgmod)[101,]
select(rgmod)
# smallest value of GCV  at 0.1
matplot(rgmod$lambda, coef(rgmod), type="l", 
        xlab=expression(lambda), ylab=expression(hat(beta)))
abline(v=0.1)

# lasso
install.packages("lars")
require(lars)
lmodlasso <- lars(as.matrix(auto1[,-1]), auto1$Price)
plot(lmodlasso)
set.seed(111)
cvlmodlasso <- cv.lars(as.matrix(auto1[,-1]), auto1$Price)
cvlmodlasso$index[which.min(cvlmodlasso$cv)]
predict(lmodlasso, s=0.7171717, type="coef", mode="fraction")$coef
cvlmodlasso$cv.error[100]
# [1] 1202496

# 'huge' package
library(huge)
x.npn = huge.npn(auto1[8:9])
out.npn = huge(x.npn, nlambda=1000, lambda.min.ratio = 0.01)
print(out.npn)
plot(x.npn)
