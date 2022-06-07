# a)
set.seed(1)
X <- rnorm(100)
noise <- rnorm(100)
Beta <- c(0.2137,0.420,-0.911,0.69)
# b)
Y <- Beta[1] + Beta[2] *X + Beta[3] *X^2 + Beta[4] *X^3 + noise

# c2)
library(leaps)
df <- data.frame(Y, X)
fit <- regsubsets(Y ~ poly(X, 10), data = df, nvmax = 10)
fit_summary <- summary(fit)

par(mfrow = c(1, 3))

cp <- fit_summary$cp
plot(cp, xlab="Variables number", ylab = "cp", type = "l")
points(which.min(cp), cp[which.min(cp)], col = "red", cex = 1.5, pch = 1)

bic <- fit_summary$bic
plot(bic, xlab="Variables number", ylab = "bic", type = "l")
points(which.min(bic), bic[which.min(bic)], col = "red", cex = 1.5, pch = 1)

adjr2 <- fit_summary$adjr2
plot(adjr2, xlab="Variables number", ylab = "adjr2", type = "l")
points(which.max(adjr2), adjr2[which.max(adjr2)], col = "red", cex = 1.5, pch = 1)

coef(fit, which.min(cp))
coef(fit, which.min(bic))
coef(fit, which.max(adjr2))


# d)
#backward
fit_backward <- regsubsets(Y ~ poly(X, 10), data = df, nvmax = 10, method = "backward")
summary_backward <- summary(fit_backward)

cp_backward <- summary_backward$cp
bic_backward <- summary_backward$bic
adjr2_backward <- summary_backward$adjr2

par(mfrow = c(1, 3))

plot(cp_backward, xlab = "Variables number", ylab = "Cp", type = "l")
points(which.min(cp_backward), cp_backward[which.min(cp_backward)], col = "red", cex = 2, pch = 20)

plot(bic_backward, xlab = "Vriables number", ylab = "BIC", type = "l")
points(which.min(bic_backward), bic_backward[which.min(bic_backward)], col = "red", cex = 2, pch = 20)

plot(adjr2_backward, xlab = "Variables number", ylab = "Adjr2", type = "l")
points(which.max(adjr2_backward), adjr2_backward[which.max(adjr2_backward)], col = "red", cex = 2, pch = 20)

coef(fit_backward, which.min(cp_backward))
coef(fit_backward, which.min(bic_backward))
coef(fit_backward, which.max(adjr2_backward))

#forward

fit_forward <- regsubsets(Y ~ poly(X, 10), data = df, nvmax = 10, method = "forward")
summary_forward <- summary(fit_forward)

par(mfrow = c(1, 3))

cp_forward <- summary_forward$cp
bic_forward <- summary_forward$bic
adjr2_forward <- summary_forward$adjr2

plot(cp_forward, xlab = "Variables number", ylab = "Cp", type = "l")
points(which.min(cp_forward), cp_forward[which.min(cp_forward)], col = "red", cex = 2, pch = 20)

plot(bic_forward, xlab = "Vriables number", ylab = "BIC", type = "l")
points(which.min(bic_forward), bic_forward[which.min(bic_forward)], col = "red", cex = 2, pch = 20)

plot(adjr2_forward, xlab = "Variables number", ylab = "Adjr2", type = "l")
points(which.max(adjr2_forward), adjr2_forward[which.max(adjr2_forward)], col = "red", cex = 2, pch = 20)

coef(fit_forward, which.min(cp_forward))
coef(fit_forward, which.min(bic_forward))
coef(fit_forward, which.max(adjr2_forward))


# e)
library(glmnet)
set.seed(1)
par(mfrow = c(1,1))

xmat <- model.matrix(Y ~ poly(X, 10), data = df)[, -1]
crossval_lasso <- cv.glmnet(xmat, Y, alpha = 1)

plot(crossval_lasso)

best_lambda <- crossval_lasso$lambda.min

lasso <- glmnet(xmat, Y, alpha = 1)
predict(lasso, s = best_lambda, type = "coefficients")[1:11, ]

# f)
Beta[7] <- 5
Y_7 <- Beta[1] + Beta[7]*X^7 + noise
df_2 <- data.frame(Y_7 = Y_7, X = X)

fit <- regsubsets(Y_7 ~ poly(X, 10), data = df_2, nvmax = 10)

fit_summary <- summary(fit)

par(mfrow = c(1, 3))

cp <- fit_summary$cp
plot(cp, xlab="Variables number", ylab = "cp", type = "l")
points(which.min(cp), cp[which.min(cp)], col = "red", cex = 1.5, pch = 1)

bic <- fit_summary$bic
plot(bic, xlab="Variables number", ylab = "bic", type = "l")
points(which.min(bic), bic[which.min(bic)], col = "red", cex = 1.5, pch = 1)

adjr2 <- fit_summary$adjr2
plot(adjr2, xlab="Variables number", ylab = "adjr2", type = "l")
points(which.max(adjr2), adjr2[which.max(adjr2)], col = "red", cex = 1.5, pch = 1)

coef(fit, which.min(cp))
coef(fit, which.min(bic))
coef(fit, which.max(adjr2))

fit_summary <- summary(fit)


#lasso 7
library(glmnet)
set.seed(1)
par(mfrow = c(1,1))

xmat <- model.matrix(Y_7 ~ poly(X, 10), data = df_2)[, -1]
crossval_lasso <- cv.glmnet(xmat, Y_7, alpha = 1)

plot(crossval_lasso)

best_lambda <- crossval_lasso$lambda.min

lasso <- glmnet(xmat, Y_7, alpha = 1)
predict(lasso, s = best_lambda, type = "coefficients")[1:11, ]

