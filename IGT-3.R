# a)
set.seed(2137)
X <- rnorm(100)
noise <- rnorm(100)
Beta <- c(0.2137,0.420,0.911,0.69)
# b)
Y <- Beta[1] + Beta[2] *X + Beta[3] *X^2 - Beta[4] *X^3 + noise

# c)
require(leaps)
df <- data.frame(Y, X)
fit <- regsubsets(Y ~ poly(X, 10), data = df, nvmax = 10)

fit_summary <- summary(fit)

require(tidyverse);require(ggplot2);require(ggthemes);

data_frame(Cp = fit_summary$cp,
           BIC = fit_summary$bic,
           R2 = fit_summary$adjr2) %>%
  mutate(id = row_number()) %>%
  gather(value_type, value, -id) %>%
  ggplot(aes(id, value, col = value_type)) +
  geom_line() + geom_point() + ylab('') + xlab('Number of Variables Used') +
  facet_wrap(~ value_type, scales = 'free') +
  theme_tufte() + scale_x_continuous(breaks = 1:10)

# d)
#backward
require(caret)

model_back <- train(Y ~ poly(X, 10), data = df, 
                    method = 'glmStepAIC', direction = 'backward', 
                    trace = 0,
                    trControl = trainControl(method = 'none', verboseIter = FALSE))

postResample(predict(model_back, df), df$Y)

summary(model_back$finalModel)

#forward

x_poly <- poly(df$X, 10)

colnames(x_poly) <- paste0('poly', 1:10)
model_forw <- train(y = Y, x = x_poly,
                    method = 'glmStepAIC', direction = 'forward',
                    trace = 0,
                    trControl = trainControl(method = 'none', verboseIter = FALSE))

postResample(predict(model_forw, data.frame(x_poly)), df$Y)

summary(model_forw$finalModel)

# e)
lasso_model <- train(Y ~ poly(X, 10), data = df,
                     method = 'glmnet',
                     trControl = trainControl(method = 'cv', number = 10),
                     tuneGrid = expand.grid(alpha = 1,
                                            lambda = seq(0.001, 0.2, by = 0.005)))

plot(lasso_model)

plot(varImp(lasso_model))

coef(lasso_model$finalModel, lasso_model$bestTune$lambda)

postResample(predict(lasso_model, df), df$Y)

# f)
Beta[7] <- 5
Y_7 <- Beta[1] + Beta[7]*X^7 + noise
df_2 <- data_frame(Y_7 = Y_7, X = df[,-1])

fit <- regsubsets(Y_7 ~ poly(X, 10), data = df_2, nvmax = 10)

fit_summary <- summary(fit)

data_frame(Cp = fit_summary$cp,
           BIC = fit_summary$bic,
           R2 = fit_summary$adjr2) %>%
  mutate(id = row_number()) %>%
  gather(value_type, value, -id) %>%
  ggplot(aes(id, value, col = value_type)) +
  geom_line() + geom_point() + ylab('') + xlab('Number of Variables Used') +
  facet_wrap(~ value_type, scales = 'free') +
  theme_tufte() + scale_x_continuous(breaks = 1:10)

lasso_y7_model <- train(Y_7 ~ poly(X, 10), data = df_2,
                        method = 'glmnet', 
                        trControl = trainControl(method = 'cv', number = 5),
                        tuneGrid = expand.grid(alpha = 1, 
                                               lambda = seq(0.001, 0.2, by = 0.005)))

plot(lasso_y7_model)

plot(varImp(lasso_y7_model))

coef(lasso_y7_model$finalModel, lasso_y7_model$bestTune$lambda)

postResample(predict(lasso_y7_model, df_2), df_2$Y_7)
