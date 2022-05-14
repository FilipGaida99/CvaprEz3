set.seed(1)
X <- rnorm(100)
noise <- rnorm(100)
Beta <- c(0.2137,0.420,0.911,0.69)
Y <- Beta[1] + Beta[2] *X + Beta[3] *X^2 - Beta[4] *X^3 + noise

require(leaps)
df <- data.frame(Y, X)
fit <- regsubsets(Y ~ poly(X, 10), data = df, nvmax = 10)

fit_summary <- summary(fit)

require(tidyverse);require(ggplot2);require(ggthemes);

data_frame(Cp = fit_summary$cp,
           BIC = fit_summary$bic,
           AdjR2 = fit_summary$adjr2) %>%
  mutate(id = row_number()) %>%
  gather(value_type, value, -id) %>%
  ggplot(aes(id, value, col = value_type)) +
  geom_line() + geom_point() + ylab('') + xlab('Number of Variables Used') +
  facet_wrap(~ value_type, scales = 'free') +
  theme_tufte() + scale_x_continuous(breaks = 1:10)

