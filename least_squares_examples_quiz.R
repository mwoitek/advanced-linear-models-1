library(dplyr)

data(mtcars)
cars_data <- as_tibble(mtcars) %>%
  select(mpg, wt, vs)

# Selecting the relevant data from group 0 (vs = 0)
group_0 <- cars_data %>%
  filter(vs == 0) %>%
  select(mpg, wt) %>%
  rename(w = wt, y = mpg)

# Selecting the relevant data from group 1 (vs = 1)
group_1 <- cars_data %>%
  filter(vs == 1) %>%
  select(mpg, wt) %>%
  rename(w = wt, y = mpg)

# Number of observations for each group
n_0 <- nrow(group_0)
n_1 <- nrow(group_1)

# Empirical variance of wt for each group
var_0 <- var(group_0$w)
var_1 <- var(group_1$w)

# Empirical covariance of wt and mpg for each group
cov_0 <- cov(group_0$w, group_0$y)
cov_1 <- cov(group_1$w, group_1$y)

# Estimates for the model coefficients
beta_3 <- ((n_0 - 1) * cov_0 + (n_1 - 1) * cov_1) / ((n_0 - 1) * var_0 + (n_1 - 1) * var_1)
beta_1 <- mean(group_0$y) - beta_3 * mean(group_0$w)
beta_2 <- mean(group_1$y) - beta_3 * mean(group_1$w)

beta_hat <- c(beta_1, beta_2, beta_3)
round(beta_hat, digits = 3)
# [1] 33.004 36.159 -4.443

# Problem 5

# Difference between the intercepts (group 1 - group 0)
diff_intercept <- beta_2 - beta_1
round(diff_intercept, digits = 3)

# Answer:
# [1] 3.154

# Problem 6

round(abs(beta_3), digits = 2)
# [1] 4.44

# Answer: 4.44 miles per gallon
